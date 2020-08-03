library(tidyverse)
library(lubridate)
library(rsconnect)

# http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
df <- rsconnect::showMetrics("container_status",
                             c("connect_count", 
                               "connect_procs"),
                             appName="ipeds_compiler",
                             server="shinyapps.io",
                             from="24w",
                             interval="1m"
) 

df1 <- df %>% 
  mutate(date=as_datetime(timestamp, tz="EST")) %>% 
  select(-timestamp) %>% 
  arrange(date) %>% 
  mutate(
    n_count=cumsum(connect_count),
    n_procs=cumsum(connect_procs),
    new_connect=case_when(
      connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
      TRUE ~ 0),
    n_connect=cumsum(new_connect) # approximate
  ) %>% 
  filter(n_count>0)

df2 <- df1 %>%  
  select(n_connect, date) %>% 
  gather(key="key", value="value", -date)

p2 <- ggplot(df2) +
  labs(title="Cumulative Connections", x="", y="") +
  geom_line(aes(x=date, y=value, colour=key)) +
  facet_wrap(~key)

print(p2)

today <- df2 %>% filter(date>=ymd(today(),tz = "EDT")) %>% 
  mutate(value_new = value-min(value))

ptoday <- ggplot(today) +
  labs(title=paste0("Number of Connections Today: ", today()), x="", y="") +
  geom_line(aes(x=date, y=value_new, colour=key)) +
  facet_wrap(~key)

print(ptoday)
