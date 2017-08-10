#' Goal: Add value labels to longitudinal file when value labels exist.
#' Contains function input dataset longitudinal table file and valueset file output  dataset longitudinal table file with value labels 
#' KMA - 8/10/17

#' subset valueset to only contain variables from the lonitudinal table 

# there should exist code to do this for now used this:

valueset_EFA <- valuesets %>% 
  filter(Tablenumber==21,
         varName%in%c("EFALEVEL", "LINE", "SECTION", "LSTUDY"))

ds <- longtable %>%

#'because some institutions have multiple values in one value set for same year need to add a unique row id for spread to work XX SHOULD THIS HAPPEN HERE????
  mutate(row_id = 1:nrow(longtable)) %>% 

#' select only the variables in small valueset to gather
  
  gather("varName", "var_numeric", names(longtable)[names(longtable)%in%valueset_EFA$varName]) %>% 

#' left join long table to small valueset

  left_join(ds, select(valueset_EFA,
                       "varName", 
                       "Codevalue", 
                       "valueLabel")) %>% 

#' paste variable value numeric with variable value description into a new column

  mutate(value_code_label = 
           paste0(Codevalue, "_", valueLabel)) %>% 
#' remove Codevalue and valueLabel for spread to function correctly
  select(-Codevalue, -valueLabel) %>% 

#' spread on variable fill in with var number desc
  spread(key = varName, value = value_code_label)

#' separate the value from the description

for(name in names(ds)[names(ds)%in%valueset_EFA$varName]) ds <- separate_(ds, name,sep = "/_/", into = c(name, paste0(name, '_desc')))

#' return dataset
