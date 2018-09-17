#'compile_IPEDS_survey
#'This is the new wrapper function that can compile in one function.
#'Inputs to this function are a peer data frame (UNITID & Institution Name),
#'data location, survey name, output directory.
#'Created by Emma Morgan 4/4/2018


script_merge_IPEDS_data <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/merge_ipeds_data.R",
                                         ssl.verifypeer = FALSE)
eval(parse(text = script_merge_IPEDS_data))
rm("script_merge_IPEDS_data")

script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", 
                                      ssl.verifypeer = FALSE)
eval(parse(text = script_add_valuesets))
rm("script_add_valuesets")

script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", 
                                           ssl.verifypeer = FALSE)
eval(parse(text = script_varnames_to_titles))
rm("script_varnames_to_titles")


compile_IPEDS_survey <- function(IPEDS_data_location_general, surveyFolder, peer_df, ignore_size_warning = F) {
  
  if(! hasArg(peer_df)){
    if (! ignore_size_warning){
      stop("Large file may break things, consider including peer_df to reduce file size. Set ignore_size_warning=T to override this error.")
    } else {peer_df = NULL}
  }
  if (!hasArg("IPEDS_data_location_general") | !hasArg("surveyFolder")){
    IPEDS_survey_location <- choose.dir(caption = "Select location of data/dictionary for the IPEDS survey you are compiling.")
  } else {IPEDS_survey_location <- paste(IPEDS_data_location_general,surveyFolder, sep="/")}
  
  if (hasArg(peer_df) && !'UNITID' %in% names(peer_df)) {
    stop("Your peer data frame must include a column titled UNITID. Please revise your data frame and try again.")
  }
  
  #Compile data across years; peer subsetting is included in this step
  merged_IPEDS_data <- merge_IPEDS_data(IPEDS_data_location = IPEDS_survey_location, peer_UNITIDs = peer_df[['UNITID']])
  
  #Add value labels and variable titles
  if (! is.null(merged_IPEDS_data[['valuesets']])){
    data_add_valuesets <- add_values(longtable=merged_IPEDS_data[['data']], valueset = merged_IPEDS_data[['valuesets']])
  } else {data_add_valuesets <- merged_IPEDS_data[['data']]}
  data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=merged_IPEDS_data[['dictionary']])
  
  #Add Institution Names
  data_final <- dplyr::left_join(data_add_vartitles, peer_df,"UNITID")
  
}
