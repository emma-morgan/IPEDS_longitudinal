#Peer file
#Updated 3/1/2018
#These functions will help to work with a peer file
#You can read in a peer file that includes UNITIDs, specify a vector of UNITIDs, or generate a peer group with characteristics from the header file

pkgs <- c("tidyverse")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}
rm("pkgs", "pkg")

#Option 1: Use a .csv to specify your peer list

IPEDS_peers_from_file <- function(peer_filepath) {
  if (missing(peer_filepath)) {
    print("Select your .csv peer file using the dialogue box")
    peer_filepath <- file.choose()

  }
  #Confirm that we have a .csv document
  if (tools::file_ext(peer_filepath) != "csv") {stop("Your chosen peer file is not a .csv. Please save your file as a .csv and try again.")}
  peerFile <- readr::read_csv(peer_filepath,col_types = readr::cols(.default = "c"))
  names(peerFile) <- toupper(names(peerFile))
  if (! "UNITID" %in% toupper(names(peerFile))) {stop("Your peer file must contain a column for UNITID. Please fix your file and try again.")}
  return(list('peerdf'= peerFile,peers_for_IPEDS = peerFile[['UNITID']]))
}

