#Peer file

#These functions will help to work with a peer file
#You can read in a peer file that includes UNITIDs, specify a vector of UNITIDs, or generate a peer group with characteristics from the header file

#Option 1: Use a .csv to specify your peer list

IPEDS_peers_from_file <- function(peer_filepath) {
  if (missing(peer_filepath)) {
    print("Select your .csv peer file using the dialogue box")
    peer_filepath <- file.choose()

  }
  #Identify whether we have an excel or .csv document
  if (tools::file_ext(peer_filepath) != "csv") {stop("Your chosen peer file is not a .csv. Please save your file as a .csv and try again.")}
  peerFile <- read.csv(peer_filepath,stringsAsFactors = FALSE)
  names(peerFile) <- toupper(names(peerFile))
  if (! "UNITID" %in% toupper(names(peerFile))) {stop("Your peer file must contain a column for UNITID. Please fix your file and try again.")}
  return(list('peerdf'= peerFile,peers_for_IPEDS = peerFile[['UNITID']]))
}

