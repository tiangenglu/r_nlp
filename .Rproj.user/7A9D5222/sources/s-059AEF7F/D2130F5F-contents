wordDistanceMatrix <- function(pattern1, pattern2, text) {
  loc_pat1 <- which(grepl(pattern1,unlist(strsplit(text," ")), ignore.case = T)==T)
  loc_pat2 <- which(grepl(pattern2,unlist(strsplit(text," ")), ignore.case = T)==T)
  if((length(loc_pat1)==0)==TRUE | (length(loc_pat2)==0)==TRUE) {warning("At least one of the patterns were not found in the string. Returns an 1*1 matrix of NA.")
    alternativeoutput <- matrix(data = NA, nrow = 1, ncol = 1)
    return(alternativeoutput)}
  else {
    distances1to2 <- list()
    {for (i in 1: length(loc_pat1)){distances1to2[[i]] <- ((abs(loc_pat1[i] - loc_pat2)))}}

    distance1to2_m <- matrix(unlist(distances1to2), nrow = length(distances1to2), ncol = length(loc_pat2), byrow = T) # convert distance1to2 to matrix
    rownames(distance1to2_m) <- paste(pattern1, seq(1:length(loc_pat1)), sep = "_")
    colnames(distance1to2_m) <- paste(pattern2, seq(1:length(loc_pat2)), sep = "_")
    return(distance1to2_m)}
}

