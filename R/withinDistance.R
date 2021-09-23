withinDistance <- function(pattern1, pattern2, text, threshold) {
  loc_pat1 <- which(grepl(pattern1,unlist(strsplit(text," ")), ignore.case = T)==T)
  loc_pat2 <- which(grepl(pattern2,unlist(strsplit(text," ")), ignore.case = T)==T)
  if((length(loc_pat1)==0)==TRUE | (length(loc_pat2)==0)==TRUE) {warning("At least one of the patterns were not found in the string. Returns NA.")
    alternativeoutput <- "NA"
    return(alternativeoutput)}
  else {
    distances1to2 <- list()
    {for (i in 1: length(loc_pat1)){distances1to2[[i]] <- ((abs(loc_pat1[i] - loc_pat2)))}}

    distance1to2_m <- matrix(unlist(distances1to2), nrow = length(distances1to2), ncol = length(loc_pat2), byrow = T) # convert distance1to2 to matrix
    min(distance1to2_m) <=threshold}
}
