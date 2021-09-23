# mutualDistance
#
# This is an example function named 'nutualDistance'
# which prints The resultant list contains five items: 1) word count; 2) pattern 1 locations; 3) pattern 2 locations; 4) pattern 1 to pattern 2 distance matrix; 5) pattern 2 to pattern 1 distance matrix..
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

mutualDistance <- function(pattern1, pattern2, text) {
  loc_pat1 <- which(grepl(pattern1,unlist(strsplit(text," ")), ignore.case = T)==T)
  loc_pat2 <- which(grepl(pattern2,unlist(strsplit(text," ")), ignore.case = T)==T)
  word_count <- sapply(strsplit(text," "), length)
  if((length(loc_pat1)==0)==TRUE | (length(loc_pat2)==0)==TRUE)stop("At least one of the patterns were not found in the string.")
  distances1to2 <- list()
  {for (i in 1: length(loc_pat1)){distances1to2[[i]] <- ((abs(loc_pat1[i] - loc_pat2)))}}
  distances2to1 <- list()
  {for (j in 1: length(loc_pat2)){distances2to1[[j]] <- ((abs(loc_pat2[j] - loc_pat1)))}}

  mutualdistoutput  <- list(distances1to2,distances2to1)

  distance1to2_m <- matrix(unlist(mutualdistoutput[[1]]), nrow = length(mutualdistoutput[[1]]), ncol = length(loc_pat2), byrow = T) # convert distance1to2 to matrix
  rownames(distance1to2_m) <- paste(pattern1, seq(1:length(loc_pat1)), sep = "_")
  colnames(distance1to2_m) <- paste(pattern2, seq(1:length(loc_pat2)), sep = "_")

  distance2to1_m <- matrix(unlist(mutualdistoutput[[2]]), nrow = length(mutualdistoutput[[2]]), ncol = length(loc_pat1), byrow = T) # convert distance1to2 to matrix
  rownames(distance2to1_m) <- paste(pattern2, seq(1: length(loc_pat2)), sep = "_")
  colnames(distance2to1_m) <- paste(pattern1, seq(1:length(loc_pat1)), sep = "_")
  outputs <- list(word_count,loc_pat1, loc_pat2, distance1to2_m, distance2to1_m)
  names(outputs) <- c("wordcount","pat1_loc","pat2_loc","dist1to2","dist2to1")
  return(outputs)
}
