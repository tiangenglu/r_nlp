aroundWord <- function(pattern, text, max.distance, min.freq) {
  splittext <- unlist(strsplit(tolower(removePunctuation(text))," "))
  loc_pat <- which(grepl(pattern, splittext, ignore.case = T)==T)
  if((length(loc_pat)==0)==TRUE) {warning("The pattern was not found in the text string. Returns NA")
    alternativeoutput <- "NA"
    return(alternativeoutput)}
  else {
    look_around <- list()
    for(i in 1: length(loc_pat)){look_around[[i]] <- splittext[ifelse((loc_pat[i] - max.distance)>0,(loc_pat[i] - max.distance),1):ifelse((loc_pat[i] + max.distance)>length(splittext),length(splittext),(loc_pat[i] + max.distance))]}
    lookaround <- unlist(look_around)
    lookaround <- lookaround[!lookaround%in%stopwords]
    lookaround <- lookaround[!lookaround%in%pattern]
    lookaround <- as.factor(lookaround)
    lookaround_table <-  data.frame(table(lookaround))
    lookaround_table <- lookaround_table[rev(order(lookaround_table$Freq)),]
    lookaround_table <- lookaround_table[lookaround_table$Freq >= min.freq,]
    colnames(lookaround_table)[1] <- "Word"
    lookaround_table <- lookaround_table[lookaround_table$Word!= pattern, ]
    return(lookaround_table)}
}
