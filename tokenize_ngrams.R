str_tokenize <- function(text, split){
  if(class(text) == "character") {
    my_token <- unlist(strsplit(text, split = split))
    names(my_token) <- seq_along(my_token)
    return(my_token)
  } else stop("The input text string class must be in character.")
} # added on May 6, 2023

word_count <- function(text, split = " ") {
  if(class(text) == "character") {
    sapply(strsplit(text, split = split), length)
  } else stop("The input text string class must be in character.")
}

## The following n-gram functions return a vector of n-grams from a text body. Input text must be tokenized.
bi_gram <- function(token) {
  if (length(token) >= 3) {
    bi_gram <- c()
    for (i in 1:(length(token) - 1)) {
      bi_gram[i] <- paste(token[i], token[i + 1])
    }
    return(bi_gram)
  } else
    stop("The input text must be TOKENIZED and has length greater than 2.")
}

tri_gram <- function(token) {
  if (length(token) >= 4) {
    tri_gram <- c()
    for (i in 1: (length(token) - 2 )) {
      tri_gram[i] <- paste(token[i], token[i+1], token[i+2])
    }
    return(tri_gram)
  } else stop("The input text must be TOKENIZED and has length greater than 3.")
}