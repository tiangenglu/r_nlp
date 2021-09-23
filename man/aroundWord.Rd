\name{aroundWord}
\alias{aroundWord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
aroundWord
}
\description{
Returns a data frame of word frequency table. It answers the question: Which are the frequent words within a specific distance from a given pattern (word)?
}
\usage{
aroundWord(pattern, text, max.distance, min.freq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{pattern}{The  pattern of interest in the text body.
}
\item{text}{The text body that contains the pattern.
}
\item{max.distance}{The maximum distance as search range before and after the pattern
}
\item{min.freq}{The minimum word frequency to report in the frequency data frame.
}
}

\examples{

aroundWord(pattern = "pleasure",text = sampletext, max.distance = 5, min.freq = 2)
}
