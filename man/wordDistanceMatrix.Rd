\name{wordDistanceMatrix}
\alias{wordDistanceMatrix}

\title{wordDistanceMatrix
}
\description{This function returns a matrix of distance between every occurrence of pattern1 and pattern2.
}
\usage{
wordDistanceMatrix(pattern1, pattern2, text)
}

\arguments{
  \item{pattern1}{The first pattern of interest in the text body.
}
\item{pattern2}{The second pattern of interest in the text body.
}
\item{text}{The text body that contains pattern1 and pattern2.
}
}

\examples{
wordDistanceMatrix(pattern1 = "pleasure", pattern2 = "pain", text = sampletext)}
