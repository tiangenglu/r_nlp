\name{mutualDistance}
\alias{mutualDistance}

\title{mutualDistance
}
\description{This function measures the distance between two patterns in a text string. The resultant list contains five items: 1) word count of the text body that contains the patterns; 2) pattern 1 locations; 3) pattern 2 locations; 4) pattern 1 to pattern 2 distance matrix; 5) pattern 2 to pattern 1 distance matrix.
}
\usage{mutualDistance(pattern1, pattern2, text)
}

\arguments{
  \item{pattern1}{The first pattern of interest in the text body.
}
\item{pattern2}{The second pattern of interest in the text body.
}
\item{text}{The text body that contains pattern1 and pattern2.
}
}

\examples{mutualDistance(pattern1 = "pleasure", pattern2 = "pain", text = sampletext)}

