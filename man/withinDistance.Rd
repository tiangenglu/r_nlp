\name{withinDistance}
\alias{withinDistance}

\title{withinDistance
}
\description{
At least one of the occurrence of the two patterns is within the threshold distance.
}
\usage{
withinDistance(pattern1, pattern2, text, threshold)
}

\arguments{
  \item{pattern1}{The first pattern of interest in the text body.
}
\item{pattern2}{The second pattern of interest in the text body.
}
\item{text}{The text body that contains pattern1 and pattern2.
}
\item{threshold}{A numeric value of the maximum distance.
}
}

\seealso{
withinDistance_all; withinDistance_percent
}
\examples{
withinDistance(pattern1 = "pleasure", pattern2 = "pain", text = sampletext, threshold = 5)
}
