\name{withinDistance_percent}
\alias{withinDistance_percent}

\title{
withinDistance_percent
}
\description{
The percentage of pattern 1 that has at least one pattern 2 nearby
}
\usage{
withinDistance_percent(pattern1, pattern2, text, threshold)
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
\examples{

withinDistance_percent(pattern1 = "pleasure", pattern2 = "pain",text = sampletext, threshold = 5)}
