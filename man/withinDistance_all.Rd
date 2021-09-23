\name{withinDistance_all}
\alias{withinDistance_all}

\title{withinDistance_all
}
\description{Returns a vector of TRUE or FALSE statement for every occurrence of pattern1.
It answers the following question:
For every occurrence of pattern 1, is there at least one pattern 2 nearby within x (threshold = ) words?
}
\usage{withinDistance_all(pattern1, pattern2, text, threshold)
}
%- maybe also 'usage' for other objects documented here.
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
 withinDistance_all(pattern1 = "pleasure", pattern2 = "pain",text = sampletext, threshold = 5)
}

