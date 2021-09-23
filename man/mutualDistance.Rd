\name{mutualDistance}
\alias{mutualDistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mutualDistance
}
\description{This function measures the distance between two patterns in a text string. The resultant list contains five items: 1) word count of the text body that contains the patterns; 2) pattern 1 locations; 3) pattern 2 locations; 4) pattern 1 to pattern 2 distance matrix; 5) pattern 2 to pattern 1 distance matrix.
}
\usage{
mutualDistance(pattern1, pattern2, text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern1}{The first pattern of interest in the text body.
}
\item{pattern2}{The second pattern of interest in the text body.
}
\item{text}{The text body that contains pattern1 and pattern2.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Tiangeng Lu
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mutualDistance(pattern1 = "Twitter", pattern2 = "network",
text = "Among many methods to study a policy debate,
we chose Twitter data. Twitter data can generate different
types of networks that are useful for different purposes.
For example, Twitter hashtag networks
illustrate the topic universe.")

