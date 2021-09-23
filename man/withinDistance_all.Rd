\name{withinDistance_all}
\alias{withinDistance_all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
withinDistance_all
}
\description{
Returns a vector of TRUE or FALSE statement for every occurrence of pattern1.
It answers the following question:
For every occurrence of pattern 1, is there at least one pattern 2 nearby within x (threshold = ) words?
}
\usage{
withinDistance_all(pattern1, pattern2, text, threshold)
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
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
withinDistance_all(pattern1 = "Twitter", pattern2 = "network",text = "Among many methods to study a policy debate, we chose Twitter data. Twitter data can generate different types of networks that are useful for  different purposes. For example, Twitter hashtag networks illustrate the topic universe.", threshold = 5)

