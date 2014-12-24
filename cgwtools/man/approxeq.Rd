\name{approxeq}
\alias{approxeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do "fuzzy" equality and return a logical vector. 
}
\description{
   This function  compares two vectors (or arrays) of values and returns the near-equality status of corresponding elements.  As with \code{all.equal()} , the intent is primarily to get around machine limits of representation of floating-point numbers.  For integer comparison, just use the base \code{==} operator. 
}
\usage{
approxeq(x, y, tolerance = .Machine$double.eps^0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{
  The two input items, typically vectors or arrays of data.
%%     ~~Describe \code{x} here~~
}
\item{tolerance}{
Set the precision to which \code{abs(x[j] - y[j])} will be compared. The  default argument provided is the R-standard value for floats. 
}
  \item{\dots}{
  Not used at this time.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If \code{x} and \code{y} are of different lengths, the shorter one is recycled and a warning issued.
}
\value{
A vector of the same length as the longer of \code{x} or \code{y}, consisting of \code{TRUE} and \code{FALSE} elements, depending on whether the corresponding elements of \code{x} and \code{y} are within the approximate equality precision desired. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Carl Witthoft,  \email{carl@witthoft.com}
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{all.equal}}, \code{\link{Comparison}}, \code{\link{identical}} 

}
\examples{
x<-1:10
y<-x+runif(10)*1e-6
approxeq(x,y) #all FALSE
approxeq(x,y,tolerance=1e-5) #mostly TRUE, probably
}