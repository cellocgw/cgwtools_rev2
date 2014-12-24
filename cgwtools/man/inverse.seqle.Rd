\name{inverse.seqle}
\alias{inverse.seqle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse of \code{\link{seqle}}
%%  ~~function to do ... ~~
}
\description{
As with \code{inverse.rle}, this function reverses the compression performed with \code{seqle} so long as you know the \code{incr} value used to generate the compressed data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inverse.seqle(x, incr = 1L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  An object of class \code{rle} 
%%     ~~Describe \code{x} here~~
}
  \item{incr}{
  The  increment between elements used to generate the compressed data object.  Note that this can be either integer or float.  For floating-point sequences, the reconstruction of the original series may differ at the level of floating-point precision used to generate the input object.
%%     ~~Describe \code{incr} here~~
}
  
}

\value{ a vector of values identical (or nearly so, for floats) to the original sequence.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
Carl Witthoft,  \email{carl@witthoft.com}
}
\note{
%%  ~~further notes~~
The bulk of the code is taken directly from \code{base::inverse.rle} . Thanks to "flodel",  http://www.linkedin.com/in/florentdelmotte ,  on StackOverflow for suggesting code to handle floating-point increments.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{seqle}} , \code{\link{inverse.rle}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x<- c(2,2,2,3:8,8,8,4,4,4,5,5.5,6)
y<-seqle(x,incr=0)
inverse.seqle(y,0)
y <- seqle(x,incr=1)
inverse.seqle(y)
inverse.seqle(y,2) # not what you wanted
}