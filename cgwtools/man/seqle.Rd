\name{seqle}
\alias{seqle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extends \code{rle} to find and encode linear sequences.
%%  ~~function to do ... ~~
}
\description{
The function \code{rle}, or "run-length encoder," is a simple compression scheme which identifies sequences of repeating values in a vector.  \code{seqle} extends this scheme by allowing the user to specify a sequence of values with a common "slope," or delta value, between adjacent elements. \code{seqle} with an increment of zero is the same as \code{rle}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seqle(x, incr = 1L, prec = .Machine$double.eps^0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The input vector of values.
%%     ~~Describe \code{x} here~~
}
  \item{incr}{
  The desired increment between elements which specifies the sequences to search for.  Note that this can be either integer or float.  For floating-point sequences, see the \code{prec} argument for determining what level of precision is used to determine whether elements continue a sequence or not.
%%     ~~Describe \code{incr} here~~
}
  \item{prec}{Defines the required precision to which elements are compared when determining whether they are part of a sequence.  Note that for integer inputs, this value is more or less meaningless.
%%     ~~Describe \code{prec} here~~
}
}
\details{Note: the returned value is assigned the class \code{"rle"}. So far as I can tell, this class has only a \code{print} method, i.e. defining what is returned to the console when the user types the name of the returned object.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{lengths}{a vector of the lengths (1 or greater) of all sequences found.}
\item{values}{a vector of the initial value for each sequence. For example, if \code{incr ==1} a \code{values}  of 5 associated with a \code{lengths} of 3 represents the sequence \code{5,6,7}.} 
}

\author{
%%  ~~who you are~~
Carl Witthoft,  \email{carl@witthoft.com}
}
\note{
%%  ~~further notes~~
The bulk of the code is taken directly from \code{base::rle} . Thanks to "flodel",  http://www.linkedin.com/in/florentdelmotte ,  on StackOverflow for suggesting code to handle floating-point increments.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rle}} \code{\link{inverse.seqle}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x<- c(2,2,2,3:8,8,8,4,4,4,5,5.5,6)
seqle(x,incr=0)
seqle(x,incr=1)
seqle(x,incr=1.5)
}