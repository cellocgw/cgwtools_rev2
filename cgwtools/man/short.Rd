\name{short}
\alias{short}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a small sample of the specified data set. 
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The user specifies both the number of elements to display and the number of elements at the start and end of the vector to ignore ('skip') when selecting elements.  The results are displayed in a nice tabular form.  There are options to set the value of N as well as the number of values to "skip" before selecting the values.  \code{short}  is similar to a combination of the unix \code{head}  and \code{tail} functions.
}
\usage{
short(x = seq(1, 20), numel = 4, skipel = 0, ynam = deparse(substitute(x)), dorows=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The data vector to be examined.
%%     ~~Describe \code{x} here~~
}
  \item{numel}{
  How many elements to display.  Note that \code{numel} elements of the beginning and of the end of the vector are returned.
%%     ~~Describe \code{numel} here~~
}
  \item{skipel}{
  If desired, skip the first \code{skipel} elements before returning \code{numel} elements.
}
  \item{ynam}{  
  Not normally changed by the user.  \code{ynam} retrieves the name of the object in question, to be used in the output table formatting.
%%     ~~Describe \code{ynam} here~~
}
  \item{dorows}{
    For matrices only, return the "numel" number of rows rather than elements. \code{dorows} is ignored with a warning if the input \code{x} has higher dimensionality.
}
}
\details{If the argument \code{x} happens to be a list, \code{short} unlists everything, so the first \code{numel} values will be taken from the first list element, going on to the second element as needed, etc.}
\value{
Nothing is returned of interest.  The function is called to provide what is printed directly to the console, which is a formatted table of the lead and tail values selected, with column labels identifying their location in the input vector object.
}

\author{
Carl Witthoft   \email{carl@witthoft.com}
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{head}} , \code{\link{tail}}
}
\examples{
foo<-matrix(runif(100),nrow=20)
short(foo)
short(foo,numel=6,skipel=10)
short(foo,numel=6,skipel=10,dorows=TRUE)
}
