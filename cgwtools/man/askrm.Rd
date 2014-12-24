\name{askrm}
\alias{askrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive application of selected function  a  list of objects.
}
\description{
This function was originally written to do the same as the unix \code{rm -i} command.  The user supplies a list of items and the name of a function which is optionally  applied to each item in turn. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
askrm(items = ls(parent.frame()), fn = "rm", ask = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{A character vector of names of the objects to be acted upon (such as \code{ls()} returns).  The default is all objects in the parent working environment. 

}
  \item{fn}{The name of the function to be applied, supplied as a character string.  Possible future upgrades may allow function names to be entered without quotes.
%%     ~~Describe \code{fn} here~~
}
  \item{ask}{If \code{TRUE} , the user is prompted for "y/n" before performing the function on each object in the list.  Be cautious about setting to \code{FALSE} for obvious reasons. Note that the only accepted positive response is \bold{exactly} "y" so, e.g. "yes" will be treated as "no." }
}

\value{{A list with three elements.}
\item{func}{Echo back the input function, for archival reference.}
\item{selected}{All the items from the input list to which the function \code{fn} was applied.  In the default case, these are the items deleted from the environment.}
\item{evout}{A list of the value(s) returned by the function, if any, each time it was executed.}
}
\author{
Carl Witthoft,  \email{carl@witthoft.com}
%%  ~~who you are~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
When interactive prompting is not desired,  \code{\link{sapply}} or its brethren are recommended.
}
\examples{
# get rid of junky objects left around from testing
foo<-1
afoo<-c(foo,2)
foob <- c('a','b','d')
askrm(ls(pattern="foo") )

x<- rep(1,10)
y<- runif(10)
askrm(c('x','y'),'sd',ask=FALSE)
}