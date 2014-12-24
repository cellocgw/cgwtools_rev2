\name{lssize}
\alias{lssize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the sizes of all selected objects.
%%  ~~function to do ... ~~
}
\description{
Just a toy to list the number of elements  or optionally the bytesize as produced with \code{object.size} of a specified selection of objects.  I find it handy when I want to rid an environment of large (or empty) objects.  In the default case, \code{byte=FALSE} , lists and S4 objects are "taken apart" down to the lowest level so all individual elements are counted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lssize(items, byte = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
  A vector of character strings identifying the objects of interest as would be returned by, e.g. \code{ls(pattern="foo")} or \code{lstype("double")} .
%%     ~~Describe \code{items} here~~
}
  \item{byte}{
  If TRUE, calculate the number of bytes taken up by an object.  If FALSE, calculate the total number of elements of an object. 
%%     ~~Describe \code{byte} here~~
}
}

\value{A vector of the object sizes, with the object names as names for the elements

}
\references{
Many thanks to Martin Morgan of bioconductor.org who provided the recursive function for deconstructing an S4 Object. See http://stackoverflow.com/questions/14803237/is-there-an-s4-equivalent-to-unlist for the original question and answer. 

%% ~put references to the literature/web site here ~
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{
\code{\link{lstype}} , \code{\link{object.size}}, \code{\link{length}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x1<-runif(100)
x2<-runif(1000)
x3<-runif(2000)
lssize(ls(pattern='x[1-3]'))
lssize(ls(pattern='x[1-3]'),byte=TRUE) 
#depending on what you have in your environment:
lssize(lstype('integer'))
}