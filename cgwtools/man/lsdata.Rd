\name{lsdata}
\alias{lsdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List all objects in an \code{.Rdata} file.
%%  ~~function to do ... ~~
}
\description{
This function opens an \code{.Rdata} file, lists the contents, and cleans up after itself.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsdata(fnam = ".Rdata")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnam}{
  the name of the datafile to be examined.
%%     ~~Describe \code{fnam} here~~
}
}

\value{
The output of \code{ls} applied to the objects loaded from the specified data file.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Various people have published similar code on Stack Overflow.
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Carl Witthoft  carl@witthoft.com
}

\seealso{
\code{\link{load}}, \code{\link{resave}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xblue<-1
yblue<-2
save(xblue,yblue,file='blue.Rdata')
lsdata('blue.Rdata')
}
