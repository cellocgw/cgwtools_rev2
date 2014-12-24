\name{pushd}
\alias{pushd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs equivalent of \code{bash} command with same name 
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 \code{pushd}  is based on the \code{cygwin} \code{bash} manpages' description of these commands. 
}
\usage{
pushd(path, dn=FALSE,rot=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
  The directory to move into.
%%     ~~Describe \code{x} here~~
}
  \item{dn}{
  Equivalent of the \code{dir} argument in \code{bash} . When TRUE, adds the current directory to the stack.
%%     ~~Describe \code{} here~~
}
  \item{rot}{
  Equivalent of the \code{n} argument in \code{bash}.  Rotates the existing stack by the value of \code{rot} ; can be positive or negative.  Note that there may be some inconsistency in how this is handled in different implementations of \code{bash} . 
}
}
\details{
Recommend reading \code{man bash} for full details of the operations. This implementation should do nothing more than change the working directory (and store directory history in a file \code{.dirhist} ). 
}
\value{
A status value, which is always \code{0} for success.  A future upgrade may implement a \code{trycatch} for conditions such as an inaccessible directory, but for now error messages will have to suffice.
}

\author{
Carl Witthoft   \email{carl@witthoft.com}
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{popd}} , \code{\link{setwd}}
}
\examples{
## depends on your local directory structure and permissions
getwd()
pushd("~/..")
getwd()
popd()
getwd()
}
