\name{popd}
\alias{popd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs equivalent of \code{bash} command with same name 
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 \code{popd}  is based on the \code{cygwin}  \code{bash} manpages' description of these commands. 
}
\usage{
popd(dn=FALSE, pull=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 
  \item{dn}{
 Determines whether a stack "pop" is to be performed. This is the equivalent of the first argument in \code{bash:popd} . If \code{dn} is \code{FALSE} and \code{pull} is zero,  then set the new directory to the value at the top of the stack.  If \code{dn} is \code{TRUE} then do not change directory, and look to \code{pull} for modifying the stack.  See details for why the conditions are set this way.  
}
  \item{pull}{
  Equivalent of the latter \code{n} arguments in \code{bash}.  Removes the stack entry corresponding to the  \code{pull}'s value ; can be positive or negative.  Note that there may be some inconsistency in how this is handled in different implementations of \code{bash}.
  }
}

\details{
Recommend reading \code{man bash} for full details of the operations. This implementation will not change the working directory if \code{dn} is \code{TRUE} The directory history is stored in a file \code{.dirhist}, typically first created with \code{\link{pushd}} . 
}
\value{
A status value: \code{0} for success or \code{1} if there is no stack file (\code{.dirhist}).  Future upgrades may include other codes for other failure mechanisms, but for now error messages will have to suffice.
}

\author{
Carl Witthoft   \email{carl@witthoft.com}
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{pushd}} , \code{\link{setwd}}
}
\examples{
## depends on your local directory structure and permissions
getwd()
pushd("~/..")
getwd()
popd()
getwd()
}
