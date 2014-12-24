\name{lsclass}
\alias{lsclass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Q&D function to list all objects with the specified \code{class attribute} .
%%  ~~function to do ... ~~
}
\description{
This is one of the author's collection of \code{ls*} Q&D functions.  Since anyone can define a new class at any time, there is no predefined set of legal or illegal class names.  Remember that an \code{object} can have multiple \code{class}es.  This function only allows searching for a single class name in a given call.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsclass(type = "numeric")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
  The name of the class you're looking for.
%%     ~~Describe \code{type} here~~
}
}

\value{
A vector of character strings containing the names of matching objects (as would be returned by the base function \code{ls} ) .

}

\author{
Carl Witthoft  carl@witthoft.com
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{typeof}}, \code{\link{class}} , \code{\link{lstype}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xyzzy<-structure(vector(),class='grue')
lsclass('integer')
lsclass('grue') 
}