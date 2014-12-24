\name{splatnd}
\alias{splatnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Execute simple zero-argument functions
%%  ~~function to do ... ~~
}
\description{
 Execute simple zero-argument functions without having to type the "()" , and without having to go through the bother of \code{makeActiveBinding}. 
This code is provided primarily to allow the user to build his own set of command "shortcuts" by modifying the set of arguments to the \code{switch} function in the function body.  The bulk of the code is copied from the excellent package \code{sos} .
The name \code{splatnd} cannot be called directly, and doesn't even exist after being sourced.  It serves to define a variety of operators \code{![your_string_here]} .  If the string after \code{!} is not in the switch-list, the function defaults to the normal splat operator, i.e. NOT[your_string_here] .
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  none
%%     ~~Describe \code{x} here~~
}
\details{
There's an obvious risk of undesired results should there exist an object in the environment with the same name as one of the items in the \code{switch} options.  The workaround is to enclose the object name in parentheses.  See the example. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is the result of whatever function or operator was invoked.

}

\author{
%%  ~~who you are~~
Carl Witthoft,  \email{carl@witthoft.com}
}
\note{
%%  ~~further notes~~
The bulk of the code is taken directly from the \code{sos} package.   
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The R manuals on creating operators,  \code{findFn} in the package \code{sos} , normally invoked as \code{???}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# based on the default items in splatnd
qapla <- 1:5
!qapla
!(qapla)
}
