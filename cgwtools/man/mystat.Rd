\name{mystat}
\alias{mystat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate and display basic statistics for an object. 
%%  ~~function to do ... ~~
}
\description{
This function calculates the \code{min, max, median, mean, standard deviation, skew} and \code{kurtosis} for the specified object and displays the results in a semi-tabular form.  An option is provided to set the number of digits displayed for the returned values.
Note: see the help pages in this package for \code{theskew} and \code{thekurt} for information on those implementations. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mystat(x, numdig = 3, na.rm = TRUE, printit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector or vectorizable object.
%%     ~~Describe \code{x} here~~
}
  \item{numdig}{
  How many digits to the right of the decimal point to display (when \code{printit} is TRUE.
%%     ~~Describe \code{numdig} here~~
}
  \item{na.rm}{
  Does the user desire NA values to be removed.  Rare is the need to set this to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
  \item{printit}{
  Set to TRUE to see the results, nicely formatted, in the console.
%%     ~~Describe \code{printit} here~~
}
}

\value{
A data frame with scalar elements matching their names:
\item{min}{minimum}
\item{max}{maximum}
\item{mean}{mean value}
\item{median}{median}
\item{sdev}{standard deviation}
\item{skew}{skew}
\item{kurtosis}{kurtosis}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Carl Witthoft,  \email{carl@witthoft.com}

%%  ~~who you are~~
}

\seealso{
\code{\link{theskew} } , \code{\link{thekurt}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- runif(100)
mystat(x)
mystat(x,numdig=6)
}