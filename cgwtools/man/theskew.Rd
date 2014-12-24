\name{theskew}
\alias{theskew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the skew of a dataset. 

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is included primarily to support \code{mystat} .  Skew is the next moment after the variance.  The algorithm used here is taken from the R package \code{ e1071} .
}
\usage{
theskew(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of data to be evaluated
  
%%     ~~Describe \code{x} here~~
}
}

\value{ A single scalar, the skew of the dataset}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\author{
Carl Witthoft,  \email{carl@witthoft.com}

%%  ~~who you are~~
}

