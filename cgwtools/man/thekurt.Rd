\name{thekurt}
\alias{thekurt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the kurtosis of the input data set. 
%%  ~~function to do ... ~~
}
\description{
 Kurtosis is the next moment after skew (which is the moment after the variance). This function is provided primarily to support the function \code{mystat} . It uses the algorithm provided in the R package \code{e1071}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
thekurt(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector or vectorizable data set.
%%     ~~Describe \code{x} here~~
}
}

\value{ A single scalar, the kurtosis of the data provided.
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
