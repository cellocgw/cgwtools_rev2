\name{lstype}
\alias{lstype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List all objects of the specified type.
%%  ~~function to do ... ~~
}
\description{
This is a Q&D tool to list all objects in the current environment of a specified type.  As discussed in the base R documentation, these types are  the vector types "logical", "integer", "double", "complex", "character", "raw" and "list", "NULL", "closure" (function), "special" and "builtin" (basic functions and operators), "environment", "S4" (some S4 objects).  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lstype(type = "closure")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
  Any valid variable type, or "function," which is redirected to "closure." 
%%     ~~Describe \code{type} here~~
}
}

\value{
A vector of character strings as is returned by the base function \code{ls} .
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Carl Witthoft  carl@witthoft.com

%%  ~~who you are~~
}

\seealso{
\code{\link{ls}} , \code{\link{lssize}}, \code{\link{lsclass}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
lstype('integer') #if you have any such in your environment.
}