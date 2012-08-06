\name{value}
\alias{value,Parameters-method}
\alias{value<-}
\alias{value<-,Parameters-method}
\alias{value}
\title{value}
\description{
Get or set the value of a specified parameter.
}
%%\usage{
%%\S4method{value}{Parameters, character, character}(obj, category, name)
%%}
%%\arguments{
%%  \item{obj}{a \code{Parameters} object.}
%%  \item{category}{a \code{character} string}
%%  \item{name}{a \code{character} string}
%%}

\value{
  The integer, numeric, logical, character or list stored in the
  specified category, with the specified name.
}
\author{Paul Shannon}

\examples{
   sample.params.file = file.path (find.package ('Parameters'), 'extdata', 'defaultParams.yml')
   params <- Parameters (sample.params.file)
   print (names (params))
   emptyParams <- Parameters ()
   value (emptyParams, "general", "weather") <- "hot" 
   value (emptyParams, "general", "temperature") <- 102.3
}

\keyword{utilities}
