\name{getAll}
\alias{getAll,Parameters-method}
\alias{getAll}
\title{getAll}
\description{
Retrieve the full list of lists in the Parameters object.
}
\usage{
\S4method{getAll}{Parameters}(obj)
}
\arguments{
  \item{obj}{a \code{Parameters} object.}
}

\value{
  A list (of categories), each containing a list of name-value pairs.
}
\author{Paul Shannon}

\examples{
   sample.params.file = file.path (find.package ('Parameters'), 'extdata', 'defaultParams.yml')
   params <- Parameters (sample.params.file)
   x = getAll (params)
   print (names (x))
}

\keyword{utilities}
