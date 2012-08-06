\name{categories}
\alias{categories,Parameters-method}
\alias{categories}
\title{categories}
\description{
Get the categories in a Parameters object.
}
\usage{
\S4method{categories}{Parameters}(obj)
}
\arguments{
  \item{obj}{a \code{Parameters} object.}
}

\value{
  A list (possibly empty) of the current top-level categories in the
  a Parameters object.
}
\author{Paul Shannon}

\examples{
   sample.params.file = file.path (find.package ('Parameters'), 'extdata', 'defaultParams.yml')
   params <- Parameters (sample.params.file)
   print (categories (params))
}

\keyword{utilities}
