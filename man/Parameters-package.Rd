\name{Parameters-package}
\alias{Parameters-package}
\docType{package}
\title{Parameters:  Parameters: general purpose YAML-based parameter access
}
\description{
  A simple utility class for retrieving parameter values from a YAML
  file, modifying and querying these values as needed.  It will be most
  useful to package builders whose running code uses a large number
  of run-time parameters and 'tuning' variables.
}
\details{
\tabular{ll}{
Package: \tab Parameters\cr
Type: \tab Package\cr
Version: \tab 0.9.5\cr
Date: \tab 2012-08-05\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
}
}
\author{
Paul Shannon <pshannon@fhcrc.org>
Maintainer: Paul Shannon <pshannon@fhcrc.org>
}
\keyword{dynamic}
\examples{
   sample.params.file = file.path (find.package ('Parameters'), 'extdata', 'defaultParams.yml')
   params <- Parameters (sample.params.file)
     # top level
   params.all <- getAll (params)
   print (names (params.all))
   head (params.all$general)
   value (params, 'newTopLevelCateogry', 'foo') <- 'bar'
}
