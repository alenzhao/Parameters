\name{Parameters}
\alias{Parameters}
\title{Parameters}
\description{
This is the constructor for the S4 parameters object.
}
\usage{
Parameters (arg)
}

\arguments{
       \item{arg}{NA or aa character string naming, directly or
       indirectly, a YAML file with parameter settings.  The character
       string can be a package names, in which case the constructor
       looks for a file 'DefaultParams.yml' in the extdata directory of
       the installed package.  Alternatively, the character string can
       name a yaml file directly and explicitly.  If NA, an empty
       Parameters object is returned, which may be manipulated with the
       values method. }
     }
     

\value{
A Parameters object.
}
\author{Paul Shannon}

\examples{
   #paramsFromPackage = Parameters ('Parameters')  # load extdata/defaultParams.yml
   #paramsEmpty = Parameters ()
     # the only guaranteed yaml file is the one loaded with this
     # package.  find it explicitly
  #filename <- file.path (find.package ('Parameters'), 'extdata', 'defaultParams.yml')
  #paramsExplicit <- Parameters (filename)
}

\keyword{utilities}
