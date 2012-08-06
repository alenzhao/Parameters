\name{Parameters-class}
\docType{class}

\alias{class:Parameters}
\alias{Parameters-class}

\title{Parameters}

\description{
Implements a simple YAML-based approach to possibly nested lists of
parameter (name-value) assignemnts.
}

\section{Accessor Methods}{
  \describe{
    \item{}{
      \code{value(x, category, name)}: returns int, numeric, character,
  logical or list from specified category with specified name.
     }
   \item{}{
      \code{getAll(x)}: returns nested list
    }
   }
 }

\author{Paul Shannon}


\keyword{methods}
\keyword{classes}
