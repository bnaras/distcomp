\name{availableComputations}
\alias{availableComputations}
\docType{data}
\title{
  Returns a list of available computations
}
\description{
  Returns a list of available computations that have been
  implemented. This list will expand over time. Currently, an iterative
  SVD algorithm and a novel distributed Stratified Cox model computation
  are implemented.
}
\usage{
availableComputations()
}
\details{
  The return value is a named list of strings. The names are unique keys
  that are used throughout the package.
}
\examples{
availableComputations()
}
