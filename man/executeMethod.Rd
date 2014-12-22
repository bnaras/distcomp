\name{executeMethod}
\alias{executeMethod}
\title{
  A function to execute a method on an object with args
}
\description{
  This function takes an object identifier and a method name and
  optional arguments and returns the result of a method call on the
  object
}
\usage{
executeMethod(objectId, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{objectId}{
    An object identifier
  }
  \item{method}{
    A method name
  }
  \item{...}{
    Additional arguments
  }
}
\details{
  Run time errors are possible.
}
\value{
  Result of the method call
}
