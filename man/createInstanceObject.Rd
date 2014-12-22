\name{createInstanceObject}
\alias{createInstanceObject}
\title{
  Instantiates an object that will be used in an iteration
}
\description{
This function creates an instance object that will take part in a
computation. The instance object may be stateful, in which case, it will
be repeatedly modified and saved during the course of the iteration.
}
\usage{
createInstanceObject(defnId, instanceId, dataFileName)
}

\arguments{
  \item{defnId}{
    An identifier specifying the computation instance
  }
  \item{instanceId}{
    An identifier to use for the instance object
  }
  \item{dataFileName}{
    The data file name to use for creating the instance. Typically only
    needed for a single server simulating several sites
  }
}
\details{
  This assumes that a workspace has been set up for the data
}
\value{
  A logical value indicating success or failure
}

