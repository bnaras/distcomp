\name{saveNewComputation}
\alias{saveNewComputation}
\title{
  Save a computation in the workspace
}
\description{
This function saves a computation definition, along with the
site-specific data in the workspace so that it becomes available for
use. An optional argument
\code{dataFileName} may be passed to keep data separate when a single process simulates independent sites.
}
\usage{
saveNewComputation(defn, data, dataFileName)
}

\arguments{
  \item{defn}{
    A list defining the computation, per the type of computation
  }
  \item{data}{
    A dataset to use for the computation
  }
  \item{dataFileName}{
    A specific file name to store the data file under
  }
}
\details{
  This assumes that a workspace has been set up for the data
}
\value{
  A logical value indicating success or failure
}

