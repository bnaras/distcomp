\name{uploadNewComputation}
\alias{uploadNewComputation}
\title{
  Uploads a computation to an R server, typically a site.
}

\description{ This function uploads a computation definition, along with
the site-specific data to a server so that a site can be instantiated
for a computation. An optional argument \code{dataFileName} may be
passed to keep data separate when a single process simulates independent
sites.}

\usage{
uploadNewComputation(url, defn, data, dataFileName)
}

\arguments{
  \item{url}{
    A url for the R server
    }
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
    This assumes that the site R server has been set up appropriately.
  }
  \value{
    A logical value indicating success or failure
  }

