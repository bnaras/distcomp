\name{destroyInstanceObject}
\alias{destroyInstanceObject}
\title{
  Deletes a previously created instance object
}
\description{
This function destroys an instance object that was created earlier,
typically at the end of an iteration.
}
\usage{
destroyInstanceObject(instanceId)
}

\arguments{
  \item{instanceId}{
    An identifier specifying the instance object
  }
}
\details{
  This assumes that a workspace has been set up for the data
}
\value{
  A logical value indicating success or failure
}

