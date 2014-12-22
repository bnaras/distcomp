\name{getConfig}
\alias{getConfig}
\title{
  Returns the workspace and other distcomp configuration parameters
}
\description{
  Returns the workspace and other distcomp configuration parameters
}
\usage{
getConfig(...)
}
\arguments{
  \item{...}{
    Any arguments. Currently not used.
  }
}
\details{
  Returned value is a list of items.
  }
\value{
  A list of the following items
  \item{workspacePath}{
    A folder specifying the workspace path. This has to be writable by
    the opencpu process. On a cloud opencpu server on Ubuntu for
    example, this requires a one-time modification of apparmor profiles to enable write
    permissions to this path}
  \item{defnPath}{
    The path where definition files will reside, organized by
    computation identifiers. Should be left unchanged usually.}
  \item{instancePath}{
    The path where instance objects will reside. Should be left
    unchanged usually.}
  \item{defnFileName}{
    The name for the compdef definition files. Should be left at default
    usually.}
  \item{dataFileName}{
    The name for the data files. Should be left unchanged usually.}
  \item{instanceFileName}{
    The name for the instance files. Should be left unchanged usually.}
  \item{ssl.verifyhost}{
    Logical value, usually true. But for testing with snake-oil certs,
    one might set this to \code{FALSE}.}
  \item{ssl.verifypeer}{
    Logical value, usually true. But for testing with snake-oil certs,
    one might set this to \code{FALSE}.}
}
