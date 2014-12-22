\name{distcompSetup}
\alias{distcompSetup}
\title{
  Sets up a distributed computation.
}
\description{
  This function sets up a distributed computation and configures some
  global parameters such as definition file names, data file names,
  instance object file names, and ssl configuration parameters.
}
\usage{
distcompSetup(workspacePath, defnPath = paste(workspacePath,
    "defn", sep = .Platform$file.sep), instancePath = paste(workspacePath,
    "instances", sep = .Platform$file.sep), defnFileName = "defn.rds",
    dataFileName = "data.rds", instanceFileName = "instance.rds",
    ssl.verifyhost = TRUE, ssl.verifypeer = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
