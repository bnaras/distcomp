#' Distributed Computing with R
#'
#' `distcomp` is a collection of methods to fit models to data that may be
#' distributed at various sites. The package arose as a way of addressing the
#' issues regarding data aggregation; by allowing sites to have control over
#' local data and transmitting only summaries, some privacy controls can be
#' maintained. Even when participants have no objections in principle to data
#' aggregation, it may still be useful to keep data local and expose just the
#' computations. For further details, please see the reference cited below.
#'
#' The initial implementation consists of a stratified Cox model fit with
#' distributed survival data and a Singular Value Decomposition
#' of a distributed matrix. General Linear Models will soon be added.
#' Although some sanity checks and balances are present, many more are needed
#' to make this truly robust. We also hope that other methods will be added by users.
#'
#' We make the following assumptions in the implementation:
#' (a) the aggregate data is logically a stacking of data at each site, i.e.,
#' the full data is row-partitioned into sites where the rows are observations;
#' (b) Each site has the package `distcomp` installed and a workspace setup
#' for (writeable) use by the `opencpu` server
#' (see [distcompSetup()]; and (c) each site is exposing `distcomp`
#' via an `opencpu` server.
#'
#' The main computation happens via a master process, a script of R code,
#' that makes calls to `distcomp` functions at worker sites via `opencpu`.
#' The use of `opencpu` allows developers to prototype their distributed implementations
#' on a local machine using the `opencpu` package that runs such a server locally
#' using `localhost` ports.
#'
#' Note that `distcomp` computations are not intended for speed/efficiency;
#' indeed, they are orders of magnitude slower. However, the models that are fit are
#' not meant to be recomputed often. These and other details are discussed in the
#' paper mentioned above.
#'
#' The current implementation, particularly the Stratified Cox Model, makes direct use of
#' code from [survival::coxph()]. That is, the underlying Cox model code is
#' derived from that in the R `survival` survival package.
#'
#' For an understanding of how this package is meant to be used, please see the documented
#' examples and the reference.
#' @seealso The examples in `system.file("doc", "examples.html", package="distcomp")`
#' @seealso The source for the examples: `system.file("doc_src", "examples.Rmd", package="distcomp")`.
#' @docType package
#' @references Software for Distributed Computation on Medical Databases:
#' A Demonstration Project. Journal of Statistical Software, 77(13), 1-22.
#' doi:10.18637/jss.v077.i13
#' @references Appendix E of Modeling Survival Data: Extending the Cox Model by
#' Terry M. Therneau and Patricia Grambsch. Springer Verlag, 2000.
#' @name distcomp
NULL

#' Make an appropriate opencpu URL for a specified function and url prefix for the
#' distcomp package
#'
#' @description .makeOpencpuURL returns an appropriate URL to call a function in the distcomp
#' package given the name of the function and a url prefix.
#'
#' @param fn is the name of the function in the distcomp package
#' @param urlPrefix is the URL of the opencpu server with the distcomp package installed
#' @return the formatted url as a string
#'
#' @rdname distcomp-internal
#' @import utils
#' @importFrom stringr str_trim
#' @importFrom httr POST stop_for_status
#' @importFrom httr headers
#' @importFrom httr add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#'
#' @examples
#' distcomp:::.makeOpencpuURL("foo", "http://localhost:9999/ocpu")
#'
#' @keywords internal
.makeOpencpuURL <- function(fn, urlPrefix, package="distcomp") {
  paste(urlPrefix, "library", package, "R", fn, "json", sep="/")
}

#' Check that a definition object meets minimal requirements
#'
#' @description .defnOK returns TRUE or FALSE depending on whether the definition object
#' meets minimimal requirements.
#'
#' @param defn is the definition object passed
#' @return TRUE or FALSE depending on the result
#'
#' @rdname distcomp-internal
#'
#' @examples
#' distcomp:::.defnOK(data.frame()) ## FALSE
#' distcomp:::.defnOK(data.frame(id = "ABC", stringsAsFactors=FALSE)) ## TRUE
#'
#' @keywords internal
.defnOK <- function(defn) {
    ! is.null(defn$id) && nchar(defn$id) > 0
}

#' Setup a workspace and configuration for a distributed computation
#' @description The function `distcompSetup` sets up a distributed computation
#' and configures some global parameters such as definition file names,
#' data file names, instance object file names, and ssl configuration parameters. The
#' function creates some of necessary subdirectories if not already present and throws
#' an error if the workspace areas are not writeable
#' @seealso [getConfig()]
#'
#' @param workspacePath a folder specifying the workspace path. This
#'     has to be writable by the opencpu process. On a cloud opencpu
#'     server on Ubuntu, for example, this requires a one-time
#'     modification of apparmor profiles to enable write permissions
#'     to this path
#' @param defnPath the path where definition files will reside,
#'     organized by computation identifiers
#' @param instancePath the path where instance objects will reside
#' @param defnFileName the name for the compdef definition files
#' @param dataFileName the name for the data files
#' @param instanceFileName the name for the instance files
#' @param resultsCacheFileName the name for the instance results cache files for HE computations
#' @param ssl_verifyhost integer value, usually `1L`, but for
#'     testing with snake-oil certs, one might set this to `0L`
#' @param ssl_verifypeer integer value, usually `1L`, but for
#'     testing with snake-oil certs, one might set this to `0L`
#' @return TRUE if all is well
#'
#' @importFrom httr config
#'
#' @examples
#' \dontrun{
#' distcompSetup(workspacePath="./workspace")
#' }
#' @export
distcompSetup <- function(workspacePath = "",
                          defnPath = paste(workspacePath, "defn", sep=.Platform$file.sep),
                          instancePath = paste(workspacePath, "instances", sep=.Platform$file.sep),
                          defnFileName = "defn.rds",
                          dataFileName = "data.rds",
                          instanceFileName = "instance.rds",
                          resultsCacheFileName = "results_cache.rds",
                          ssl_verifyhost = 1L,
                          ssl_verifypeer = 1L) {
  ## TODO: In the next version, this should be stuffed in an R6 class
  testFileName <- paste(sample(letters, 15), collapse="")
  if (!file.exists(defnPath)) {
    defnOk <- dir.create(defnPath)
    if (!defnOk) {
      stop("distcompSetup: workspace permissions issue: not writable!")
    }
  } else {
    ## defnPath exists; check if it is writable
    testFile <- paste(defnPath, testFileName, sep=.Platform$file.sep)
    createOk <- file.create(testFile)
    if (!createOk) {
      stop("distcompSetup: workspace permissions issue: not writable!")
    }
    file.remove(testFile)
  }

  if (!file.exists(instancePath)) {
    defnOk <- dir.create(instancePath)
    if (!defnOk) {
      stop("distcompSetup: workspace permissions issue: not writable!")
    }
  } else {
    ## instancePath exists; check if it is writable
    testFile <- paste(instancePath, testFileName, sep=.Platform$file.sep)
    createOk <- file.create(testFile)
    if (!createOk) {
      stop("distcompSetup: workspace permissions issue: not writable!")
    }
    file.remove(testFile)
  }

  distcompEnv <- getOption("distcompEnv")
  distcompEnv[["config"]] <- list(workspacePath = workspacePath,
                                  defnPath = defnPath,
                                  instancePath = instancePath,
                                  defnFileName = defnFileName,
                                  dataFileName = dataFileName,
                                  instanceFileName = instanceFileName,
                                  resultsCacheFileName = resultsCacheFileName,
                                  sslConfig = config(ssl_verifyhost = ssl_verifyhost,
                                    ssl_verifypeer = ssl_verifypeer))
  distcompEnv[["computationInfo"]] <- list()
  TRUE
}


#' Return the workspace and configuration setup values
#' @description The function `getConfig` returns the values of the
#' configuration parameters set up by `distcompSetup`
#' @seealso [distcompSetup()]
#' @param ... any further arguments
#' @return a list consisting of
#' \item{workspacePath}{a folder specifying the workspace path. This has to be
#' writable by the opencpu process. On a cloud opencpu server on Ubuntu, for example,
#' this requires a one-time modification of apparmor profiles to enable write
#' permissions to this path}
#' \item{defnPath}{the path where definition files will reside, organized by
#' computation identifiers}
#' \item{instancePath}{the path where instance objects will reside}
#' \item{defnFileName}{the name for the compdef definition files}
#' \item{dataFileName}{the name for the data files}
#' \item{instanceFileName}{the name for the instance files}
#' \item{ssl_verifyhost}{integer value, usually `1L`, but for testing with
#' snake-oil certs, one might set this to `0L`}
#' \item{ssl_verifypeer}{integer value, usually `1L`, but for testing with
#' snake-oil certs, one might set this to `0L`}
#'
#' @examples
#' \dontrun{
#' getConfig()
#' }
#' @export
getConfig <- function(...) {
  ## TODO: In the next version, this should be stuffed in an R6 class
  getOption("distcompEnv")[["config"]]
}


#' Make a worker object given a definition and data
#' @description The function `makeWorker` returns an object of the
#'     appropriate type based on a computation definition and sets the
#'     data for the object. The types of objects that can be created
#'     depend upon the available computations
#' @seealso [availableComputations()]
#' @param defn the computation definition
#' @param data the data for the computation
#' @param pubkey_bits the number of bits for the public key (used only
#'     if `he` is `TRUE` in computation definition)
#' @param pubkey_n the `n` for public key (used only if `he` is `TRUE`
#'     in computation definition)
#' @param den_bits the number of bits for the denominator (used only
#'     if `he` is `TRUE` in computation definition)
#' @return a worker object of the appropriate class based on the
#'     definition
#'
#' @export
makeWorker <- function (defn, data, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
  compType <- defn$compType
  available <- availableComputations()
  k <- match(compType, names(available))
  if (is.na(k)) {
    stop(sprintf("No such computation: %s", compType))
  } else {
      available[[k]]$makeWorker(defn = defn, data = data, pubkey_bits = pubkey_bits,
                                pubkey_n = pubkey_n, den_bits = den_bits)
  }
}

#' Make a master object given a definition
#' @description The function `makeMaster` returns a master object
#'     corresponding to the definition. The types of master objects
#'     that can be created depend upon the available computations
#' @seealso [availableComputations()]
#' @param defn the computation definition
#' @param partyNumber the number of the noncooperating party, which
#'     can be optionally set if HE is desired
#' @param debug a debug flag
#' @return a master object of the appropriate class based on the
#'     definition
#'
#' @export
makeMaster <- function(defn, partyNumber = NULL, debug = FALSE) {
    compType <- defn$compType
    available <- availableComputations()
    k <- match(compType, names(available))
    if (is.na(k)) {
        stop(sprintf("No such computation: %s", compType))
    } else {
        available[[k]]$makeMaster(defn = defn, partyNumber = partyNumber, debug = debug)
    }
}

#' Return the currently available (implemented) computations
#' @description The function `availableComputations` returns a list
#' of available computations with various components. The names of this list
#' (with no spaces) are unique canonical tags that are used throughout the
#' package to unambiguously refer to the type of computation; web applications
#' particularly rely on this list to instantiate objects. As more computations
#' are implemented, this list is augmented.
#' @seealso [getComputationInfo()]
#' @return a list with the components corresponding to a computation
#' \item{desc}{a textual description (25 chars at most)}
#' \item{definitionApp}{the name of a function that will fire up a shiny webapp
#' for defining the particular computation}
#' \item{workerApp}{the name of a function that will fire up a shiny webapp
#' for setting up a worker site for the particular computation}
#' \item{masterApp}{the name of a function that will fire up a shiny webapp
#' for setting up a master for the particular computation}
#' \item{makeDefinition}{the name of a function that will return a data frame
#' with appropriate fields needed to define the particular computation assuming
#' that they are populated in a global variable. This function is used by web
#' applications to construct a definition object based on inputs specified
#' by the users. Since the full information is often gathered incrementally by
#' several web applications, the inputs are set in a global variable and
#' therefore retrieved here using the function `getComputationInfo`
#' designed for the purpose}
#' \item{makeMaster}{a function that will construct a master object for the
#' computation given the definition and a logical flag indicating
#' if debugging is desired}
#' \item{makeWorker}{a function that will construct
#' a worker object for that computation given the definition and data}
#'
#' @examples
#' availableComputations()
#' @export
availableComputations <- function() {
  list(
      QueryCount = list(
          desc = "Distributed Query Count",
          definitionApp = "defineNewQueryCountModel",
          setupWorkerApp = "setupQueryCountWorker",
          setupMasterApp = "setupQueryCountMaster",
          makeDefinition = function() {
              data.frame(id = getComputationInfo("id"),
                         compType = getComputationInfo("compType"),
                         projectName = getComputationInfo("projectName"),
                         projectDesc = getComputationInfo("projectDesc"),
                         he = getComputationInfo("he"),
                         filterCondition = getComputationInfo("filterCondition"),
                         stringsAsFactors=FALSE)
          },
          makeMaster = function(defn, partyNumber, debug = FALSE) {
              if(!is.null(defn$he) && defn$he) {
                  HEQueryCountMaster$new(defn = defn, partyNumber = partyNumber, debug = debug)
              } else {
                  QueryCountMaster$new(defn = defn, debug = debug)
              }
          },
          makeWorker = function(defn, data, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
              if (defn$he) {
                  HEQueryCountWorker$new(defn = defn, data = data, pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)
              } else {
                  QueryCountWorker$new(defn = defn, data = data)
              }
          }
      ),
      StratifiedCoxModel = list(
          desc = "Stratified Cox Model",
          definitionApp = "defineNewCoxModel",
          setupWorkerApp = "setupCoxWorker",
          setupMasterApp = "setupCoxMaster",
          makeDefinition = function() {
              data.frame(id = getComputationInfo("id"),
                         compType = getComputationInfo("compType"),
                         projectName = getComputationInfo("projectName"),
                         projectDesc = getComputationInfo("projectDesc"),
                         he = getComputationInfo("he"),
                         formula = getComputationInfo("formula"),
                         stringsAsFactors=FALSE)
          },
          makeMaster = function(defn, partyNumber, debug = FALSE) {
              if(!is.null(defn$he) && defn$he) {
                  stop("Not implemented")
              } else {
                  CoxMaster$new(defn = defn, debug = debug)
              }
          },
          makeWorker = function(defn, data, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
              if(!is.null(defn$he) && defn$he) {
                  stop("Not implemented")
              } else {
                  CoxWorker$new(defn = defn, data = data)
              }
          }
      ),
      RankKSVD = list(
          desc = "Rank K SVD",
          definitionApp = "defineNewSVDModel",
          setupWorkerApp = "setupSVDWorker",
          setupMasterApp = "setupSVDMaster",
          makeDefinition = function() {
              data.frame(id = getComputationInfo("id"),
                         compType = getComputationInfo("compType"),
                         projectName = getComputationInfo("projectName"),
                         projectDesc = getComputationInfo("projectDesc"),
                         he = getComputationInfo("he"),
                         rank = getComputationInfo("rank"),
                         ncol = getComputationInfo("ncol"),
                         stringsAsFactors=FALSE)
          },
          makeMaster = function(defn, partyNumber, debug = FALSE) {
              if(!is.null(defn$he) && defn$he) {
                  stop("Not implemented")
              } else {
                  SVDMaster$new(defn = defn, debug = debug)
              }
          },
          makeWorker = function(defn, data, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
              if(!is.null(defn$he) && defn$he) {
                  stop("Not implemented")
              } else {
                  SVDWorker$new(defn = defn, data = data)
              }
          }
      )
  )
}

#' Given the definition identifier of an object, instantiate and store
#' object in workspace
#' @description This function uses an identifier (`defnId`) to locate
#'     a stored definition in the workspace to create the appropriate
#'     object instance. The instantiated object is assigned the
#'     instanceId and saved under the dataFileName if the latter is
#'     not `NULL`.  This instantiated object may change state between
#'     iterations when a computation executes
#' @param name identifying the NC party
#' @param ncpId the id indicating the NCP definition
#' @param instanceId an indentifier to use for the created instance
#' @param pubkey_bits the public key number of bits
#' @param pubkey_n the pubkey n
#' @param den_bits the denominator number of bits for for rational
#'     approximations
#' @param dataFileName a file name to use for saving the
#'     data. Typically `NULL`, this is only needed when one is using a
#'     single opencpu server to behave like multiple sites in which
#'     case the data file name serves to distinguish the site-specific
#'     data files.  When it is `NULL`, the data file name is taken
#'     from the configuration settings
#' @import utils
#' @return TRUE if everything goes well
#' @export
createNCPInstance <- function (name, ncpId, instanceId, pubkey_bits, pubkey_n, den_bits, dataFileName = NULL) {
    config <- getConfig()
    defnPath <- paste(config$defnPath, ncpId, sep=.Platform$file.sep)
    defnFileName <- paste(name, "defn.rds", sep = "-")
    defn <- readRDS(paste(defnPath, defnFileName, sep=.Platform$file.sep))
    ncp <- makeNCP(ncp_defn = defn$defn, comp_defn = defn$comp_defn, pubkey_bits = pubkey_bits,
                   pubkey_n = gmp::as.bigz(pubkey_n), den_bits = den_bits)

    ncpDataFileName  <- if (is.null(dataFileName)) {
                            paste(name, config$dataFileName, sep = "-")
                        } else {
                            dataFileName
                        }
    sites  <- readRDS(paste(defnPath, ncpDataFileName, sep=.Platform$file.sep))
    site_names  <- sites$name
    site_urls  <- sites$url
    ## The JSON serialization makes the columns as lists, so use double bracket indexing!
    for (i in seq_along(site_names)) {
        ncp$addSite(name = site_names[[i]], url = site_urls[[i]])
    }

    ## Check if the instance folder exists
    thisInstancePath <- paste(config$instancePath, instanceId, sep=.Platform$file.sep)
    dir.create(thisInstancePath)
    ## Save it under the instance id to find it.
    saveRDS(ncp, file=paste(thisInstancePath, config$instanceFileName, sep=.Platform$file.sep))
    TRUE
}


#' Return currently implemented data sources
#' @description The function `availableDataSources` returns the
#' currently implemented data sources such as CSV files, Redcap etc.
#'
#' @return a list of named arguments, each of which is another list, with
#' required fields named `desc`, a textual description and
#' `requiredPackages`
#'
#' @examples
#' availableDataSources()
#' @export
availableDataSources <- function() {
  list(
    CSVFile = list(desc = "CSV File", requiredPackages = list())
    , Redcap = list(desc = "Redcap API", requiredPackages = list("redcapAPI"))
    ##, Postgres = list(desc = "Postgres", requiredPackages = list("RPostgreSQL"))
  )
}


#' Make a computation definition given the computation type
#' @description The function `makeDefinition` returns a computational
#' definition based on current inputs (from the global store) given a
#' canonical computation type tag. This is a utility function for web
#' applications to use as input is being gathered
#'
#' @seealso [availableComputations()]
#' @param compType the canonical computation type tag
#' @return a data frame corresponding to the computation type
#' @examples
#' \dontrun{
#' makeDefinition(names(availableComputations())[1])
#' }
#' @export
makeDefinition <- function(compType) {
  available <- availableComputations()
  k <- match(compType, names(available))
  if (is.na(k)) {
    stop(paste("makeDefintion: No such computation:", compType))
  } else {
    available[[k]]$makeDefinition()
  }
}

#' Given the id of a serialized object, invoke a method on the object
#' with arguments
#' @description The function `executeMethod` is really the heart of
#'     distcomp.  It executes an arbitrary method on an object that
#'     has been serialized to the distcomp workspace with any
#'     specified arguments. The result, which is dependent on the
#'     computation that is executed, is returned. If the object needs
#'     to save state between iterations on it, it is automatically
#'     serialized back for the ensuing iterations
#' @param objectId the (instance) identifier of the object on which to invoke a method
#' @param method the name of the method to invoke
#' @param ... further arguments as appropriate for the method
#' @return a result that depends on the computation being executed
#' @export
executeMethod <- function(objectId, method, ...) {
  config <- getConfig()
  filePath <- paste(config$instancePath, objectId, config$instanceFileName, sep=.Platform$file.sep)
  object <- readRDS(file=filePath)
  call <- substitute(object$METHOD(...), list(METHOD = as.name(method)))
  result <- eval(call)
  if (object$getStateful()) {
    saveRDS(object, file=filePath)
  }
  result
}

#' Given the id of a serialized object, invoke a method on the object
#' with arguments using homomorphic encryption
#' @description The function `executeHEMethod` is a homomorphic
#'     encryption wrapper around `executeMethod`. It ensures any
#'     returned result is encrypted using the homomorphic encryption
#'     function.
#' @param objectId the (instance) identifier of the object on which to
#'     invoke a method
#' @param method the name of the method to invoke
#' @param ... further arguments as appropriate for the method
#' @return a list containing an integer and a fractional result converted to characters
#' @export
executeHEMethod <- function(objectId, method, ...) {
  cat("In executeHEMethod\n")
  config <- getConfig()
  print(config)
  filePath <- paste(config$instancePath, objectId, config$instanceFileName, sep=.Platform$file.sep)
  cat("File path is \n")
  print(filePath)
  object <- readRDS(file=filePath)
  cat("Read object\n")
  call <- substitute(object$METHOD(...), list(METHOD = as.name(method)))
  cat("created call\n")
  print(call)
  result <- eval(call)
  cat("Result is \n")
  print(result)
  cat("Saving object back\n")
  if (object$getStateful()) {
    saveRDS(object, file=filePath)
  }
  cat("Saved object back \n")
  ## For an HE method, return results as strings since we need to serialize things over
  ## the wire!
  result  <- list(int = as.character(result$int),
                  frac = as.character(result$frac))
  cat("Result is\n")
  print(result)
  result
}

#' Deserialize the result of a http response
#' @description .deSerialize will convert the JSON result of a http response as needed,
#' else the raw content is returned.
#'
#' @rdname distcomp-internal
#' @import utils
#' @importFrom stringr str_trim
#' @importFrom httr POST
#' @importFrom httr headers
#' @importFrom httr add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @param q the result of a httr response
#' @return the converted result, if JSON, or the raw content
#'
#' @keywords internal
## Deserialize URL result, only handle JSON at the moment
.deSerialize <- function(q) {
    httr::stop_for_status(q)
    cType <- headers(q)['content-type']
    if (cType == "application/json")
        jsonlite::fromJSON(rawToChar(q$content), simplifyDataFrame = FALSE)
    else
        q$content
}


#' Given the definition identifier of an object, instantiate and store object in workspace
#' @description The function `createWorkerInstance` uses a definition identified by
#' defnId to create the appropriate object instance. The instantiated object is assigned
#' the instanceId and saved under the dataFileName if the latter is specified.
#' This instantiated object may change state between iterations when a computation executes
#' @seealso [availableComputations()]
#' @param defnId the identifier of an already defined computation
#' @param instanceId an indentifier to use for the created instance
#' @param pubkey_bits number of bits for public key
#' @param pubkey_n the `n` for public key
#' @param den_bits the number of bits for the denominator
#' @param dataFileName a file name to use for saving the data. Typically `NULL`, this
#' is only needed when one is using a single opencpu server to behave like multiple
#' sites in which case the data file name serves to distinguish the site-specific data files.
#' When it is `NULL`, the data file name is taken from the configuration settings
#' @import utils
#' @return TRUE if everything goes well
#' @export
createWorkerInstance <- function (defnId, instanceId, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL, dataFileName=NULL) {
  config <- getConfig()
  defn <- readRDS(paste(config$defnPath, defnId, config$defnFileName, sep=.Platform$file.sep))
  compType <- defn$compType
  available <- availableComputations()
  if (!(compType %in% names(available))) {
    stop(paste("createWorkerInstance: No such computation:", compType))
  }
  data <- readRDS(paste(config$defnPath, defnId,
                        if (is.null(dataFileName)) config$dataFileName else dataFileName,
                        sep=.Platform$file.sep))

  object <- makeWorker(defn = defn, data = data, pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)

  ## Check if the instance folder exists
  thisInstancePath <- paste(config$instancePath, instanceId, sep=.Platform$file.sep)
  dir.create(thisInstancePath)
  ## Save it under the instance id to find it.
  saveRDS(object, file=paste(thisInstancePath, config$instanceFileName, sep=.Platform$file.sep))
  TRUE
}

#' Given the definition identifier of an object, instantiate and store
#' object in workspace
#' @description The function `createHEWorkerInstance` uses a
#'     definition identified by defnId to create the appropriate
#'     object instance for HE computations. The instantiated object is
#'     searched for in the instance path and loaded if already
#'     present, otherwise it is created and assigned the instanceId
#'     and saved under the dataFileName if the latter is specified.
#'     This instantiated object may change state between iterations
#'     when a computation executes
#' @seealso [availableComputations()]
#' @param defnId the identifier of an already defined computation
#' @param instanceId an indentifier to use for the created instance
#' @param pubkey_bits number of bits for public key
#' @param pubkey_n the `n` for public key
#' @param den_bits the number of bits for the denominator
#' @param dataFileName a file name to use for saving the
#'     data. Typically `NULL`, this is only needed when one is using a
#'     single opencpu server to behave like multiple sites in which
#'     case the data file name serves to distinguish the site-specific
#'     data files.  When it is `NULL`, the data file name is taken
#'     from the configuration settings
#' @import utils
#' @return TRUE if everything goes well
#' @export
createHEWorkerInstance <- function (defnId, instanceId, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL, dataFileName=NULL) {
  config <- getConfig()
  thisInstancePath <- paste(config$instancePath, instanceId, sep=.Platform$file.sep)
  instanceFile  <- file.path(thisInstancePath, config$instanceFileName)
  if (!file.exists(instanceFile)) {
      defn <- readRDS(paste(config$defnPath, defnId, config$defnFileName, sep=.Platform$file.sep))
      compType <- defn$compType
      available <- availableComputations()
      if (!(compType %in% names(available))) {
          stop(paste("createWorkerInstance: No such computation:", compType))
      }
      data <- readRDS(paste(config$defnPath, defnId,
                            if (is.null(dataFileName)) config$dataFileName else dataFileName,
                            sep=.Platform$file.sep))

      object <- makeWorker(defn = defn, data = data, pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)
      ## Check if the instance folder exists
      thisInstancePath <- paste(config$instancePath, instanceId, sep=.Platform$file.sep)
      dir.create(thisInstancePath)
      ## Save it under the instance id to find it.
      saveRDS(object, file=paste(thisInstancePath, config$instanceFileName, sep=.Platform$file.sep))
  }
  TRUE
}



#' Destroy an instance object given its identifier
#' @description The function `destroyInstanceObject` deletes an object associated
#' with the instanceId. This is typically done after a computation completes and results
#' have been obtained.
#' @param instanceId the id of the object to destroy
#' @seealso [createWorkerInstance()]
#' @import utils
#' @return TRUE if everything goes well
#' @export
destroyInstanceObject <- function (instanceId) {
  config <- getConfig()
  file.remove(paste(config$instancePath, instanceId, config$instanceFileName, sep=.Platform$file.sep))
  file.remove(paste(config$instancePath, instanceId, sep=.Platform$file.sep))
  TRUE
}

#' Save a computation instance, given the computation definition, associated data and
#' possibly a data file name to use
#' @description The function `saveNewComputation` uses the computation definition to save
#' a new computation instance. This is typically done for every site that wants to participate
#' in a computation with its own local data. The function examines the computation definition
#' and uses the identifier therein to uniquely refer to the computation instance at the site.
#' This function is invoked (maybe remotely) on the opencpu server by
#' [uploadNewComputation()] when a worker site is being set up
#' @seealso [uploadNewComputation()]
#' @param defn an already defined computation
#' @param data the (local) data to use
#' @param dataFileName a file name to use for saving the data. Typically `NULL`, this
#' is only needed when one is using a single opencpu server to behave like multiple
#' sites in which case the data file name serves to distinguish the site-specific data files.
#' When it is `NULL`, the data file name is taken from the configuration settings
#' @return TRUE if everything goes well
#' @export
saveNewComputation <- function(defn, data, dataFileName=NULL) {
  config <- getConfig()
  defnId <- defn$id
  thisDefnPath <- paste(config$defnPath, defnId, sep=.Platform$file.sep)
  ## Should tryCatch this
  if (!file.exists(thisDefnPath)) {
    dir.create(thisDefnPath)
  }
  saveRDS(object=defn, file=paste(thisDefnPath, config$defnFileName, sep=.Platform$file.sep))
  if (is.null(dataFileName)) {
    saveRDS(object=data, file=paste(thisDefnPath, config$dataFileName, sep=.Platform$file.sep))
  } else {
    saveRDS(object=data, file=paste(thisDefnPath, dataFileName, sep=.Platform$file.sep))
  }
  TRUE
}

#' Upload a new computation and data to an opencpu server
#' @description The function `uploadNewComputation` is really a remote version
#' of [saveNewComputation()], invoking that function on an opencpu server.
#' This is typically done for every site that wants to participate in a computation
#' with its own local data. Note that a site is always a list of at least a unique
#' name element (distinguishing the site from others) and a url element.
#' @seealso [saveNewComputation()]
#' @param site a list of two items, a unique `name` and a `url`
#' @param defn the identifier of an already defined computation
#' @param data the (local) data to use
#' @importFrom stringr str_trim
#' @importFrom httr POST
#' @importFrom httr headers
#' @importFrom httr add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#'
#' @return TRUE if everything goes well
#' @export
uploadNewComputation <- function(site, defn, data) {
  if (! .defnOK(defn)) {
      stop("uploadNewComputation: Improper definition")
  }
  if (is.null(site$worker)) {
      localhost <- (grepl("^http://localhost", site$url) || grepl("^http://127.0.0.1", site$url))
      payload <- if (localhost) {
                     list(defn = defn, data = data, dataFileName = paste0(site$name, ".rds"))
                 } else {
                     list(defn = defn, data = data)
                 }
      q <- POST(.makeOpencpuURL(urlPrefix = site$url, fn = "saveNewComputation"),
                body = jsonlite::toJSON(payload),
                add_headers("Content-Type" = "application/json"),
                config=getConfig()$sslConfig
                )
      .deSerialize(q)
  } else {

  }
}

#' Save an NCP instance, given the sites as associated data and
#' possibly a data file name to use
#' @description The function `saveNewNCP` uses the list of sites
#'     definition to save a new NCP instance. This is
#'     typically done for every pair of NCPs used in a computation. The function examines the
#'     computation definition and uses the identifier therein to
#'     uniquely refer to the computation instance at the site.  This
#'     function is invoked (maybe remotely) on the opencpu server by
#'     [uploadNewComputation()] when a worker site is being set up
#' @seealso [uploadNewNCP()]
#' @param defn a definition of the ncp
#' @param comp_defn the computation definition
#' @param data the list of sites with name and url to use
#' @param dataFileName a file name to use for saving the
#'     data. Typically `NULL`, this is only needed when one is using a
#'     single opencpu server to behave like multiple sites in which
#'     case the data file name serves to distinguish the site-specific
#'     data files.  When it is `NULL`, the data file name is taken
#'     from the definition settings
#' @return TRUE if everything goes well
#' @export
saveNewNCP <- function(defn, comp_defn, data, dataFileName = NULL) {
  config <- getConfig()
  defnId <- defn$id
  browser()
  thisDefnPath <- paste(config$defnPath, defnId, sep = .Platform$file.sep)
  ncpDefnFileName <- paste(defn$name, "defn.rds", sep = "-")
  ## Should tryCatch this
  if (!file.exists(thisDefnPath)) {
    dir.create(thisDefnPath)
  }
  saveRDS(object = list(defn = defn, comp_defn = comp_defn),
          file = paste(thisDefnPath, ncpDefnFileName, sep = .Platform$file.sep))
  ncpDataFileName  <- if (is.null(dataFileName)) {
                          paste(defn$name, config$dataFileName, sep = "-")
                      } else {
                          dataFileName
                      }
  saveRDS(object = data, file = paste(thisDefnPath, ncpDataFileName, sep = .Platform$file.sep))
  TRUE
}

#' Upload a new Non-Cooperating Party (NCP) information and sites to
#' an opencpu server
#' @description The function `uploadNewNCP` is really a remote version
#'     of [saveNewNCP()], invoking that function on an opencpu server.
#'     This is typically done for the two NCPs participating in a
#'     computation with the list of sites. Note that sites are always
#'     a list of at least a unique name element (distinguishing the
#'     site from others) and a url element.
#' @seealso [saveNewNCP()]
#' @param defn a definition for the NCP
#' @param comp_defn the computation definition
#' @param url the url for the NCP. Only one of url and worker can be
#'     non-null
#' @param worker the worker for the NCP if local. Only one of url and
#'     worker can be non-null
#' @param sites a list of lists, each containing two items, a unique
#'     `name` and a (not necessarily unique) `url`. This is the data
#'     for the NCP!
#' @importFrom stringr str_trim
#' @importFrom httr POST
#' @importFrom httr headers
#' @importFrom httr add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#'
#' @return TRUE if everything goes well
#' @export
uploadNewNCP <- function(defn, comp_defn, url = NULL, worker = NULL, sites) {
    if (is.null(worker)) {
        localhost <- (grepl("^http://localhost", url) || grepl("^http://127.0.0.1", url))
        payload <- if (localhost) {
                       list(defn = defn, comp_defn = comp_defn, data = sites, dataFileName = paste0(defn$name, "-data.rds"))
                   } else {
                       list(defn = defn, comp_defn = comp_defn, data = sites)
                   }
        q <- POST(.makeOpencpuURL(urlPrefix = url, fn = "saveNewNCP"),
                  body = jsonlite::toJSON(payload),
                  add_headers("Content-Type" = "application/json"),
                  config=getConfig()$sslConfig
                  )
        .deSerialize(q)
    } else {

    }
    invisible(TRUE)
}

#' Generate an identifier for an object
#' @description A hash is generated based on the contents of the object
#'
#' @seealso [digest::digest()]
#' @param object the object for which a hash is desired
#' @param algo the algorithm to use, default is "xxhash64" from
#' [digest::digest()]
#' @importFrom digest digest
#' @return the hash as a string
#'
#' @export
generateId <- function(object, algo="xxhash64") digest::digest(object, algo=algo)

##
## We have many apps that need to communicate values between each other.
## So a global area is needed to share variables.
## The function saveModelInfo will save model information in a named list.
## The function getModelInfo will return the value of an item in that list.
##


#' Set a name to a value in a global variable
#' @description In distcomp, several web applications need to communicate
#' between themselves. Since only one application is expected to be
#' active at any time, they do so via a global store, essentially a hash table.
#' This function sets a name to a value
#'
#' @seealso [getComputationInfo()]
#' @param name the name for the object
#' @param value the value for the object
#' @return invisibly returns the all the name value pairs
#'
#' @export
setComputationInfo <- function(name, value) {
  ## TODO: In the next version, this should be stuffed in an R6 class
  distcompEnv <- getOption("distcompEnv")
  currentValue <- distcompEnv[["computationInfo"]]
  ## This is guaranteed to be non-null because of distcompSetup
  ## Set the value
  currentValue[[name]] <- value
  ## Then save it back
  distcompEnv[["computationInfo"]] <- currentValue
  invisible(currentValue)
}

#' Get the value of a variable from the global store
#' @description In distcomp, several web applications need to communicate
#' between themselves. Since only one application is expected to be
#' active at any time, they do so via a global store, essentially a hash table.
#' This function retrieves the value of a name
#'
#' @seealso [setComputationInfo()]
#' @param name the name for the object
#' @return the value for the variable, `NULL` if not set
#'
#' @export
getComputationInfo <- function(name) {
  ## TODO: In the next version, this should be stuffed in an R6 class
  distcompEnv <- getOption("distcompEnv")
  ## The statement below is guaranteed to work because of distcompSetup
  currentValue <- distcompEnv[["computationInfo"]]
  ## Note return value could be null
  currentValue[[name]]
}

#' Clear the contents of the global store
#' @description In distcomp, several web applications need to communicate
#' between themselves. Since only one application is expected to be
#' active at any time, they do so via a global store, essentially a hash table.
#' This function clears the store, except for the working directory.
#'
#' @seealso [setComputationInfo()], [getComputationInfo()]
#' @return an empty list
#'
#' @export
resetComputationInfo <- function() {
  ## TODO: In the next version, this should be stuffed in an R6 class
  workingDir <- getComputationInfo("workingDir")
  distcompEnv <- getOption("distcompEnv")
  if (is.null(workingDir)) {
    invisible(distcompEnv[["computationInfo"]] <- list())
  } else {
    invisible(distcompEnv[["computationInfo"]] <- list(workingDir = workingDir))
  }
}

#' Run a specified distcomp web application
#' @description Web applications can define computation, setup worker sites or masters.
#' This function invokes the appropriate web application depending on the task
#' @seealso [defineNewComputation()], [setupWorker()], [setupMaster()]
#' @import shiny
#' @param appType one of three values: "definition", "setupWorker", "setupMaster"
#' @return the results of running the web application
#'
#' @export
runDistcompApp <- function(appType = c("definition", "setupWorker", "setupMaster")) {
  resetComputationInfo()
  appType <- match.arg(appType)
  app <- paste0(appType, "App")
  appPath <- system.file("webApps", app, package="distcomp")
  compType <- shiny::runApp(appPath, launch.browser=TRUE)
  available <- availableComputations()
  availableNames <- names(available)
  index <- match(compType, availableNames)
  subApp <- available[[index]][[app]]
  appPath <- system.file("webApps", app, subApp, package="distcomp")
  ##print(appPath)
  shiny::runApp(appPath, launch.browser=TRUE)
}

#' Define a new computation
#' @description This function just calls [runDistcompApp()] with the
#' parameter "definition"
#' @seealso [runDistcompApp()]
#' @return the results of running the web application
#'
#' @export
defineNewComputation <- function() {
  setComputationInfo("workingDir", getwd())
  runDistcompApp(appType = "definition")
}

#' Setup a worker site
#' @description This function just calls [runDistcompApp()] with the
#' parameter "setupWorker"
#' @seealso [runDistcompApp()]
#' @return the results of running the web application
#'
#' @export
setupWorker <- function() {
  setComputationInfo("workingDir", getwd())
  runDistcompApp(appType = "setupWorker")
}

#' Setup a computation master
#' @description This function just calls [runDistcompApp()] with the
#' parameter "setupMaster"
#' @seealso [runDistcompApp()]
#' @return the results of running the web application
#'
#' @export
setupMaster <- function() {
  resetComputationInfo()
  setComputationInfo("workingDir", getwd())
  appPath <- system.file("webApps", "setupMasterApp", package="distcomp")
  shiny::runApp(appPath, launch.browser=TRUE)
}

#' Write the code necessary to run a master process
#' @description Once a computation is defined, worker sites are set
#'     up, the master process code is written by this function. The
#'     current implementation does not allow one to mix localhost URLs
#'     with non-localhost URLs
#' @seealso [setupMaster()]
#' @param defn the computation definition
#' @param sites a named list of site URLs participating in the
#'     computation
#' @param outputFilenamePrefix the name of the output file prefix
#'     using which code and data will be written
#' @return the value `TRUE` if all goes well
#' @export
writeCode <- function(defn, sites, outputFilenamePrefix) {
  compType <- defn$compType
  siteNames <- names(sites)
  wd <- getComputationInfo("workingDir")
  f <- file(paste(wd, paste0(outputFilenamePrefix, ".R"), sep=.Platform$file.sep), open="w")
  writeLines("library(distcomp)", con=f)
  ## Change in version 1.2
  ## writeLines(sprintf("defn <- jsonlite::fromJSON('%s')", toJSON(defn)), f)
  ## writeLines(sprintf("sites <- jsonlite::fromJSON('%s', simplifyDataFrame = FALSE)", toJSON(sites)), f)
  ## dump(c("defn", "sites"), f)
  ## Neither dump, nor conversion to JSON is foolproof.  So resorting to
  ## R serialization.
  dataForCode  <- list(defn = defn, sites = sites)
  dataFilename  <- paste0(outputFilenamePrefix, ".RDS")
  dataFilepath <- paste(wd, dataFilename, sep=.Platform$file.sep)
  saveRDS(dataForCode, dataFilepath)
  writeLines(sprintf("masterData <- readRDS('%s')", dataFilename), f)
  writeLines("defn <- masterData$defn; sites <- masterData$sites;", f)
  writeLines("master <- makeMaster(defn)", f)
  writeLines("for (site in sites) {", f)
  writeLines("   master$addSite(name = site$name, url = site$url)", f)
  writeLines("}", f)
  writeLines("result <- master$run()", f)
  writeLines("print(master$summary())", f)
  close(f)
  TRUE
}

#' Instantiate an noncooperating party
#' @param ncp_defn the NCP definition
#' @param comp_defn the computation definition
#' @param sites a list of sites each entry a named list of name, url, worker
#' @param pubkey_bits number of bits for public key
#' @param pubkey_n the n for the public key
#' @param den_bits the log to base 2 of the denominator
#' @return an NCP object
#' @export
makeNCP  <- function(ncp_defn, comp_defn, sites = list(), pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
    NCP$new(ncp_defn = ncp_defn, comp_defn = comp_defn, sites = sites, pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)
}


#' Instantiate a master process for HE operations
#' @param defn the computation definition
#' @return an master object for HE operations
#' @export
makeHEMaster  <- function(defn) {
    HEMaster$new(defn)
}


