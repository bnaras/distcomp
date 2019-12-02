#' Create a HEMaster process for use in a distributed homomorphic
#' computation
#'
#' @description `HEMaster` objects run a distributed computation based
#'     upon a definition file that encapsulates all information
#'     necessary to perform a computation. A master makes use of two
#'     non-cooperating parties which communicate with sites that
#'     perform the actual computations using local data.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @seealso [NCP()]
#' @section Methods:
#' \describe{
#'   \item{`HEMaster$new(defn)`}{Create a new `HEMaster` instance using
#'         definition provided}
#'   \item{`addNCP(url, ncpWorker)`}{Add a non-cooperating party using either the `url` provided or a `ncpWorker` object of class `NCP`. The latter is for prototyping locally using created objects and so exactly one of the two has to be non-null}
#'   \item{`run()`}{Run a computation}
#' }
#'
#' @importFrom gmp as.bigz as.bigq
#' @export
#' @format An [R6::R6Class()] generator object
HEMaster  <-
    R6::R6Class(
            "HEMaster",
            private = list(
                ## the definition
                defn = NA,
                ## dry run (i.e. not web calls!)
                dry_run = FALSE,
                ## Private and public keys
                keys = NA,
                ## Non cooperating party list
                nc_party = list()
            ),
            public = list(
                ## Denominator for rational arithmetic
                den  = NA,
                den_bits = 256L,
                initialize = function(defn) {
                    private$defn  <- defn
                    private$keys <- homomorpheR::PaillierKeyPair$new(1024) ## Generate new public and private key.
                    self$den <- gmp::as.bigq(2)^(self$den_bits)  #Our denominator for rational approximations
                },
                getNC_party = function() private$nc_party,
                getPubkey = function() {
                    private$keys$pubkey
                },
                addNCP  = function(ncp_defn, url = NULL, ncpWorker = NULL) {
                    'Add an NCP identified by url or ncpWorker'
                    ## Only one of url/ncpWorker should be non-null
                    stopifnot((is.null(url) + is.null(ncpWorker)) == 1)
                    n  <- length(private$nc_party)
                    if (n == 2) stop("Two NC parties already added!")
                    ## Mixing url and ncpWorker parties not allowed for now
                    if (private$dry_run && !is.null(url)) stop("Mixing local and remote NCP not allowed!")
                    name  <- ncp_defn$name
                    number <- ncp_defn$number
                    if (!(number %in% c(1, 2))) stop("Party number should be 1 or 2!")
                    if (ncp_defn$id != private$defn$id) stop("NC Party id does not match computation defn id!")
                    config <- getConfig()

                    if (is.null(url)) {
                        private$dry_run <- TRUE
                        private$nc_party[[number]]  <- list(defn = ncp_defn,
                                                            worker = ncpWorker)
                    } else {
                        localhost <- (grepl("^http://localhost", url) ||
                                      grepl("^http://127.0.0.1", url))
                        private$nc_party[[number]]  <- list(defn = ncp_defn,
                                                            url = url,
                                                            dataFileName = if (localhost) paste(name, config$dataFileName, sep = "-") else NULL)
                    }
                },
                ## run method
                run = function(debug = FALSE) {
                    nc_party  <- private$nc_party
                    dry_run <- private$dry_run
                    defn <- private$defn
                    if (debug) {
                        print("run(): checking object object creation")
                    }
                    pubkey  <- private$keys$pubkey
                    pubkey_n  <- pubkey$n
                    pubkey_bits  <- pubkey$bits
                    den_bits  <- self$den_bits
                    ## Generate a token for ensuring an appropriate result pair
                    token  <- generateId(object=list(Sys.time(), self))

                    if (dry_run) {
                        ## NCP workers have already been instantiated, so configure them
                        workers  <- lapply(private$nc_party, function(x) x$worker)
                        ## Send an indentifying number, pubkey and denominator
                        workers[[1L]]$setParams(pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)
                        workers[[2L]]$setParams(pubkey_bits = pubkey_bits, pubkey_n = pubkey_n, den_bits = den_bits)
                        ## Ask each worker to run the computation
                        result1  <- workers[[1L]]$run(token)
                        result2  <- workers[[2L]]$run(token)
                        list(result1, result2)
                    } else {
                        ## Create an instance id to use for this run
                        instanceId <- generateId(object=list(Sys.time(), self))
                        ## Function to create NCP objects remotely
                        ## To minimize requests, we send everything in one request
                        makeNCP  <- function(ncp) {
                            payload <- list(name = ncp$name, ncpId = ncp$id,
                                            instanceId = instanceId,
                                            pubkey_bits = pubkey_bits, pubkey_n = pubkey_n,
                                            den_bits = den_bits,
                                            dataFileName = ncp$dataFileName)
                            q <- POST(url = .makeOpencpuURL(urlPrefix=ncp$url, fn="createNCPInstance"),
                                      body = toJSON(payload),
                                      add_headers("Content-Type" = "application/json"),
                                      config=getConfig()$sslConfig
                                      )
                            .deSerialize(q)
                        }
                        ncp1_ok <- makeNCP(nc_party[[1L]])
                        ncp2_ok  <- makeNCP(nc_party[[2L]])
                        if ((!ncp1_ok) || (!ncp2_ok)) {
                            stop("makeNCP:  Either or both NCP did not respond successfully!")
                        }
                        ## Now run the computation on each NCP
                        runNCP  <- function(objectId, ncp, token) {
                            payload <- list(objectId = objectId, method = "run", token = token)
                            q <- POST(.makeOpencpuURL(urlPrefix=ncp$url, fn="executeMethod"),
                                      body = toJSON(payload),
                                      add_headers("Content-Type" = "application/json"),
                                      config=getConfig()$sslConfig
                                      )
                            .deSerialize(q)
                        }
                        ## Each result is list of int part and a fractional part
                        result1  <- runNCP(objectId = instanceId, ncp = nc_party[[1L]], token = token)
                        result2  <- runNCP(objectId = instanceId, url = nc_party[[2L]], token = token)
                        list(result1, result2)
                    }
                    ## Accumulate the integer and fractional parts across NCPs
                    sumInt  <- pubkey$add(result1$int, result2$int)
                    ## ignore frac
                    ## sumFrac  <- pubkey$add(result1$frac, result2$frac)
                    privkey  <- private$keys$getPrivateKey()
                    intResult <- as.double(privkey$decrypt(sumInt))
                    ## ignore frac
                    ## fracResult <- as.double(gmp::as.bigq(privkey$decrypt(sumFrac)) / self$den)

                    if (!dry_run) {
                        if (debug) {
                            print("run(): checking worker object cleanup")
                        }
                        sitesOK <- sapply(nc_party,
                                          function(x) {
                                              payload <- list(instanceId = instanceId)
                                              q <- POST(url = .makeOpencpuURL(urlPrefix=x$url, fn="destroyInstanceObject"),
                                                        body = toJSON(payload),
                                                        add_headers("Content-Type" = "application/json"),
                                                        config=getConfig()$sslConfig
                                                        )
                                              .deSerialize(q)
                                          })
                        if (!all(sitesOK)) {
                            warning("run():  Some sites did not clean up successfully!")
                        }
                    }

                    ## Since we get twice the result, we divide by 2.
                    intResult / 2
                }
            )
        )
