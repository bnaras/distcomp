#' R6 object to use as non-cooperating party in a distributed homomorphic computation
#'
#' @description `NCP` objects are worker objects that separate a
#'     master process from communicating directly with the worker
#'     processes. Typically two such are needed for a distributed
#'     homomorphic computation. A master process can communicate with
#'     `NCP` objects and the `NCP` objects can communicate
#'     with worker processes. However, the two `NCP` objects,
#'     designated by numbers 1 and 2, are non-cooperating in the sense
#'     that they don't communicate with each other and are isolated
#'     from each other.
#'
#' @importFrom homomorpheR PaillierPublicKey
#' @export
NCP <-
    R6::R6Class(
            "NCP",

            private = list(
                ## name the noncooperating party name
                name = NA,

                ## number the party number
                number = NA,

                ## stateful a flag indicating if the object is stateful
                stateful = FALSE,

                ## defn the computation defn
                defn = NA,

                ## dry_run flag indicating if a dry run is in effect
                dry_run = FALSE,

                ## sites the list of sites
                sites = list()
            ),

            public = list(

                #' @field pubkey the master's public key visible to everyone
                pubkey = NA,

                #' @field pubkey_bits the number of bits in the public key (used for reconstructing public key remotely by serializing to character)
                pubkey_bits = NA,

                #' @field pubkey_n the `n` for the public key used for reconstructing public key remotely
                pubkey_n  = NA,

                #' @field den the denominator for rational arithmetic
                den = NA,

                #' @field den_bits the number of bits in the denominator used for reconstructing denominator remotely
                den_bits = NA,

                #' @description
                #' Create a new `NCP` object.
                #' @param ncp_defn the NCP definition; see example
                #' @param comp_defn the computation definition
                #' @param sites list of sites
                #' @param pubkey_bits the number of bits in public key
                #' @param pubkey_n the `n` for the public key
                #' @param den_bits the number of bits in the denominator (power of 2) used in rational approximations
                #' @return a new `NCP` object
                initialize = function(ncp_defn, comp_defn, sites = list(), pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
                    private$name  <- ncp_defn$name
                    private$number  <- ncp_defn$number
                    private$defn  <- comp_defn
                    self$pubkey_bits  <- pubkey_bits
                    self$pubkey_n  <- pubkey_n
                    self$den_bits  <- den_bits
                    if (!is.null(pubkey_bits)) {
                        self$pubkey  <- homomorpheR::PaillierPublicKey$new(pubkey_bits, pubkey_n)
                        self$den <- gmp::as.bigq(2)^(den_bits)  #Our denominator for rational approximations
                    }
                    private$sites  <- sites
                },

                #' @description
                #' Retrieve the value of the `stateful` field
                getStateful = function() {
                    private$stateful
                },

                #' @description
                #' Set some parameters of the `NCP` object for homomorphic computations
                #' @param pubkey_bits the number of bits in public key
                #' @param pubkey_n the `n` for the public key
                #' @param den_bits the number of bits in the denominator (power of 2) used in rational approximations
                setParams = function(pubkey_bits, pubkey_n, den_bits) {
                    self$pubkey_bits  <- pubkey_bits
                    self$pubkey_n  <- pubkey_n
                    self$den_bits  <- den_bits
                    self$pubkey  <- homomorpheR::PaillierPublicKey$new(pubkey_bits, pubkey_n)
                    self$den <- gmp::as.bigq(2)^(den_bits)  #Our denominator for rational approximations
                    invisible(TRUE)
                },

                #' @description
                #' Retrieve the value of the private `sites` field
                getSites = function() {
                    private$sites
                },

                #' @description
                #' Set the value of the private `sites` field
                #' @param sites the list of sites
                setSites = function(sites) {
                    private$sites  <- sites
                },

                #' @description
                #' Add a url or worker object for a site for participating in the distributed computation. The worker object can be used to avoid complications in debugging remote calls during prototyping.
                #' @param name of the site
                #' @param url web url of the site; exactly one of `url` or `worker` should be specified
                #' @param worker worker object for the site; exactly one of `url` or `worker` should be specified
                addSite = function(name, url = NULL, worker = NULL) {
                    'Add a site identified by url with a name'
                    ## Only one of url/worker should be non-null
                    stopifnot((is.null(url) + is.null(worker)) == 1)
                    sites <- private$sites
                    n <- length(sites)
                    ## Mixing url and local workers not allowed for now
                    if (private$dry_run && !is.null(url)) stop("Mixing local objects and remote workers not allowed!")
                    count  <- n + 1L
                    if (is.null(url)) {
                        private$dry_run <- TRUE
                        sites[[count]] <- list(name = name, worker = worker)
                    } else {
                        sites[[count]] <- list(name = name, url = url)
                    }
                    private$sites <- sites
                },

                #' @description
                #' Clean up by destroying instance objects created in workspace.
                #' @param token the token for the instance
                cleanupInstance = function(token) {
                    if (!private$dry_run) {
                        if (debug) {
                            print("run(): checking worker object cleanup")
                        }
                        sites  <- private$sites
                        sitesOK <- sapply(sites,
                                          function(x) {
                                              ## The instanceId is simply the token
                                              payload <- list(instanceId = token)
                                              q <- POST(url = .makeOpencpuURL(urlPrefix=x$url, fn="destroyInstanceObject"),
                                                        body = jsonlite::toJSON(payload),
                                                        add_headers("Content-Type" = "application/json"),
                                                        config=getConfig()$sslConfig
                                                        )
                                              .deSerialize(q)
                                          })
                        if (!all(sitesOK)) {
                            warning("run():  Some sites did not clean up successfully!")
                        }
                    }
                    invisble(TRUE)
                },

                #' @description
                #' Run the distributed homomorphic computation
                #' @param token a unique token for the run, used to ensure that correct parts of cached results are returned appropriately
                #' @return the result of the computation
                run = function(token) {
                    ## The token is generated by the process that calls this NCP
                    ## This master is always in memory!!! So no need to worry about
                    ## individual function calls: they all happen in the same process
                    cat("in main run; creating master\n")

                    comptypeMaster  <- makeMaster(defn = private$defn, partyNumber = private$number)
                    cat("created comptype master\n")
                    comptypeMaster$setParams(pubkey_bits = self$pubkey_bits, pubkey_n = self$pubkey_n, den_bits = self$den_bits)
                    cat("set comptype params\n")
                    sites  <- private$sites
                    cat("adding sites\n")
                    for (site in private$sites) {
                        comptypeMaster$addSite(name = site$name, url = site$url, worker = site$worker)
                    }
                    cat("done adding sites; calling comptype master run\n")
                    result  <- comptypeMaster$run(token)

                    ## If this NCP is the second party, which will
                    ## bring up the rear in our case send sites the
                    ## message to cleanup, or else get wrong results
                    ## due to caching.
                    if (private$number == 2) comptypeMaster$cleanup()
                    result
                }
            )
        )

