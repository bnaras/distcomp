#' Create a non-cooperating object for use in a distributed homomorphic computation
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
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#' \describe{
#'   \item{`NCP$new(name, defn)`}{Create a new `NCP` instance with a name and a computation definition}
#'   \item{`setParams(pubkey, den)`}{Set a public key, and a denominator for rational approximations of fractional parts of real numbers}
#'   \item{`addSite(site)`}{Register a site for a computation}
#' }
#' @importFrom homomorpheR PaillierPublicKey
#' @export
#' @format An [R6::R6Class()] generator object
NCP <-
    R6::R6Class(
            "NCP",
            private = list(
                ## party number
                number = NA,
                ## stateful
                stateful = FALSE,
                ## defn
                defn = NA,
                ## dry_run or not
                dry_run = FALSE,
                ## The sites
                sites = list()
            ),
            public = list(
                ## The master's public key; everyone has this
                pubkey = NA,
                pubkey_bits = NA,
                pubkey_n  = NA,
                ## The denoinator for rational arithmetic
                den = NA,
                den_bits = NA,
                initialize = function(number, defn, pubkey_bits = NULL, pubkey_n = NULL, den_bits = NULL) {
                    private$number  <- number
                    private$defn  <- defn
                    self$pubkey_bits  <- pubkey_bits
                    self$pubkey_n  <- pubkey_n
                    self$den_bits  <- den_bits
                    if (!is.null(pubkey_bits)) {
                        self$pubkey  <- homomorpheR::PaillierPublicKey$new(pubkey_bits, pubkey_n)
                        self$den <- gmp::as.bigq(2)^(den_bits)  #Our denominator for rational approximations
                    }
                },  getStateful = function() {
                    private$stateful
                },
                setParams = function(number, pubkey_bits, pubkey_n, den_bits) {
                    private$number  <- number
                    self$pubkey_bits  <- pubkey_bits
                    self$pubkey_n  <- pubkey_n
                    self$den_bits  <- den_bits
                    self$pubkey  <- homomorpheR::PaillierPublicKey$new(pubkey_bits, pubkey_n)
                    self$den <- gmp::as.bigq(2)^(den_bits)  #Our denominator for rational approximations
                    TRUE
                },
                getSites = function() {
                    private$sites
                },
                addSite = function(name, url = NULL, worker = NULL) {
                    'Add a site identified by url with a name'
                    ## Only one of url/worker should be non-null
                    stopifnot((is.null(url) + is.null(worker)) == 1)
                    n <- length(private$sites)
                    ## Mixing url and local workers not allowed for now
                    if (private$dry_run && !is.null(url)) stop("Mixing local objects and remote workers not allowed!")
                    count  <- n + 1L
                    if (is.null(url)) {
                        private$dry_run <- TRUE
                        private$sites[[count]] <- list(name = name, worker = worker)
                    } else {
                        localhost <- (grepl("^http://localhost", url) ||
                                      grepl("^http://127.0.0.1", url))
                        private$sites[[count]] <- list(name = name, url = url,
                                                       localhost = localhost,
                                                       dataFileName = if (localhost) paste0(name, ".rds") else NULL)
                    }
                },
                ## Run a computation
                run = function(token) {
                    ## This master is always in memory!!! So no need to worry about
                    ## individual function calls: they all happen in the same process
                    comptypeMaster  <- makeMaster(defn = private$defn, partyNumber = private$number)
                    comptypeMaster$setParams(pubkey_bits = self$pubkey_bits, pubkey_n = self$pubkey_n, den_bits = self$den_bits)
                    sites  <- private$sites
                    for (site in private$sites) {
                        comptypeMaster$addSite(name = site$name, url = site$url, worker = site$worker)
                    }
                    result  <- comptypeMaster$run(token)
                    result
                }
            )
        )

