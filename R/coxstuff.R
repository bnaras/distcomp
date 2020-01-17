#' R6 class for object to use as a worker with `CoxMaster` master objects
#'
#' @description `CoxWorker` objects are worker objects at each data site of
#' a distributed Cox model computation
#'
#' @seealso `CoxMaster` which goes hand-in-hand with this object
#' @importFrom R6 R6Class
#' @export
CoxWorker <- R6::R6Class(
    "CoxWorker",

    private = list(
        defn = NA
      , stateful = TRUE
      , fitter = NA
      , p = NA
      , result = list()
    ),

    public = list(

        #' @description
        #' Create a new `CoxWorker` object.
        #' @param defn the computation definition
        #' @param data the local data
        #' @param stateful a boolean flag indicating if state needs to be preserved between REST calls
        #' @return a new `CoxWorker` object
        initialize = function(defn, data, stateful = TRUE) {
            private$defn <- defn
            private$stateful <- stateful
            result <- dccoxph(formula = as.formula(defn$formula), data = data)
            private$fitter <- result$fitter
            private$p <- result$p
            stopifnot(self$kosher())
        },

        #' @description
        #' Return the dimension of the parameter vector.
        #' @param ... other args ignored
        #' @return the dimension of the parameter vector
        getP = function(...) {
            private$p
        },

        #' @description
        #' Return the stateful status of the object.
        #' @return the stateful flag, `TRUE` or `FALSE`
        getStateful = function() {
            private$stateful
        },

        #' @description
        #' Return the partial log likelihood on local data for given `beta` parameter.
        #' @param beta the parameter vector
        #' @param ... further arguments, currently unused
        #' @return a named list with three components: `value` contains the value of the
        #' log likelihood, `gradient` contains the score vector, and `hessian` contains
        #' the estimated hessian matrix
        logLik = function(beta, ...) {
            fit <- private$fitter(beta)
            list(value = fit$loglik[1], gradient=fit$gradient, hessian=-solve(fit$var))
        },

        #' @description
        #' Return the variance of estimate for given `beta` parameter on local data.
        #' @param beta the parameter vector
        #' @param ... further arguments, currently unused
        #' @return variance vector
        var = function(beta, ...) {
            private$fitter(beta)$var
        },

        #' @description
        #' Check if inputs and state of object are sane. For future use
        #' @return `TRUE` or `FALSE`
        kosher = function() {
            ## add sanity checks
            TRUE
        }
    )
)

#' Create a master object to control `CoxWorker` worker objects
#' @description `CoxMaster` objects instantiate and run a distributed Cox model
#' computation fit
#'
#' @seealso `CoxWorker` which generates objects matched to such a master object
#' @importFrom R6 R6Class
#' @export
CoxMaster <- R6::R6Class(
    "CoxMaster",

    private = list(
        defn = NA
        , dry_run = FALSE,
        sites = list()
        , p = NA
      , mapFn = function(site, beta) {
          payload <- list(objectId = site$instanceId,
                          method = "logLik",
                          beta = beta)
          q <- POST(.makeOpencpuURL(urlPrefix=site$url, fn="executeMethod"),
                    body = jsonlite::toJSON(payload),
                    add_headers("Content-Type" = "application/json"),
                    config=getConfig()$sslConfig
                    )
          ## Should really examine result here.
          .deSerialize(q)
      }, result = list()
       , debug = FALSE
    ),

    public = list(

        #' @description
        #' `CoxMaster` objects instantiate and run a distributed Cox model
        #' computation fit
        #' @param defn a computation definition
        #' @param debug a flag for debugging, default `FALSE`
        #' @return R6 `CoxMaster` object
        initialize = function(defn, debug = FALSE) {
            'Initialize the object with a defn and flag'
            private$defn <- defn
            private$debug <- debug
            stopifnot(self$kosher())
        },

        #' @description
        #' Check if inputs and state of object are sane. For future use
        #' @return `TRUE` or `FALSE`
        kosher = function() {
            ' Check for sanity'
            TRUE
        },

        #' @description
        #' Return the partial log likelihood on all data for given `beta` parameter.
        #' @param beta the parameter vector
        #' @return a named list with three components: `value` contains the value of the
        #' log likelihood, `gradient` contains the score vector, and `hessian` contains
        #' the estimated hessian matrix
        logLik = function(beta) {
            'Compute the (partial) log-likelihood on all data'
            debug <- private$debug
            if (debug) {
                print("beta")
                print(beta)
            }
            sites <- private$sites
            n <- length(sites)
            if (private$dry_run) {
                mapFn <- function(x, arg) x$worker$logLik(arg)
            } else {
                mapFn <- private$mapFn
            }
            results <- Map(mapFn, sites, rep(list(beta), n))
            value <- Reduce(f = sum, lapply(results, function(x, y) x[[y]], y = "value"))
            gradient <- Reduce(f = '+', lapply(results, function(x, y) x[[y]], y = "gradient"))
            hessian <- Reduce(f = '+', lapply(results, function(x, y) x[[y]], y = "hessian"))
            attr(value, "gradient") <- gradient
            attr(value, "hessian") <- hessian
            if (debug) {
                print("value")
                print(value)
            }
            value
        },

        #' @description
        #' Add a url or worker object for a site for participating in the distributed computation. The worker object can be used to avoid complications in debugging remote calls during prototyping.
        #' @param name of the site
        #' @param url web url of the site; exactly one of `url` or `worker` should be specified
        #' @param worker worker object for the site; exactly one of `url` or `worker` should be specified
        addSite = function(name, url = NULL, worker = NULL) {
            'Add a site identified by url with a name'
            ## critical section start
            ## This is the time to cache "p" and check it
            ## against all added sites
            ## Only one of url/worker should be non-null
            stopifnot(is.null(url) || is.null(worker))
            n <- length(private$sites)
            if (is.null(url)) {
                private$dry_run <- private$dry_run || TRUE
                private$sites[[n+1]] <- list(name = name, worker = worker)
            } else {
                localhost <- (grepl("^http://localhost", url) ||
                              grepl("^http://127.0.0.1", url))
                private$sites[[n+1]] <- list(name = name, url = url,
                                             localhost = localhost,
                                             dataFileName = if (localhost) paste0(name, ".rds") else NULL)
            }
            ## critical section end
        },

        #' @description
        #' Run the distributed Cox model fit and return the estimates
        #' @param control parameters, same as `survival::coxph.control()`
        #' @return a named list of `beta`, `var`, `gradient`, `iter`, and `returnCode`
        run = function(control = coxph.control()) {
            'Run estimation'
            dry_run <- private$dry_run
            debug <- private$debug
            defn <- private$defn
            if (debug) {
                print("run(): checking worker object creation")
            }
            if (dry_run) {
                ## Workers have already been created and passed
                sites <- private$sites
                pVals <- sapply(sites, function(x) x$worker$getP())
            } else {
                ## Create an instance Id
                instanceId <- generateId(object=list(Sys.time(), self))
                ## Augment each site with object instance ids
                private$sites <- sites <-
                    lapply(private$sites,
                           function(x) list(name = x$name,
                                            url = x$url,
                                            localhost = x$localhost,
                                            dataFileName = x$dataFileName,
                                            instanceId = if (x$localhost) x$name else instanceId))
                ## Create instance objects
                sitesOK <- sapply(
                    sites,
                    function(x) {
                        payload <- if (is.null(x$dataFileName)) {
                                       list(defnId = defn$id, instanceId = x$instanceId)
                                   } else {
                                       list(defnId = defn$id, instanceId = x$instanceId,
                                            dataFileName = x$dataFileName)
                                   }
                        q <- POST(url = .makeOpencpuURL(urlPrefix=x$url, fn="createWorkerInstance"),
                                  body = jsonlite::toJSON(payload),
                                  add_headers("Content-Type" = "application/json"),
                                  config=getConfig()$sslConfig
                                  )
                        .deSerialize(q)
                    })

                ## I am not checking the value of p here; I do it later below
                if (!all(sitesOK)) {
                    warning("run():  Some sites did not respond successfully!")
                    private$sites <- sites <- sites[which(sitesOK)]  ## Only use sites that created objects successfully.
                }
                ## stop if no sites
                if (debug) {
                    print("run(): checking p")
                }
                pVals <- sapply(
                    sites,
                    function(x) {
                        payload <- list(objectId = x$instanceId, method = "getP")
                        q <- POST(.makeOpencpuURL(urlPrefix=x$url, fn="executeMethod"),
                                  body = jsonlite::toJSON(payload),
                                  add_headers("Content-Type" = "application/json"),
                                  config=getConfig()$sslConfig
                                  )
                        .deSerialize(q)
                    })
            }
            if (debug) {
                print(pVals)
            }
            if (any(pVals != pVals[1])) {
                stop("run(): Heterogeneous sites! Stopping!")
            }
            p <- pVals[1]
            if (debug) {
                print(paste("p is ", p))
            }

            ## DO Newton-Raphson
            ##control <- coxph.control()
            prevBeta <- beta <- rep(0, p)
            m <- prevloglik <- self$logLik(beta)
            iter <- 0
            returnCode <- 0
            repeat {
                beta <- beta - solve(attr(m, "hessian")) %*% attr(m, "gradient")
                iter <- iter + 1
                m <- self$logLik(beta)
                if (abs(m - prevloglik) < control$eps) {
                    break
                }
                if (iter >= control$iter.max) {
                    returnCode <- 1
                    break
                }
                prevBeta <- beta
                prevloglik <- m
                if (debug) {
                    print(beta)
                }
            }
            private$result <- result <- list(beta = beta,
                                             var = -solve(attr(m, "hessian")),
                                             gradient = attr(m, "gradient"),
                                             iter = iter,
                                             returnCode = returnCode)

            if (!dry_run) {
                if (debug) {
                    print("run(): checking worker object cleanup")
                }
                sitesOK <- sapply(
                    sites,
                    function(x) {
                        payload <- list(instanceId = x$instanceId)
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
            ## Should probably clean up sites list and remove instanceId too...
            result
        },

        #' #' @description ' Return the summary of fit as a data frame
        #' @return a summary data frame columns for `coef`,
        #' `exp(coef)`, ' standard error, z-score, and p-value for each
        #' parameter in the model following the same format as the
        #' `survival` package
        summary = function() {
            'Run the summary'
            result <- private$result
            if (length(result) == 0) {
                warning("Calculating...")
                run()
                result <- private$result
            }

            d <- as.data.frame(t(sapply(seq.int(length(result$beta)),
                                        function(i) {
                                            coef <- result$beta[i]
                                            eCoef <- exp(coef)
                                            se <- sqrt(result$var[i, i])
                                            z <- coef / se
                                            pValue <- 2*pnorm(z, lower.tail=(z <= 0))
                                            c("coef"=coef, "exp(coef)"=eCoef, "se(coef)"=se, z=z, p=pValue)
                                        })))
            d
        }
    )
)


