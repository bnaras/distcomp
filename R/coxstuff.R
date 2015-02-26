coxSlave <- R6Class("CoxSlave",
                    portable = FALSE,
                    private = list(
                        stateful = TRUE,
                        fitter = NA,
                        p = NA,
                        result = list()
                    ),
                    public = list(
                        initialize = function(formula, data, stateful=TRUE) {
                            stateful <<- stateful
                            result <- dccoxph(formula = as.formula(formula), data = data)
                            fitter <<- result$fitter
                            p <<- result$p
                            stopifnot(kosher())
                        },
                        getP = function(...) p,
                        getResult = function() result,
                        getStateful = function() stateful,
                        logLik = function(beta, ...) {
                            fit <- fitter(beta)
                            value <- fit$loglik[1]
                            gradient <- fit$gradient
                            hessian <- -solve(fit$var)
                            list(value = value, gradient=gradient, hessian=hessian)
                        },
                        var = function(beta, ...) {
                            fitter(beta)$var
                        },
                        kosher = function() {
                            ## add sanity checks
                            TRUE
                        })
                    )

coxMaster <- R6Class("CoxMaster",
                     portable = FALSE,
                     private = list(
                         defnId = NA,
                         instanceId = NA,
                         formula = NA,
                         sites = list(),
                         p = NA,
                         localServer = FALSE,
                         mapFn = function(site, id, beta) {
                             payload <- list(objectId = id,
                                             method = "logLik",
                                             beta = beta)
                             q <- POST(.makeOpencpuURL(urlPrefix=site$url, fn="executeMethod"),
                                       body = toJSON(payload),
                                       add_headers("Content-Type" = "application/json"),
                                       config=getConfig()$sslConfig
                                       )
                             ## Should really examine result here.
                             .deSerialize(q)
                         },
                         result = list(),
                         debug = FALSE
                     ),
                     public = list(
                         initialize = function(defnId, formula, debug=FALSE, localServer=FALSE) {
                             'Initialize the object with a formula and a dataset'
                             defnId <<- defnId
                             formula <<- formula
                             debug <<- debug
                             localServer <<- localServer
                             stopifnot(self$kosher())
                         },
                         kosher = function() {
                             ' Check for sanity'
                             TRUE
                         },
                         logLik = function(beta) {
                             'Compute the (partial) log-likelihood on all data'
                             if (debug) {
                                 print("beta")
                                 print(beta)
                             }
                             n <- length(sites)
                             if (localServer) {
                                 results <- Map(mapFn, sites, lapply(sites, function(x) x$name),
                                                rep(list(beta), n))

                             } else {
                                 results <- Map(mapFn, sites, rep(list(private$instanceId), n),
                                                rep(list(beta), n))
                             }
                             ##results <- lapply(private$sites, private$mapFn, beta)
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
                         addSite = function(name, url, dataFileName=getConfig()$dataFileName) {
                             'Add a site identified by url with a name'
                             ## critical section start
                             ## This is the time to cache "p" and check it
                             ## against all added sites
                             n <- length(sites)
                             sites[[n+1]] <<- list(name=name, url=url, dataFileName=dataFileName)
                             ## critical section end
                         },
                         run = function() {
                             'Run estimation'
                             ## Create an instance Id
                             instanceId <<- generateId(object=list(Sys.time(), self))
                             if (debug) {
                                 print("run(): checking slave object creation")
                             }
                             sitesOK <- sapply(sites,
                                               function(x) {
                                                   payload <- list(defnId = defnId,
                                                                   instanceId = ifelse(localServer, x$name, instanceId),
                                                                   dataFileName = ifelse(localServer, x$dataFileName, getConfig()$dataFileName))
                                                   q <- POST(url = .makeOpencpuURL(urlPrefix=x$url, fn="createInstanceObject"),
                                                             body = toJSON(payload),
                                                             add_headers("Content-Type" = "application/json"),
                                                             config=getConfig()$sslConfig
                                                             )
                                                   .deSerialize(q)
                                               })

                             ## I am not checking the value of p here; I do it later below
                             if (!all(sitesOK)) {
                                 warning("run():  Some sites did not respond successfully!")
                                 sites <- sites[which(sitesOK)]  ## Only use sites that created objects successfully.
                             }
                             ## stop if no sites
                             if (debug) {
                                 print("run(): checking p")
                             }
                             pVals <- sapply(sites,
                                             function(x) {
                                                 payload <- list(objectId = ifelse(localServer, x$name, instanceId),
                                                                 method = "getP")
                                                 q <- POST(.makeOpencpuURL(urlPrefix=x$url, fn="executeMethod"),
                                                           body = toJSON(payload),
                                                           add_headers("Content-Type" = "application/json"),
                                                           config=getConfig()$sslConfig
                                                           )
                                                 .deSerialize(q)
                                             })
                             if (debug) {
                                 print(pVals)
                             }
                             if (any(pVals != pVals[1])) {
                                 stop("run(): Heterogeneous sites! Stopping!")
                             }
                             p <<- pVals[1]
                             if (debug) {
                                 print(paste("p is ", p))
                             }

                             ## DO Newton-Raphson
                             control <- coxph.control()
                             prevBeta <- beta <- rep(0, p)
                             m <- prevloglik <- logLik(beta)
                             iter <- 0
                             returnCode <- 0
                             repeat {
                                 beta <- beta - solve(attr(m, "hessian")) %*% attr(m, "gradient")
                                 iter <- iter + 1
                                 m <- logLik(beta)
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
                             result <<- list(beta = beta,
                                             var = -solve(attr(m, "hessian")),
                                             gradient = attr(m, "gradient"),
                                             iter = iter,
                                             returnCode = returnCode)

                             if (debug) {
                                 print("run(): checking slave object cleanup")
                             }
                             sitesOK <- sapply(sites,
                                               function(x) {
                                                   payload <- list(instanceId = ifelse(localServer, x$name, instanceId))
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
                             result
                         },
                         summary = function() {
                             'Run the summary'

                             if (length(result) == 0) {
                                 warning("Calculating...")
                                 run()
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
                     ))

