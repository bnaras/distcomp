svdSlave <- R6Class("SVDSlave",
                    portable=FALSE,
                    private = list(
                        stateful = TRUE,
                        x = NA,
                        u = NA,
                        p = NA,
                        workX = NA
                    ),
                    public = list(
                        initialize = function(x, stateful=TRUE) {
                            x <<- workX <<- x
                            u <<- rep(1, nrow(x))
                            stateful <<- stateful
                            stopifnot(kosher())
                        },
                        reset = function(){
                            workX <<- x
                            u <<- rep(1, nrow(x))
                        },
                        dimX = function(...) dim(x),
                        updateV = function(arg, ...) {
                            t(workX) %*% u / arg
                        },
                        updateU = function(arg, ...) {
                            u <<- as.numeric(workX %*% arg)
                            sum(u^2)
                        },
                        normU = function(arg, ...) {
                            u <<- u / arg
                            NULL
                        },
                        fixU = function(arg, ...) {
                            workX <<- workX - u%*%t(arg)
                        },
                        getn = function() {
                            nrow(x)
                        },
                        getP = function() {
                            ncol(x)
                        },
                        getX = function() x,
                        getStateful = function() stateful,
                        kosher = function() {
                            TRUE
                        })
                    )


svdMaster <- R6Class("SVDMaster",
                     portable = FALSE,
                     private = list(
                         defnId = NA,
                         instanceId = NA,
                         sites = list(),
                         dimX = NA,
                         localServer = FALSE,
                         mapFn =  function(site, id, arg, method) {
                             payload <- list(objectId = id,
                                             method = method,
                                             arg = arg)
                             q <- POST(.makeOpencpuURL(urlPrefix=site$url, fn="executeMethod"),
                                       body = toJSON(payload),
                                       add_headers("Content-Type" = "application/json"),
                                       config = getConfig()$sslConfig
                                       )
                             ## Should really examine result here.
                             .deSerialize(q)
                         },
                         result = list(),
                         debug = FALSE
                     ),
                     public = list(
                         initialize = function(defnId, debug=FALSE, localServer=FALSE) {
                             'Initialize the object with a dataset'
                             defnId <<- defnId
                             debug <<- debug
                             localServer <<- localServer
                             stopifnot(self$kosher())
                         },
                         kosher = function() {
                             ' Check for appropriateness'
                             TRUE
                         },
                         updateV = function(arg) { ## Here arg is a list of right size already
                             'Compute or Update VD'
                             n <- length(sites)
                             if (localServer) {
                                 results <- Map(mapFn, sites, lapply(sites, function(x) x$name),
                                                rep(arg, n), rep(list("updateV"), n))
                             } else {
                                 results <- Map(mapFn, sites, rep(list(id), n),
                                                rep(arg, n), rep(list("updateV"), n))
                             }
                             ##results <- lapply(sites, mapFn, beta)
                             vd <- Reduce(f = '+', results)
                             vd / sqrt(sum(vd^2))
                         },
                         updateU = function(arg) { ## arg is a single vector
                             'Compute/Update U'
                             n <- length(sites)
                             if (localServer) {
                                 results <- Map(mapFn, sites, lapply(sites, function(x) x$name),
                                                rep(list(arg), n), rep(list("updateU"), n))
                             } else {
                                 results <- Map(mapFn, sites, rep(list(id), n),
                                                rep(list(arg), n), rep(list("updateU"), n))
                             }
                             uNorm <- sqrt(Reduce(f = '+', results))
                         },
                         fixFit = function(v, d) { ## arg is a single vector
                             'Compute/Update U'
                             result$v <<- cbind(result$v, v)
                             result$d <<- c(result$d, d)
                             n <- length(sites)
                             if (localServer) {
                                 results <- Map(mapFn, sites, lapply(sites, function(x) x$name),
                                                rep(list(v), n), rep(list("fixU"), n))
                             } else {
                                 results <- Map(mapFn, sites, rep(list(id), n),
                                                rep(list(v), n), rep(list("fixU"), n))
                             }
                         },
                         reset = function() {
                             arg = "THISISUNUSED"
                             n <- length(sites)
                             if (localServer) {
                                 results <- Map(mapFn, sites, lapply(sites, function(x) x$name),
                                                rep(list(arg), n), rep(list("reset"), n))
                             } else {
                                 results <- Map(mapFn, sites, rep(list(id), n),
                                                rep(list(arg), n), rep(list("reset"), n))
                             }
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
                         run = function(k = 1, thr = 1e-8, max.iter = 100) {
                             result <<- list()
                             'Run Calculation'
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
                                                 payload <- list(objectId = ifelse(localServer, x$name,
                                                                     instanceId), method = "getP")
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
                             p <- pVals[1]
                             if (debug) {
                                 print(paste("p is ", p))
                             }

                             n <- length(sites)
                             stopifnot(n > 1)
                             returnCode <- 0
                             reset()
                             for(j in 1:k) {
                                 v <-  rep(1, p)
                                 vold <- rep(0, p)
                                 for (i in seq.int(max.iter)) {
                                     unorm <- updateU(v)
                                     v <- updateV(unorm) # computes vd
                                     print(paste("done with iter", i))
                                     print(convval <- max(abs(v-vold)))
                                     if (convval < thr) break
                                     vold <- v
                                 }

                                 fixFit(v, unorm)
                             }

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
                             return(result)
                         }
                     )
                     )

