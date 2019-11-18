## ----echo=FALSE----------------------------------------------------------
### get knitr just the way we like it

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  tidy = FALSE,
  cache = FALSE
)


## ------------------------------------------------------------------------
if (!require("survival")) {
    stop("this vignette requires the survival package")
}
library(homomorpheR)


## ------------------------------------------------------------------------
sampleSize <- c(n1 = 1000, n2 = 500, n3 = 1500)

set.seed(12345)

beta.1 <- -.015; beta.2 <- .2; beta.3 <- .001;

lambdaT <- c(5, 4, 3)
lambdaC <- 2

coxData <- lapply(seq_along(sampleSize),
                  function(i) {
                      sex <- sample(c(0, 1), size = sampleSize[i], replace = TRUE)
                      age <- sample(40:70, size = sampleSize[i], replace = TRUE)
                      bm <- rnorm(sampleSize[i])
                      trueTime <- rweibull(sampleSize[i],
                                           shape = 1,
                                           scale = lambdaT[i] * exp(beta.1 * age + beta.2 * sex + beta.3 * bm ))
                      censoringTime <- rweibull(sampleSize[i],
                                                shape = 1,
                                                scale = lambdaC)
                      time <- pmin(trueTime, censoringTime)
                      event <- (time == trueTime)
                      data.frame(stratum = i,
                                 sex = sex,
                                 age = age,
                                 bm = bm,
                                 time = time,
                                 event = event)
                  })


## ------------------------------------------------------------------------
str(coxData[[1]])


## ------------------------------------------------------------------------
str(coxData[[2]])


## ------------------------------------------------------------------------
str(coxData[[3]])


## ------------------------------------------------------------------------
aggModel <- coxph(formula = Surv(time, event) ~ sex +
                                age + bm + strata(stratum),
                            data = do.call(rbind, coxData))
aggModel


## ------------------------------------------------------------------------
aggModel$loglik


## ------------------------------------------------------------------------
Site <-
    R6::R6Class(
            "Site",
            private = list(
                ## name of the site
                name = NA,
                ## local data
                data = NA,
                ## Control variable for cox regression
                cph.control = NA,
                beta_cache = list(),
                local_nll = function(beta) {
                    ## Check if value is cached
                    beta_hash  <- paste0("b", digest::digest(beta, algo = "xxhash64"))
                    result  <- private$beta_cache[[beta_hash]]
                    if (is.null(result)) {
                        ## We're worker, so compute local negative log likelihood
                        nllValue  <- tryCatch({
                            m <- coxph(formula = Surv(time, event) ~ sex + age + bm,
                                       data = private$data,
                                       init = beta,
                                       control = private$cph.control)
                            -(m$loglik[1])
                        },
                        error = function(e) NA)
                        if (!is.na(nllValue)) {
                            pubkey <- self$pubkey
                            ## Generate random offset for int and frac parts
                            offset <- list(int = random.bigz(nBits = 256),
                                           frac = random.bigz(nBits = 256))
                            ## 2. Add to neg log likelihood
                            result.int <- floor(nllValue)
                            result.frac <- nllValue - result.int
                            ## Approximate fractional part by a rational
                            result.fracnum <- gmp::as.bigz(gmp::numerator(gmp::as.bigq(result.frac) * self$den))
                            result  <- list(
                                int1 = pubkey$encrypt(result.int - offset$int),
                                frac1 = pubkey$encrypt(result.fracnum - offset$frac),
                                int2 = pubkey$encrypt(result.int + offset$int),
                                frac2 = pubkey$encrypt(result.fracnum + offset$frac)
                            )
                            private$beta_cache[[beta_hash]]  <- result
                        } else {
                            result  <- list(int1 = NA, frac1 = NA, int2 = NA, frac2 = NA)
                        }
                    }
                    result
                }
            ),
            public = list(
                count = NA,
                ## Common denominator for approximate real arithmetic
                den = NA,
                ## The master's public key; everyone has this
                pubkey = NA,
                initialize = function(name, data) {
                    private$name <- name
                    private$data <- data
                    private$cph.control <- replace(coxph.control(), "iter.max", 0)
                },
                setPublicKey = function(pubkey) {
                    self$pubkey <- pubkey
                },
                setDenominator = function(den) {
                    self$den = den
                },
                ## neg log lik,
                nll = function(beta, party) {
                    result  <- private$local_nll(beta)
                    if (party == 1) {
                        list(int = result$int1, frac = result$frac1)
                    } else {
                        list(int = result$int2, frac = result$frac2)
                    }
                }
            )
        )



## ------------------------------------------------------------------------
NCParty <-
    R6::R6Class(
            "NCParty",
            private = list(
                ## name of the site
                name = NA,
                ## NC party number
                number = NA,
                ## The master
                master = NA,
                ## The sites
                sites = list()
            ),
            public = list(
                ## The master's public key; everyone has this
                pubkey = NA,
                ## The denoinator for rational arithmetic
                den = NA,
                initialize = function(name, number) {
                    private$name <- name
                    private$number  <- number
                },
                setPublicKey = function(pubkey) {
                    self$pubkey <- pubkey
                    ## Propagate to sites
                    for (site in sites) {
                        site$setPublicKey(pubkey)
                    }
                },
                setDenominator = function(den) {
                    self$den <- den
                    ## Propagate to sites
                    for (site in sites) {
                        site$setDenominator(den)
                    }
                },
                addSite = function(site) {
                    private$sites  <- c(private$sites, list(site))
                },
                ## neg log lik
                nll = function(beta) {
                    pubkey  <- self$pubkey
                    results  <- lapply(sites, function(x) x$nll(beta, private$number))
                    ## Accumulate the integer and fractional parts
                    n  <- length(results)
                    sumInt  <- results[[1L]]$int
                    sumFrac <- results[[1L]]$frac
                    for (i in 2:n) {
                        sumInt  <- pubkey$add(sumInt, results[[i]]$int)
                        sumFrac  <- pubkey$add(sumFrac, results[[i]]$frac)
                    }
                    list(int = sumInt, frac = sumFrac)
                }
            )
        )



## ------------------------------------------------------------------------
Master  <-
    R6::R6Class(
            "Master",
            private = list(
                ## name of the site
                name = NA,
                ## Private and public keys
                keys = NA,
                ## Non cooperating party 1
                nc_party_1 = NA,
                ## Non cooperating party 2
                nc_party_2 = NA
            ),
            public = list(
                ## Denominator for rational arithmetic
                den  = NA,
                initialize = function(name) {
                    private$name <- name
                    private$keys <- PaillierKeyPair$new(1024) ## Generate new public and private key.
                    self$den <- gmp::as.bigq(2)^256  #Our denominator for rational approximations
                },
                setNCParty1  = function(site) {
                    private$nc_party_1 <- site
                    private$nc_party_1$setPublicKey(private$keys$pubkey)
                    private$nc_party_1$setDenominator(self$den)
                },
                setNCParty2  = function(site) {
                    private$nc_party_2 <- site
                    private$nc_party_2$setPublicKey(private$keys$pubkey)
                    private$nc_party_2$setDenominator(self$den)
                },
                ## neg log lik
                nLL = function(beta) {
                    pubkey  <- private$keys$pubkey
                    privkey  <- private$keys$getPrivateKey()
                    result1  <- private$nc_party_1$nll(beta)
                    result2  <- private$nc_party_2$nll(beta)
                    ## Accumulate the integer and fractional parts
                    sumInt  <- pubkey$add(result1$int, result2$int)
                    sumFrac  <- pubkey$add(result1$frac, result2$frac)
                    intResult <- as.double(privkey$decrypt(sumInt))
                    fracResult <- as.double(gmp::as.bigq(privkey$decrypt(sumFrac)) / self$den)
                    ## Since we 2L, we divide by 2.
                    (intResult + fracResult) / 2.0
                }
            )
        )


## ------------------------------------------------------------------------
site1 <- Site$new(name = "Site 1", data = coxData[[1]])
site2 <- Site$new(name = "Site 2", data = coxData[[2]])
site3 <- Site$new(name = "Site 3", data = coxData[[3]])

sites  <- list(site1 = site1, site2 = site2, site3 = site3)


## ------------------------------------------------------------------------
ncp1  <- NCParty$new("NCP1", 1)
ncp2  <- NCParty$new("NCP1", 2)



## ------------------------------------------------------------------------
for (s in sites) {
    ncp1$addSite(s)
    ncp2$addSite(s)
}


## ------------------------------------------------------------------------
master  <- Master$new("Master")


## ------------------------------------------------------------------------
master$setNCParty1(ncp1)
master$setNCParty2(ncp2)



## ------------------------------------------------------------------------
library(stats4)
nll <- function(age, sex, bm) master$nLL(c(age, sex, bm))
fit <- mle(nll, start = list(age = 0, sex = 0, bm = 0))


## ------------------------------------------------------------------------
summary(fit)


## ------------------------------------------------------------------------
summary(aggModel)


## ------------------------------------------------------------------------
cat(sprintf("logLik(MLE fit): %f, logLik(Agg. fit): %f\n", logLik(fit), aggModel$loglik[2]))

