library(homomorpheR)

library(distcomp)

set.seed(123)

## Three sites
nSites <- 3
siteData <- lapply(c(30, 30, 40), function(n) {
    data.frame(age = sample(10:90, size = n, replace = TRUE),
               gender = sample(c("M", "F"), size = n, replace = TRUE),
               fluid = runif(n = n, min = 25, max = 75))
})

qdef <- data.frame(compType = "QueryCount",
                   id = "QC",
                   projectName = "QCTest",
                   projectDesc = "QC Test Project",
                   he = TRUE,
                   filterCondition = 'age > 50 & gender == "M"',
                   stringsAsFactors = FALSE)

master <- HEMaster$new(qdef)

pubkey  <- master$getPubkey()
pubkey_bits  <- pubkey$bits; pubkey_n  <- pubkey$n
den_bits  <- master$den_bits

## makeWorker(defn = qdef, data = siteData[[1]],
##            pubkey_bits = pubkey_bits,
##            pubkey_n = pubkey_n,
##            den_bits = den_bits)

sites <- lapply(seq.int(nSites),
                function(x) list(
                                name = paste0("site", x),
                                worker = makeWorker(defn = qdef, data = siteData[[x]],
                                                    pubkey_bits = pubkey_bits,
                                                    pubkey_n = pubkey_n,
                                                    den_bits = den_bits)
                            )
                )

ncp1  <- NCP$new(number = 1, defn = qdef,
                 pubkey_bits = pubkey_bits,
                 pubkey_n = pubkey_n,
                 den_bits = den_bits)

ncp2  <- NCP$new(number = 2, defn = qdef,
                 pubkey_bits = pubkey_bits,
                 pubkey_n = pubkey_n,
                 den_bits = den_bits)

for (site in sites) {
    ncp1$addSite(name = site$name, worker = site$worker)
    ncp2$addSite(name = site$name, worker = site$worker)
}

master$addNCP(ncpWorker = ncp1)
master$addNCP(ncpWorker = ncp2)

##debug(ncp1$run)
##debug(ncp2$run)
##debug(master$run)
##HEQueryCountMaster$debug("run")

##HEQueryCountWorker$debug("queryCount")

result <- master$run()

