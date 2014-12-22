library(distcomp)
library(opencpu)
##library(httr)
##library(jsonlite)

print(availableComputations())

svdDef <- list(compType = names(availableComputations())[2],
               defnId = "SVD")

set.seed(12345)
## Three sites
nSites <- 3
siteNames <- sapply(seq.int(nSites), function(i) paste("site", i, sep=""))
siteData <- lapply(seq.int(nSites), function(i) matrix(rnorm(100), nrow=20))
siteURLs <- lapply(seq.int(nSites), function(i) opencpu$url())
names(siteData) <- names(siteURLs) <- siteNames

## Fix needed for local server in order not to clobber data sets
siteDataFiles <- lapply(seq.int(nSites), function(i) paste("site", i, ".rds", sep=""))

ok <- Map(uploadNewComputation, siteURLs,
          lapply(seq.int(nSites), function(i) svdDef),
          siteData,
          siteDataFiles)

stopifnot(all(ok))

## Assumes distcompProfile.R is executed, so objectIds contains object ids.
master <- svdMaster$new(defnId = svdDef$defnId, localServer=TRUE)

for (i in seq.int(nSites)) {
    master$addSite(siteNames[i], siteURLs[[i]], dataFileName=siteDataFiles[[i]])
}

result <- master$run(k=5, thr=1e-14, max.iter=100)

## Compare with:

x <- do.call(rbind, siteData)

svd(x)$d
