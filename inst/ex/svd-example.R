library(distcomp)

##print(availableComputations())

svdDef <- data.frame(compType = names(availableComputations())[3],
                     rank = 2L,
                     he = FALSE,
                     ncol = 5L,
                     id = "SVD",
                     stringsAsFactors = FALSE)

##
## Start opencpu server in a separate process or terminal before proceeding
## This is because the opencpu server blocks the main thread when it runs
## library(opencpu)
## ocpu_server_start()
## Default port is localhost:5656
##

set.seed(12345)

## Three sites
nSites <- 3
siteData <- lapply(seq.int(nSites), function(i) matrix(rnorm(100), nrow=20))
sites <- lapply(seq.int(nSites),
                function(x) list(name = paste0("site", x),
                                 url = "http://localhost:5656/ocpu"))

## Upload definition
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) svdDef),
          siteData)

stopifnot(all(as.logical(ok)))

##
master <- SVDMaster$new(defn = svdDef)

for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()

print(result)

## Compare with:

x <- do.call(rbind, siteData)

print(result$d)
print(svd(x)$d)

print(result$v)
print(svd(x)$v[, 1:2])

## All 5 singular values (takes much longer!)
svdDef <- data.frame(compType = names(availableComputations())[3],
                     rank = 5L,
                     he = FALSE,
                     ncol = 5L,
                     id = "SVD",
                     stringsAsFactors = FALSE)

## Upload definition
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) svdDef),
          siteData)

stopifnot(all(as.logical(ok)))

##
master <- SVDMaster$new(defn = svdDef)

for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()

print(result$d)
print(svd(x)$d)

print(result$v)
print(svd(x)$v)


sessionInfo()

