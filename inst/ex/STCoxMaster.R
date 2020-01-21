library(distcomp)
masterData <- readRDS('STCoxMaster.RDS')
defn <- masterData$defn; sites <- masterData$sites;
master <- makeMaster(defn)
for (site in sites) {
   master$addSite(name = site$name, url = site$url)
}
result <- master$run()
print(master$summary())
