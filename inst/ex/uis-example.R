library(distcomp)
library(opencpu)

## First we do the conventional thing.

uis <- readRDS("uis.RDS")
coxOrig <- coxph(formula=Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 +
                     ivhx3 + race + treat + strata(site), data=uis)
summary(coxOrig)

## We define the computation

coxDef <- list(compType = names(availableComputations())[1],
               formula = "Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 + ivhx3 + race + treat",
               defnId = "UIS")

## We split the data by site
siteData <- with(uis, split(x=uis, f=site))
nSites <- length(siteData)
siteNames <- sapply(seq.int(nSites), function(i) paste("site", i, sep=""))
siteURLs <- lapply(seq.int(nSites), function(i) opencpu$url())
names(siteData) <- names(siteURLs) <- siteNames

## Fix needed for local server in order not to clobber data sets
siteDataFiles <- lapply(seq.int(nSites), function(i) paste("site", i, ".rds", sep=""))

ok <- Map(uploadNewComputation, siteURLs,
          lapply(seq.int(nSites), function(i) coxDef),
          siteData,
          siteDataFiles)

stopifnot(all(ok))

master <- coxMaster$new(defnId = coxDef$defnId, formula=coxDef$formula, localServer=TRUE)

for (i in seq.int(nSites)) {
    master$addSite(siteNames[i], siteURLs[[i]], dataFileName=siteDataFiles[[i]])
}

result <- master$run()

master$summary()

print(master$summary(), digits=5)
