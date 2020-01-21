library(distcomp)

## First we do the conventional thing.

uis <- readRDS("uis.RDS")
coxOrig <- coxph(formula=Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 +
                     ivhx3 + race + treat + strata(site), data=uis)
summary(coxOrig)

## We define the computation

coxDef <- data.frame(compType = names(availableComputations())[2],
                     he = FALSE,
                     formula = "Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 + ivhx3 + race + treat",
                     id = "UIS",
                     stringsAsFactors=FALSE)
##
## Start opencpu server in a separate process or terminal before proceeding
## This is because the opencpu server blocks the main thread when it runs
## library(opencpu)
## ocpu_server_start()
## Default port is localhost:5656
##

## We split the data by site
siteData <- with(uis, split(x=uis, f=site))
nSites <- length(siteData)
sites <- lapply(seq.int(nSites),
                function(x) list(name = paste0("site", x),
                                 url = "http://localhost:5656/ocpu"))

ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) coxDef),
          siteData)

stopifnot(all(as.logical(ok)))

master <- CoxMaster$new(defn = coxDef)

for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()

master$summary()

print(master$summary(), digits=5)

sessionInfo()
