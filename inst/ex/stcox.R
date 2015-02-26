library(distcomp)
defn <-
structure(list(id = "ae2be5012430150b", compType = "StratifiedCoxModel", 
    projectName = "STCoxTest", projectDesc = "Stratified Cox Test", 
    formula = "Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 + ivhx3 + race + treat"), .Names = c("id", 
"compType", "projectName", "projectDesc", "formula"), row.names = c(NA, 
-1L), class = "data.frame")
sites <-
structure(c("http://127.0.0.1:3978/ocpu", "http://127.0.0.1:3978/ocpu"
), .Names = c("site1", "site2"))
siteDataFiles <-
c("site1.rds", "site2.rds")
siteNames <-
c("site1", "site2")
master <- coxMaster$new(defnId = defn$id, formula=defn$formula, localServer=TRUE)
for (i in seq.int(length(sites))) {
   master$addSite(siteNames[i], sites[i], dataFileName=siteDataFiles[i])
}
result <- master$run()
print(master$summary())
