library(distcomp)
defn <-
structure(list(id = "a824ef3e39f9a9f4", compType = "StratifiedCoxModel", 
    projectName = "STCoxTest", projectDesc = "Stratified Cox Model Test", 
    formula = "Surv(time, censor) ~ age + becktota + ndrugfp1 + ndrugfp2 + ivhx3 + race + treat"), .Names = c("id", 
"compType", "projectName", "projectDesc", "formula"), row.names = c(NA, 
-1L), class = "data.frame")
sites <-
list(structure(list(name = "Site1", url = "http://127.0.0.1:5844/ocpu"), .Names = c("name", 
"url")), structure(list(name = "Site2", url = "http://127.0.0.1:5844/ocpu"), .Names = c("name", 
"url")))
master <- makeMaster(defn)
for (site in sites) {
   master$addSite(name = site$name, url = site$url)
}
result <- master$run()
print(master$summary())
