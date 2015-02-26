library(distcomp)
defn <-
structure(list(id = structure(1L, .Label = "db19ec158c9d5218", class = "factor"), 
    compType = structure(1L, .Label = "RankKSVD", class = "factor"), 
    projectName = structure(1L, .Label = "SVDTest", class = "factor"), 
    projectDesc = structure(1L, .Label = "SVD Test Example", class = "factor"), 
    rank = 2L, ncol = 5L), .Names = c("id", "compType", "projectName", 
"projectDesc", "rank", "ncol"), row.names = c(NA, -1L), class = "data.frame")
sites <-
structure(c("http://127.0.0.1:3978/ocpu", "http://127.0.0.1:3978/ocpu", 
"http://127.0.0.1:3978/ocpu"), .Names = c("site1", "site2", "site3"
))
siteDataFiles <-
c("site1.rds", "site2.rds", "site3.rds")
siteNames <-
c("site1", "site2", "site3")
master <- svdMaster$new(defnId = defn$id, localServer=TRUE)
for (i in seq.int(length(sites))) {
   master$addSite(siteNames[i], sites[i], dataFileName=siteDataFiles[i])
}
result <- master$run(k=defn$rank)
print(result)
