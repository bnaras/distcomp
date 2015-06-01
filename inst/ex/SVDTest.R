library(distcomp)
defn <-
structure(list(id = "c83c9bd59551df3a", compType = "RankKSVD", 
    projectName = "SVDTest", projectDesc = "SVD Test Example", 
    rank = 2L, ncol = 5L), .Names = c("id", "compType", "projectName", 
"projectDesc", "rank", "ncol"), row.names = c(NA, -1L), class = "data.frame")
sites <-
list(structure(list(name = "Site1", url = "http://127.0.0.1:5999/ocpu"), .Names = c("name", 
"url")), structure(list(name = "Site2", url = "http://127.0.0.1:5999/ocpu"), .Names = c("name", 
"url")), structure(list(name = "Site3", url = "http://127.0.0.1:5999/ocpu"), .Names = c("name", 
"url")))
master <- makeMaster(defn)
for (site in sites) {
   master$addSite(name = site$name, url = site$url)
}
result <- master$run()
print(master$summary())
