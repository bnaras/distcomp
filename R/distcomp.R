.makeOpencpuURL <- function(fn, urlPrefix, package="distcomp") {
    paste(urlPrefix, "library", package, "R", fn, "json", sep="/")
}

distcompSetup <- function(workspacePath = "",
                          defnPath = paste(workspacePath, "defn", sep=.Platform$file.sep),
                          instancePath = paste(workspacePath, "instances", sep=.Platform$file.sep),
                          defnFileName = "defn.rds",
                          dataFileName = "data.rds",
                          instanceFileName = "instance.rds",
                          ssl.verifyhost = TRUE,
                          ssl.verifypeer = TRUE) {

    testFileName <- paste(sample(letters, 15), collapse="")
    if (!file.exists(defnPath)) {
        defnOk <- dir.create(defnPath)
        if (!defnOk) {
            stop("distcompSetup: workspace permissions issue: not writable!")
        }
    } else {
        ## defnPath exists; check if it is writable
        testFile <- paste(defnPath, testFileName, sep=.Platform$file.sep)
        createOk <- file.create(testFile)
        if (!createOk) {
            stop("distcompSetup: workspace permissions issue: not writable!")
        }
        file.remove(testFile)
    }

    if (!file.exists(instancePath)) {
        defnOk <- dir.create(instancePath)
        if (!defnOk) {
            stop("distcompSetup: workspace permissions issue: not writable!")
        }
    } else {
        ## instancePath exists; check if it is writable
        testFile <- paste(instancePath, testFileName, sep=.Platform$file.sep)
        createOk <- file.create(testFile)
        if (!createOk) {
            stop("distcompSetup: workspace permissions issue: not writable!")
        }
        file.remove(testFile)
    }

    distcompEnv <- getOption("distcompEnv")
    distcompEnv[["config"]] <- list(workspacePath = workspacePath,
                                    defnPath = defnPath,
                                    instancePath = instancePath,
                                    defnFileName = defnFileName,
                                    dataFileName = dataFileName,
                                    instanceFileName = instanceFileName,
                                    sslConfig = list(ssl.verifyhost=ssl.verifyhost, ssl.verifypeer=ssl.verifypeer))
    TRUE
}

getConfig <- function(...) {
    getOption("distcompEnv")[["config"]]
}


makeSlave <- function (defn, data) {
    compType <- defn$compType
    available <- availableComputations()
    stopifnot(compType %in% names(available))
    switch(compType,
           "StratifiedCoxModel" = coxSlave$new(data = data, formula = defn$formula),
           "RankKSVD" = svdSlave$new(x = data),
           stop(paste("No such Computation:", compType)))
}


makeMaster <- function(defn) {
    compType <- defn$compType
    available <- availableComputations()
    stopifnot(compType %in% names(available))
    switch(compType,
           "StratifiedCoxModel" = coxMaster$new(id = defn$id, formula = defn$formula),
           "RankKSVD" = svdMaster$new(id = defn$id),
           stop(paste("No such Computation:", compType)))
}

availableComputations <- function() {
    list(
        StratifiedCoxModel ="Stratified Cox Model",
        RankKSVD = "Rank K SVD")

}

## Execute method on object whose id is given with args ...
executeMethod <- function(objectId, method, ...) {
    config <- getConfig()
    filePath <- paste(config$instancePath, objectId, config$instanceFileName, sep=.Platform$file.sep)
    object <- readRDS(file=filePath)
    call <- substitute(object$METHOD(...), list(METHOD = as.name(method)))
    result <- eval(call)
    if (object$getStateful()) {
        saveRDS(object, file=filePath)
    }
    result
}

## Deserialize URL result, only handle JSON at the moment
.deSerialize <- function(q) {
    cType <- headers(q)['content-type']
    if (cType == "application/json")
        fromJSON(rawToChar(q$content))
    else
        q$content
}

## Propose a new computation
defineNewComputation <- function() {
    shiny::runApp(system.file("NewComputation", package="distcomp"))
}

## Make Slave Site
setupSlave <- function() {
    shiny::runApp(system.file("SlaveApp", package="distcomp"))
}

## Make Master Site
setupMaster <- function() {
    shiny::runApp(system.file("MasterApp", package="distcomp"))
}

## Make an object specified by defnId and index it by
## a specified instanceId
createInstanceObject <- function (defnId, instanceId, dataFileName) {
    config <- getConfig()
    defn <- readRDS(paste(config$defnPath, defnId, config$defnFileName, sep=.Platform$file.sep))
    compType <- defn$compType
    available <- availableComputations()
    if (!(compType %in% names(available))) {
        stop(paste("createInstanceObject: No such computation:", compType))
    }
    data <- readRDS(paste(config$defnPath, defnId, dataFileName, sep=.Platform$file.sep))

    object <- switch(compType,
                     "StratifiedCoxModel" = coxSlave$new(data = data, formula = defn$formula),
                     "RankKSVD" = svdSlave$new(x = data),
                     stop(paste("No such Computation:", compType))) ## should not be reached
    ## Check if the instance folder exists
    thisInstancePath <- paste(config$instancePath, instanceId, sep=.Platform$file.sep)
    dir.create(thisInstancePath)
    ## Save it under the instance id to find it.
    saveRDS(object, file=paste(thisInstancePath, config$instanceFileName, sep=.Platform$file.sep))
    TRUE
}

destroyInstanceObject <- function (instanceId) {
    config <- getConfig()
    file.remove(paste(config$instancePath, instanceId, config$instanceFileName, sep=.Platform$file.sep))
    file.remove(paste(config$instancePath, instanceId, sep=.Platform$file.sep))
    TRUE
}

saveNewComputation <- function(defn, data, dataFileName) {
    config <- getConfig()
    defnId <- defn$defnId
    thisDefnPath <- paste(config$defnPath, defnId, sep=.Platform$file.sep)
    ## Should tryCatch this
    dir.create(thisDefnPath)
    saveRDS(object=defn, file=paste(thisDefnPath, config$defnFileName, sep=.Platform$file.sep))
    if (missing(dataFileName)) {
        saveRDS(object=data, file=paste(thisDefnPath, config$dataFileName, sep=.Platform$file.sep))
    } else {
        saveRDS(object=data, file=paste(thisDefnPath, dataFileName, sep=.Platform$file.sep))
    }
    TRUE
}

uploadNewComputation <- function(url, defn, data, dataFileName) {
    if (missing(dataFileName)) {
        payload <- list(defn = defn,
                        data = data)
    } else {
        payload <- list(defn = defn,
                        data = data,
                        dataFileName = dataFileName)
    }
    q <- POST(.makeOpencpuURL(urlPrefix=url, fn="saveNewComputation"),
              body = toJSON(payload),
              add_headers("Content-Type" = "application/json"),
              config=getConfig()$sslConfig
              )
    .deSerialize(q)
}

