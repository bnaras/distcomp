.onLoad <- function(lib, pkg) {
    ## Added by Naras
    if (is.null(getOption("distcompEnv"))) {
        options(distcompEnv = new.env(parent=emptyenv()))
    }
    ## End of Naras
}

.onUnload <- function(libpath)
    library.dynam.unload("distcomp", libpath)


