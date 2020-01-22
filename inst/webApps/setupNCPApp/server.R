shinyServer(function(input, output, session) {

  output$definitionSummary <- renderPrint({
    if (input$uploadDefinition == 0) return("")
    ## Create data frame from file
    isolate({
      inputFile <- input$definitionFile
      ## shiny::validate(
      ##   need(inputFile != "", "Please select a data set")
      ## )

      available <- availableComputations()

      ## Try to read the RDS fileReturn data frame or error as the case may be
      definition <- tryCatch(
      { readRDS(inputFile$datapath) },
      warning = function(x) x,
      error = function(x) x
      )

      if ( inherits(definition, "error") ||
          !is.data.frame(definition) || is.null(definition$compType) ||
          !(definition$compType %in% names(available)) ) {
        cat("Error! Bad definition file")
      } else {
        for (x in names(definition)) {
          setComputationInfo(x, definition[[x]])
        }
        str(definition)
      }
    })
  })

    output$definitionUploaded <- reactive({
        if (input$uploadDefinition == 0) return()
        ifelse(!is.null(getComputationInfo("id")), "Definition Uploaded; Proceed to specify sites", "")
    })

    output$setupNCP <- reactive({
        if (input$addSite == 0) return()
        if(length(getComputationInfo("siteList")) >= 1) "At least one site added; so can send to NCP server" else ""
    })

  output$siteList <- renderPrint({
    if (input$addSite == 0) return()
    isolate({
      name <- stringr::str_trim(input$siteName)
      url <- stringr::str_to_lower(stringr::str_trim(input$ocpuURL))
      shiny::validate(
        need(name != "", "Please enter a non-empty name")
      )
      shiny::validate(
        need(url != "" && grepl("^http", url), "Please enter a URL")
      )

      siteList <- getComputationInfo("siteList")

      shiny::validate(
        need(!(name %in% names(siteList)), paste("Bad site: duplicate name", name))
      )

      n <- length(siteList)
      siteList[[n + 1]] <- list(name = name, url = url)
      setComputationInfo("siteList", siteList)
      str(siteList)
    })
  })

    output$populateResult <- renderPrint({
        if (input$populateServer == 0) return()
        isolate({
            shiny::validate(need(input$siteName != "", "Please enter a site name"))
            shiny::validate(need(input$ocpuURL != "", "Please enter an opencpu URL"))
            site <- list(name = input$siteName, url = input$ocpuURL)
            defn <- makeDefinition(getComputationInfo("compType"))
            data <- getComputationInfo("siteList")
            result <- tryCatch(uploadNewComputation(site=site, defn=defn, data=data),
                               error = function(x) x,
                               warning = function(x) x)
            if (inherits(result, "error") ) {
                "Error: Uploading the definition to server"
            } else {
                ifelse(result, "Success: definition uploaded to server",
                       "Error while uploading definition to server")
            }
        })
    })


  observe({
    if (input$exitApp ==0 ) return()
    stopApp(TRUE)
  })

})

