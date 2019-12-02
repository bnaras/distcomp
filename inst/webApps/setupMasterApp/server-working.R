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
        defn_id  <- getComputationInfo("id")
        he  <- getComputationInfo("he")
        if (!is.null(defn_id)) {
            ## If defn_id is there, so is he
            if (!is.null(he)) {
                if (he) {
                    "Definition Uploaded; Proceed to specify NCPs"
                } else {
                    "Definition Uploaded; Proceed to specify Sites"
                }
            }
        } else {
            ""
        }
    })

    output$siteControls  <- renderUI({
        if (input$uploadDefinition == 0) return()
        he  <- getComputationInfo("he")
        if (!is.null(he) && he) {
            tabPanel(title = "Specify NCPs"
                   , value = "specifySiteURLs"
                   , textInput(inputId = "siteName"
                             , label = "NCP Name"
                             , value = "NCP[12]+")
                   , textInput(inputId = "ocpuURL"
                             , label = "OpenCPU URL"
                             , value = "http://localhost:")
                   , actionButton(inputId = "addSite"
                                , label = "Add NCP")
                   , conditionalPanel(condition = "input.addSite != 0"
                                    , verbatimTextOutput(outputId = 'siteList')
                                    , h5(textOutput(outputId = 'writeRCode')))
                     )
        } else {
            tabPanel(title = "Specify Sites"
                   , value = "specifySiteURLs"
                   , textInput(inputId = "siteName"
                             , label = "Site Name"
                             , value = "Site[0-9]+")
                   , textInput(inputId = "ocpuURL"
                             , label = "OpenCPU URL"
                             , value = "http://localhost:")
                   , actionButton(inputId = "addSite"
                                , label = "Add site")
                   , conditionalPanel(condition = "input.addSite != 0"
                                    , verbatimTextOutput(outputId = 'siteList')
                                    , h5(textOutput(outputId = 'writeRCode')))
                     )
        }
    })


    output$writeRCode <- reactive({
        if (input$addSite == 0) return()
        ifelse(length(getComputationInfo("siteList")) > 1, "At least two sites added; so can write R code", "")
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

    output$codeSaved <- reactive({
        if (input$saveCode ==0 ) return()
        shiny::validate(
                   need(stringr::str_trim(input$outputFile) != "", "Please enter a non-empty name")
               )

        sites <- getComputationInfo("siteList")
        defn <- makeDefinition(getComputationInfo("compType"))
        result <- tryCatch(writeCode(defn, sites, input$outputFile)
                         , error=function(x) x
                         , warning=function(x) x)

        if (inherits(result, "error")) {
            "Error!"
        } else {
            paste("Success: code saved to", input$outputFile)
        }

    })

    observe({
        if (input$exitApp ==0 ) return()
        stopApp(TRUE)
    })

})

