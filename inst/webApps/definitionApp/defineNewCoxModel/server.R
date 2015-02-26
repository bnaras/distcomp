shinyServer(function(input, output, session) {

  createProjectDefinition <- function() {
    data.frame(id = getComputationInfo("id"),
               compType = getComputationInfo("compType"),
               projectName = getComputationInfo("projectName"),
               projectDesc = getComputationInfo("projectDesc"),
               formula = getComputationInfo("formula"),
               stringsAsFactors=FALSE)
  }

  ## -- Begin: functions to make tabs active sequentially --
  ## First time make sure Data Upload is selected
  session$sendCustomMessage('activeNavs', 'Data Upload')
  updateTabsetPanel(session, inputId="navigationList", selected="Data Upload")
  observe({
    if (input$uploadData > 0 && is.data.frame(getComputationInfo("data"))) {
      session$sendCustomMessage('activeNavs', 'Formula Check')
      updateTabsetPanel(session, inputId="navigationList", selected="Formula Check")
    }
  })
  observe({
    if (input$checkFormula > 0 && !is.null(getComputationInfo("formula"))) {
      ##session$sendCustomMessage('activeNavs', 'Output Result')
      updateTabsetPanel(session, inputId="navigationList", selected="Output Result")
    }
  })
  observe({
    if (input$exitApp > 0) stopApp(TRUE)
  })

  ## -- End: functions to make tabs active sequentially --

  ## Variables to detect various states the app can be in, depending
  ## on what actions the user has taken.

  ## output$datasetSpecified
  ## output$datasetChecked
  ## output$formulaEntered
  ## output$formulaChecked
  ## output$definitionSaved

  ## When the user chooses a file and clicks on the "Upload Data" button
  ## this function is triggered
  output$dataFileContentSummary <- renderPrint({
    if (input$uploadData == 0) return("")
    ## Create data frame from file
    isolate({
      inputFile <- input$dataFile
      shiny::validate(
        need(inputFile != "", "Please select a data set")
      )

      ## Parse missing value strings
      missingValueIndicators <- stringr::str_trim(scan(textConnection(input$missingIndicators),
                                                       what = character(0), sep=",", quiet=TRUE))
      ## Return data frame or error as the case may be
      dataResult <- tryCatch(
        { read.csv(file = inputFile$datapath, na.strings = missingValueIndicators) } ,
        warning = function(x) x,
        error = function(x) x
      )

      if (is.data.frame(dataResult)){
        setComputationInfo("data", dataResult) ## Store data object
        updateTabsetPanel(session, inputId="navigationList", selected="Formula Check")
        str(dataResult)
      } else {
        cat('Error!', dataResult$message)
      }
    })
  })

  output$dataUploaded <- reactive({
    if (input$uploadData == 0) return()
    ifelse(is.data.frame(getComputationInfo("data")), "Data Uploaded; Proceed to Formula Check", "")
  })

  ## When the user clicks on the "Check Formula" button
  ## this function is triggered
  output$checkFormulaResult <- renderPrint({
    if (input$checkFormula == 0) return()
    isolate({
      result <- tryCatch(
        { coxSlave$new(formula = as.formula(input$formula), data=getComputationInfo("data")) },
        warning = function(x) x,
        error = function(x) x)
      if ("CoxSlave" %in% class(result)) { ## Success
        setComputationInfo("formula", input$formula)
        ## At this point, generate the definition id too.
        ## object <- list(compType = getComputationInfo("compType"),
        ##                projectName = getComputationInfo("projectName"),
        ##                projectDesc = getComputationInfo("projectDesc"),
        ##                formula = getComputationInfo("formula"),
        ##                random = runif(10))
        setComputationInfo("id", generateId(list(random=runif(10))))
        sprintf("Formula '%s' is OK!", input$formula)
      } else {
        cat("Error!", result$message)
      }
    })
  })

  output$formulaChecked <- reactive({
    if (input$checkFormula == 0) return()
    ifelse(is.null(getComputationInfo("formula")), "", "Formula Checked; Proceed to Output Result")
  })


  output$outputResult <- renderPrint({
    if (input$saveDefinition == 0) return()
    defn <- createProjectDefinition()
    str(defn)
  })

  output$definitionSaved <- reactive({
    if (input$saveDefinition == 0) return()
    defn <- createProjectDefinition()
    defnPath <- getConfig()$defnPath
    dirName <- paste(defnPath, defn$id, sep=.Platform$file.sep)
    fileName <- paste(dirName, input$outputFile, sep=.Platform$file.sep)
    result <- tryCatch(
      {
        dir.create(dirName)
        saveRDS(object = defn, file=fileName)
      },
      error = function(x) x)
    if (inherits(result, "error")) {
      paste("Error!", result$message)
    } else {
      paste0("Definition saved to ", fileName)
    }
  })
})

