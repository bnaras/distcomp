server <- function(input, output, session) {

    observeEvent(input$exitApp, {
        stopApp(TRUE)
    })

  # Filter data based on selections

    output$data <-
        DT::renderDataTable({
                ## Take dependency on button
##                input$loadDefinition
##                isolate({
                    DT::datatable({
                        inputFile <- input$dataFile
                        if (!is.null(inputFile)) {
                            dataContent <- read.csv(file = inputFile$datapath,
                                                    na.strings = input$missingIndicators)
                            distcomp::setComputationInfo("data", dataContent)
                            dataContent
                        } else
                            data.frame()
                    })
##                })
        })
    output$defn <-
        DT::renderDataTable({
##            input$loadDataFile
##            isolate({
                DT::datatable({
                    inputFile <- input$defnFile
                    if (!is.null(inputFile)) {
                        rdsContent <- readRDS(file = inputFile$datapath)
                        distcomp::setComputationInfo("defn", rdsContent)
                        rdsContent
                    } else
                        data.frame()
                })
            })
##        })

    common_stuff <- reactive({
        ## Take dependency on defn and data
        input$defnFile
        input$dataFile
        defn <- distcomp::getComputationInfo("defn")
        data <- distcomp::getComputationInfo("data")
        if (is.null(defn) || is.null(data)) {
            return(list(defn = NULL, data = NULL, varNames = NULL))
        }
        varNames <- all.vars(as.formula(defn$formula))
        names(varNames) <- varNames
        result <- list(defn = defn, data = data, varNames = varNames)
        ##print(result)
        result
    })



 output$variable <- renderUI({
     d <- common_stuff()
     with(d, {
         ##print("In renderUI")
         vTypes <- sapply(varNames, function(x) is.numeric(data[, x]))
         ## Variable selection:
         selectInput(inputId = "variable", label = "Variables in formula", choices = varNames, selected = 1)
     })
 })

  output$vType <- renderTable({
      if (is.null(input$variable)) return(data.frame())
      d <- common_stuff()
      with(d, {
          ##print("In vType")
          vTypes <- sapply(varNames, function(x) is.numeric(data[, x]))
          ## Variable selection:
          ##print(input$variable)
          vName <- varNames[input$variable]
          ##print(vName)
          if (vTypes[vName]) {
              data.frame(type = "Numeric", stringsAsFactors = FALSE)
          } else {
              data.frame(FactorLevels = levels(as.factor(data[, vName])),
                         stringsAsFactors = FALSE)
          }
      })
  })

  observeEvent(input$save, {
      showNotification("Schema saved in definition.", duration = 25)
  })

}

#shinyApp(ui = ui, server = server)
