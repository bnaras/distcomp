shinyUI(fluidPage(
  titlePanel("Define the Stratified Cox Model")
, navlistPanel(selected = "Data Upload"
             , id = 'navigationList' ## keeps track of our state for navigation panel on left
             , tabPanel(title = "Data Upload"
                      , value = "dataUpload"
                      , textInput(inputId = "missingIndicators"
                                , label = "Optional missing values indicator(s), comma separated"
                                , value = "NA")

                      , fileInput(inputId = 'dataFile'
                                , label = '(CSV data file)'
                                , multiple = FALSE
                                , accept = NULL)

                      , actionButton(inputId = "uploadData"
                                   , label = "Upload Data")

                      , conditionalPanel(condition = "input.uploadData != 0"
                                       , h5("Summary")
                                       , verbatimTextOutput(outputId = 'dataFileContentSummary')
                                       , br()
                                       , h3(textOutput(outputId = 'dataUploaded')))
                        )

             , tabPanel(title = "Formula Check"
                      , value = "formulaCheck"
                      , textInput(inputId = "formula"
                                , label = "Formula"
                                , value = "")
                      , actionButton(inputId = "checkFormula",
                                     label = "Check Formula")
                      , br()
                      , conditionalPanel(condition = "input.checkFormula != 0"
                                       , h5("Summary")
                                       , verbatimTextOutput(outputId = 'checkFormulaResult')
                                       , br()
                                       , h3(textOutput(outputId = 'formulaChecked')))
                        )

             , tabPanel(title = "Output Result"
                      , value = "outputResult"
                      , textInput(inputId = "outputFile"
                                , label = "Output File Name"
                                , value = "defn.rds")
                      , actionButton(inputId = "saveDefinition"
                                   , label = "Save Definition")
                      , conditionalPanel(condition = "input.saveDefinition != 0"
                                       , verbatimTextOutput(outputId = 'outputResult')
                                       , br()
                                       , h5(textOutput(outputId = 'definitionSaved'))
                                       , br()
                                       , actionButton(inputId = "exitApp", label = "Exit"))
                        )
             , "-----"
             , tabPanel("Help-FAQ"
                      , h5("here in help")
                        )
               )
))
