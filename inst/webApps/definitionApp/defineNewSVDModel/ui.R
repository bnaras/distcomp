shinyUI(fluidPage(
  titlePanel("Define Low Rank Approximation Model")
, navlistPanel(selected = "Data Upload"
             , id = 'navigationList' ## keeps track of our state for navigation panel on left
             , tabPanel(title = "Data Upload"
                      , value = "dataUpload"
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

             , tabPanel(title = "Rank Check"
                      , value = "rankCheck"
                      , textInput(inputId = "rank"
                                , label = "Rank"
                                , value = "")
                      , actionButton(inputId = "checkRank",
                                     label = "Check Rank")
                      , br()
                      , conditionalPanel(condition = "input.checkRank != 0"
                                       , h5("Summary")
                                       , verbatimTextOutput(outputId = 'checkRankResult')
                                       , br()
                                       , h3(textOutput(outputId = 'rankChecked')))
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
