shinyUI(fluidPage(
  titlePanel(h3("Output Master code"))
, navlistPanel(selected="Definition File", id='mynavlist'
             , tabPanel(title = "Definition File"
                      , value = "definitionUpload"
                      , fileInput(inputId = 'definitionFile'
                                , label = 'RDS file:'
                                , multiple = FALSE
                                , accept = c(".RDS", ".rds"))

                      , actionButton(inputId = "uploadDefinition"
                                   , label = "Upload Definition")

                      , conditionalPanel(condition = "input.uploadDefinition != 0"
                                       , verbatimTextOutput(outputId = 'definitionSummary')
                                       , br()
                                       , h5(textOutput(outputId = 'definitionUploaded')))
                        )
             , tabPanel(conditionalPanel(condition = "input.uploadDefinition != 0"
                               , uiOutput("siteControls")))
             , tabPanel(title = "Output R code"
                      , value = "outputResult"
                      , textInput(inputId = "outputFile"
                                , label = "Output Filename Prefix"
                                , value = "master")
                      , actionButton(inputId = "saveCode"
                                   , label = "Save Code")
                      , conditionalPanel(condition = "input.saveCode != 0"
                                       , h5(textOutput(outputId = 'codeSaved'))
                                       , br()
                                       , actionButton(inputId = "exitApp", label = "Exit"))
                        )
             , "-----"
             , tabPanel("Help-FAQ"
                      , h5("here in help")
                        )
               )
))


