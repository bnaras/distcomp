shinyUI(fluidPage(
  titlePanel("Setup Low Rank Approximation Model")
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

             , tabPanel(title = "Sanity Check"
                      , value = "sanityCheck"
                      , actionButton(inputId = "checkSanity",
                                     label = "Check Sanity")
                      , br()
                      , conditionalPanel(condition = "input.checkSanity != 0"
                                       , verbatimTextOutput(outputId = 'sanityCheckResult'))
                        )
             , tabPanel(title = "Send to OpenCPU Server"
                      , value = "sendToServer"
                      , textInput(inputId = "siteName"
                                , label = "Site Name"
                                , value = "Site[0-9]+")
                      , textInput(inputId = "ocpuURL"
                                , label = "OpenCPU URL"
                                , value = "http://localhost:")
                      , actionButton(inputId = "populateServer"
                                   , label = "Populate OpenCPU Server")
                      , conditionalPanel(condition = "input.populateServer != 0"
                                       , verbatimTextOutput(outputId = 'populateResult')
                                       , br()
                                       , actionButton(inputId = "exitApp", label = "Exit"))
                        )
             , "-----"
             , tabPanel("Help-FAQ"
                      , h5("here in help")
                        )
               )
))
