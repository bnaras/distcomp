shinyUI(fluidPage(
  titlePanel(h3("Set up NCP"))
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
             , tabPanel(title = "Specify Sites"
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
                                       , h5(textOutput(outputId = 'setupNCP')))
                        )
             , tabPanel(title = "Send to NCP Server"
                      , value = "sendToServer"
                      , textInput(inputId = "siteName"
                                , label = "Site Name"
                                , value = "NCP[12]")
                      , textInput(inputId = "ocpuURL"
                                , label = "OpenCPU URL"
                                , value = "http://localhost:")
                      , actionButton(inputId = "populateServer"
                                   , label = "Populate NCP OpenCPU Server")
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


