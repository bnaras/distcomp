
shinyUI(fluidPage(
    
  tags$head(tags$script("
        window.onload = function() {
            $('#mynavlist a:contains(\"Variable Matching\")').parent().addClass('disabled');
            $('#mynavlist a:contains(\"Data Check\")').parent().addClass('disabled');
            $('#mynavlist a:contains(\"Dry Run\")').parent().addClass('disabled');
        };

        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
        });
   ")),

  titlePanel(h3("Instantiate a slave site")),
  navlistPanel(selected="Project/Data Summary", id='mynavlist',
               tabPanel("Project/Data Summary",                      
                  fileInput('projectfile',  h4('Please upload the project file: '), multiple = TRUE, accept = NULL),
                  actionButton("uploadPro", "Upload .RDS file"),
                  verbatimTextOutput('contentPro'),
                  
                  ##IF an RDS file was uploaded, offer the user to update his/her own data file
                  conditionalPanel(
                    condition = "output.fileRDS==1", 
                    h4('Please upload your .CSV data file: '),
                    textInput("naIn", label = h5("Missing values indicator(s) (optional, comma separated)"), 
                            value = "Enter text..."),
                    fileInput('datafile',  label="", multiple = TRUE, accept = NULL),
                    actionButton("uploadData", "Upload .CSV file"),
                    verbatimTextOutput('contentData'),
                    
                    #IF file is valid --> proceed to variables matching and data check
                    conditionalPanel(
                      condition = "output.fileCSV==1",
                      actionButton("matchVarsCheckData", "Proceed to Variable Matching")    
                      )
                    
                 )
               ),
               
               tabPanel("Variable Matching",value="varMatch",
                  h5('Please match the required variables to those in your data set'),
                  
                  verbatimTextOutput('remindFormula'),
                  
                  uiOutput("uiMatch"),
                  br(),
                  tags$br(),
                  actionButton("varMatchDone","Choose these variables and proceed to Data Check")

               ),
               
               tabPanel("Data Check",value="dataCheck",
                        h5('Variable Map:'),
                        verbatimTextOutput('varMap'),
                        actionButton("checkEmpty", "Empty Data Set Check"),
                        tags$br(),
                        verbatimTextOutput('validRows'),      

                        ## make this button appear ONLY after there's output from 'validRows'
                        conditionalPanel(
                          condition = "output.validRowsCheck==1",
                          actionButton("dataCheckDone", "Proceed to Dry Run")
                        )
                         
               ),
               
               tabPanel("Dry Run",value="dryRun",
                        
                  textInput("workspaceDir", label = h5("Workspace directory on VM:"), 
                                  value = "Enter text..."),
                  actionButton("generateDirs","Continue"),                        
                  verbatimTextOutput('printInstructions'),                 
                  textInput("slaveURL", label = h5("Instance (slave, VM) URL:"), 
                              value = "Enter text..."),
                  actionButton("runOnVM", "Test on VM"),
                  tags$br(),                    
                  verbatimTextOutput('serializeMsg')                                      
               )               
  )
))            