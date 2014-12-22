shinyUI(fluidPage(

  tags$head(tags$script("
        window.onload = function() {
            $('#mynavlist a:contains(\"Data Check\")').parent().addClass('disabled');
            $('#mynavlist a:contains(\"Dry Run\")').parent().addClass('disabled');
            $('#mynavlist a:contains(\"Output\")').parent().addClass('disabled');
        };

        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
        });
   ")),
   titlePanel(h3("Propose a new distributed computation")),
   navlistPanel(selected="Project/Data Summary", id='mynavlist',
   tabPanel("Project/Data Summary", icon=img(src="checkmark.png"),

         textInput("nameIn", label = h5("Project name (50 characters max.)"),
                   value = "Enter text..."),

         textInput("descripIn", label = h5("Description (250 characters max.)"),
                   value = "Enter text..."),

         br(),
         actionButton("summaryP", "Project Summary"),
         h5(textOutput('projectSummary')),
         textOutput('name'),
         textOutput('desc'),

         #for data summary: missing values indicator (optional), data file upload
         textInput("naIn", label = h5("Missing values indicator(s) (optional, comma separated)"),
                   value = "Enter text..."),

         fileInput('datafile',  h5('Data (CSV) File: '), multiple = TRUE, accept = NULL),
         actionButton("upload", "Upload Data"),
         h5(textOutput('dataSummary')),
         verbatimTextOutput('content'),

         br(),
         actionButton('data_upload_done', 'Done')
         ),


   tabPanel("Data Check",

         selectInput("compType",
                     label = h5("Type of computation"),
                     choices = list("Stratified Cox Model"),
                     selected = "Stratified Cox Model"),

         textInput("formulaIn", label = h5("Formula"),
                   value = "Enter text..."),
         actionButton("checkFormula", "Check if formula is correct"),
         tags$br(),
         verbatimTextOutput('formulaKosherness'),
         br(),

         conditionalPanel(
           condition = "output.pls==1",
           actionButton("checkEmpty", "Empty Data Set Check"),
           tags$br(),
           verbatimTextOutput('validRows'),
           actionButton("checkVarNames", "Check variable names"),

           conditionalPanel(
             condition = "input.checkVarNames>0",
             verbatimTextOutput('newNames'),

             conditionalPanel(
               condition = "output.nn==1", #there is only ONE variable to change
               #h5('add here radio buttons')
               radioButtons("radioN", label = "", inline=TRUE,
                            c("Yes"= "y", "No" = "n"))
               )
             )
           ),

         br(),
         actionButton('data_check_done', 'Done')
         ),
   tabPanel("Dry Run",

         h5('Length of vector beta required for your formula: '),
         verbatimTextOutput('betalen'),
         textInput("betaIn", label = h5("Please enter beta (comma separated values, if necessary)"),
                   value = "Enter value..."),
         actionButton("runModel", "Run Model on Data Set"),
         tags$br(),
         verbatimTextOutput('testrun'),

         actionButton('dry_run_done', 'Done')
        ),
   tabPanel("Output",

            actionButton("genList", "View project \'object\'"),
            verbatimTextOutput('projectObj'),
            downloadButton('saveData', 'Save as RDS'),
            tags$br()

            ),
   "-----",
   tabPanel("Help-FAQ",

    h5("Edit this page!")
    )

   )
))


