sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabId"
      , menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
      , menuItem("Load Definition", tabName = "loadDefn", icon = icon("upload", lib = "glyphicon"))
      , conditionalPanel(
            condition = "input.tabId == 'loadDefn'"
          , fileInput(inputId = 'defnFile'
                    , label = 'RDS file:'
                    , multiple = FALSE
                    , accept = c(".RDS", ".rds"))
        ##  , actionButton("loadDefinition", "Load")
        )
      , menuItem("Load Data", tabName = "loadData", icon = icon("upload", lib = "glyphicon"))
      , conditionalPanel(
            condition = "input.tabId == 'loadData'"
          , textInput(inputId = "missingIndicators"
                    , label = "Optional missing values indicator(s), comma separated"
                    , value = "NA")
          , fileInput(inputId = 'dataFile'
                    , label = '(CSV data file)'
                    , multiple = FALSE
                    , accept = NULL)
          ## , actionButton("loadDataFile", "Load")
        )
      , menuItem("Data Schema", tabName = "dataSchema", icon = icon("list", lib = "glyphicon"))
      , conditionalPanel(
            condition = "input.tabId == 'dataSchema'"
          , htmlOutput("variable")
          , actionButton(inputId = "save", "Save Schema")
        )
      , actionButton(inputId = "exitApp", "Exit")
    )
)

body <- dashboardBody(
  tabItems(
      tabItem(tabName = "dashboard"
            , h2("Distributed Computations without Aggregation")
            , p(em(intro_line_1, intro_line_2))
            , h3("Abstract")
            , p(paper_citation$abstract)
              )
    , tabItem(tabName = "loadDefn"
            , h2("Computation Definition")
            , DT::dataTableOutput("defn")
              )
    , tabItem(tabName = "loadData"
            , h2("Data")
            , DT::dataTableOutput("data")
              )
    , tabItem(tabName = "dataSchema"
            , h2("Variable Type")
            , tableOutput("vType")
              )
  )
)

dbHeader <- dashboardHeader(title = "Distcomp")

ui <- dashboardPage(
    title = "Data Schema"
  , dbHeader
  , sidebar
  , body
  , skin = "yellow"
)

