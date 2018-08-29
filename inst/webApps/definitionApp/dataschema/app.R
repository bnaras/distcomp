library(shiny)
library(shinydashboard)
library(SBCpip)
library(tidyverse)
library(gridExtra)
library(bibtex)

paper_citation <- bibtex::read.bib(system.file("extdata", "platelet.bib", package = "SBCpip"))

intro_line_1 <- 'An application implementing the method described by'
intro_line_2 <- a(href = sprintf("https://doi.org/%s", paper_citation$doi),
                  sprintf("%s et. al., PNAS %s(%s) %s",
                          paper_citation$author[1],
                          paper_citation$volume,
                          paper_citation$number,
                          paper_citation$year))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(3L)

set_config_param(param = "output_folder",
                 value = "/Users/naras/R/packages/platelet-data/Blood_Center_Outputs")
set_config_param(param = "log_folder",
                 value = "/Users/naras/R/packages/platelet-data/Blood_Center_Logs")
set_config_param(param = "report_folder",
                 value = "/Users/naras/R/packages/platelet-data/Blood_Center_Reports")
config <- set_config_param(param = "data_folder",
                           value = "/Users/naras/R/packages/platelet-data/Blood_Center_inc")

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabId"
      , menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
      , menuItem("Settings", tabName = "settings", icon = icon("wrench"))
      , conditionalPanel(
            condition = "input.tabId == 'settings'"
          , actionButton(inputId = "setValues", label = "Apply")
        )
      , menuItem("Predict for Today", icon = icon("caret-right"), tabName = "prediction")
      , conditionalPanel(
            condition = "input.tabId == 'prediction'"
          , dateInput("predictDate", label = "Date", value = as.character(Sys.Date()))
          , actionButton("predictButton", "Predict")
        )
      , menuItem("Reports", icon = icon("table")
               , menuSubItem("CBC Summary", tabName = "cbc")
               , conditionalPanel(
                     condition = "input.tabId == 'cbc'"
                   , dateInput("cbcDate", label = "Date", value = as.character(Sys.Date()))
                   , actionButton("cbcSummaryButton", "Summarize")
                 )
               , menuSubItem("Census Summary", tabName = "census")
               , conditionalPanel(
                     condition = "input.tabId == 'census'"
                   , dateInput("censusDate", label = "Date", value = as.character(Sys.Date()))
                   , actionButton("censusSummaryButton", "Summarize")
                 )
               , menuSubItem("Prediction Summary", tabName = "predictionSummary")
               , conditionalPanel(
                     condition = "input.tabId == 'predictionSummary'"
                   , dateInput("startDate", label = "Start Date", value = "2018-04-10")
                   , dateInput("endDate", label = "End Date", value = as.character(Sys.Date()))
                   , actionButton("predictionSummaryButton", "Summarize")
                 )
                 )
      , menuItem("Plots", icon = icon("bar-chart-o"), tabName = "modelPlots")
              ## , menuSubItem("Model Performance", tabName = "modelPlots")
              ## , menuSubItem("Model vs. Inventory", tabName = "modelVsInventoryPlots")
              ##   )
        ## , textOutput("selectedTab")
      , actionButton(inputId = "exitApp", "Exit")
    )
)

body <- dashboardBody(
  tabItems(
      tabItem(tabName = "dashboard"
            , h2("Platelet Inventory Prediction")
            , p(em(intro_line_1, intro_line_2))
            , h3("Abstract")
            , p(paper_citation$abstract)
              )
    , tabItem(tabName = "settings"
            , fluidRow(
                  h2("Configuration Settings")
                , box(
                      h3("Filename Patterns (%s is YYYY-mm-dd)")
                    , textInput(inputId = "cbc_filename_prefix", label = "CBC Files", value = SBCpip::get_SBC_config()$cbc_filename_prefix)
                    , textInput(inputId = "census_filename_prefix", label = "Census Files", value = SBCpip::get_SBC_config()$census_filename_prefix)
                    , textInput(inputId = "transfusion_filename_prefix", label = "Transfusion Files", value = SBCpip::get_SBC_config()$transfusion_filename_prefix)
                    , textInput(inputId = "inventory_filename_prefix", label = "Inventory Files", value = SBCpip::get_SBC_config()$inventory_filename_prefix)
                    , textInput(inputId = "output_filename_prefix", label = "Output Files", value = SBCpip::get_SBC_config()$output_filename_prefix)
                    , textInput(inputId = "log_filename_prefix", label = "Log Files", value = SBCpip::get_SBC_config()$log_filename_prefix)
                  )
                , box(
                      h3("Input and Output Locations")
                    , textInput(inputId = "data_folder", label = "Data Folder", value = SBCpip::get_SBC_config()$data_folder)
                    , textInput(inputId = "report_folder", label = "Report Folder", value = SBCpip::get_SBC_config()$report_folder)
                    , textInput(inputId = "output_folder", label = "Output Folder", value = SBCpip::get_SBC_config()$output_folder)
                    , textInput(inputId = "log_folder", label = "Log Folder", value = SBCpip::get_SBC_config()$log_folder)
                  )
                , box(
                      h3("Model Fitting Parameters")
                    , sliderInput(inputId = "c0"
                                , label = "Minimum Remaining Units:"
                                , min = 20
                                , max = 100
                                , value = 30)
                    , sliderInput(inputId = "min_inventory"
                                , label = "Minimum Required Inventory Units:"
                                , min = 20
                                , max = 100
                                , value = 30)
                    , sliderInput(inputId = "history_window"
                                , label = "History Window:"
                                , min = 150
                                , max = 300
                                , value = 200)
                    , sliderInput(inputId = "penalty_factor"
                                , label = "Penalty Factor:"
                                , min = 10
                                , max = 25
                                , value = 15)
                    , sliderInput(inputId = "start"
                                , label = "Skip Initial:"
                                , min = 10
                                , max = 25
                                , value = 10)
                    , sliderInput(inputId = "model_update_frequency"
                                , label = "Model Update Frequency:"
                                , min = 7
                                , max = 30
                                , value = 7)
                  )
              )
              )
    , tabItem(tabName = "prediction"
            , h2("Prediction Result")
            , tableOutput("predictionResult")
            , h3("Prediction Log")
            , tableOutput("predictionLog")
              )
    , tabItem(tabName = "cbc"
            , h2("CBC Summary")
            , tableOutput("cbcSummary")
              )
    , tabItem(tabName = "census"
            , h2("Census Summary")
            , tableOutput("censusSummary")
              )
    , tabItem(tabName = "predictionSummary"
            , h2("Prediction Table")
            , tableOutput("predictionTable")
              )
    , tabItem(tabName = "modelPlots"
            , h2("Waste and Remaining Histograms")
            , plotOutput("wrPlot")
              )
  )
)

dbHeader <- dashboardHeader()
##logo_src <- system.file("webapps", "dashboard", "assets", "sbc.png", package = "SBCpip")
dbHeader$children[[2]]$children <-  tags$a(href='https://stanfordbloodcenter.org',
                                           tags$img(src = 'https://sbcdonor.org/client_assets/images/logos/logo_stanford.png',
                                                    alt = 'Stanford Blood Center Dashboard', height = '40'))
##dbHeader

ui <- dashboardPage(
    title = "SBC Dashboard",
    ##dashboardHeader(title = "Stanford Blood Center")
    dbHeader
  , sidebar
  , body
  , skin = "green"
)

server <- function(input, output, session) {

    observeEvent(input$exitApp, {
        stopApp(TRUE)
    })

    output$cbcSummary <- renderTable({
        ## Take dependency on button
        input$cbcSummaryButton
        isolate({
            config <- SBCpip::get_SBC_config()
            filename_prefix <- sprintf(config$cbc_filename_prefix, as.character(input$cbcDate))
            filenames <- list.files(path = config$report_folder, pattern = filename_prefix, full.names = TRUE)
            ##print(filenames)
            ## Should only be one, in any case, we use the last
            if (length(filenames) > 0) {
                readxl::read_xlsx(tail(filenames, 1), sheet = 2) %>%
                    dplyr::select(-RESULT_DATE)
            }
        })
    })

    output$censusSummary <- renderTable({
        ## Take dependency on button
        input$censusSummaryButton
        isolate({
            config <- SBCpip::get_SBC_config()
            filename_prefix <- sprintf(config$census_filename_prefix, as.character(input$censusDate))
            filenames <- list.files(path = config$report_folder, pattern = filename_prefix, full.names = TRUE)
            ##print(filenames)
            ## Should only be one, in any case, we use the last
            if (length(filenames) > 0) {
                readxl::read_xlsx(tail(filenames, 1), sheet = 1) %>%
                    dplyr::select(-LOCATION_DT) %>%
                        t ->
                        tmp
                tibble::tibble(Location = rownames(tmp), Count = as.integer(tmp[, 1])) %>%
                    dplyr::arrange(Location)
            }
        })
    })

    output$predictionTable <- renderTable({
        ## Take dependency on button
        input$predictionSummaryButton
        isolate({
            config <- SBCpip::get_SBC_config()
            SBCpip::build_prediction_table(config,
                                           generate_report = FALSE,
                                           start_date = input$startDate,
                                           end_date = input$endDate) %>%
                dplyr::select(1, 2, 10:15) %>%
                    dplyr::mutate_at("date", as.character) %>%
                    dplyr::mutate_if(is.numeric, as.integer)
        })
    })

    observeEvent(input$predictButton, {
        opts <- options(warn = 1) ## Report warnings as they appear
        config <- SBCpip::get_SBC_config()
        log_file <- tempfile("SBCpip", fileext = ".json")
        loggit::setLogFile(log_file)
        output$predictionResult <- renderTable({
            SBCpip::predict_for_date(config = config, date = input$predictDate) %>%
                dplyr::mutate_at("date", as.character)
        })
        output$predictionLog <- renderTable({
            jsonlite::read_json(log_file, simplifyVector = TRUE)
        })
    })

    observeEvent(input$setValues, {
        set_config_param("data_folder", input$data_folder)
        set_config_param("output_folder", input$output_folder)
        set_config_param("report_folder", input$report_folder)
        set_config_param("log_folder", input$log_folder)

        set_config_param("cbc_filename_prefix", input$cbc_filename_prefix)
        set_config_param("census_filename_prefix", input$census_filename_prefix)
        set_config_param("transfusion_filename_prefix", input$transfusion_filename_prefix)
        set_config_param("inventory_filename_prefix", input$inventory_filename_prefix)
        set_config_param("output_filename_prefix", input$output_filename_prefix)

        set_config_param("c0", input$c0)
        set_config_param("min_inventory", input$min_inventory)
        set_config_param("start", input$start)
        set_config_param("history_window", input$history_window)
        set_config_param("penalty_factor", input$penalty_factor)
        set_config_param("model_update_frequency", input$model_update_frequency)
        loggit::loggit(log_lvl = "INFO", log_msg = "Settings saved.")
        showNotification("Settings saved.")
    })

    output$wrPlot <- renderPlot({
        config <- SBCpip::get_SBC_config()
        input$min_inventory
        build_prediction_table(config, generate_report = FALSE,
                               start_date = as.Date("2018-04-10"), end_date = Sys.Date()) %>%
            dplyr::select(1,2, 10:15) %>%
            dplyr::mutate_at("date", as.character) %>%
            dplyr::filter(dplyr::row_number() > config$start) ->
            d

        p1 <- ggplot2::ggplot() +
            ggplot2::geom_histogram(mapping = ggplot2::aes(x = dplyr::pull(d[, 4]) + dplyr::pull(d[, 5])),
                                    bins = 100, color = cols[3]) +
            ggplot2::labs(x = "Remaining Units", title = "")

        p2 <- ggplot2::ggplot() +
            ggplot2::geom_histogram(mapping = ggplot2::aes(x = dplyr::pull(d[, 6])),
                                    bins = 100, color = cols[3]) +
            ggplot2::labs(x = "Wasted Units", title = "")

        d %>%
            dplyr::select(1,2,4,5) %>%
            dplyr::mutate(date = as.Date(date),
                          `Actual usage` = `Platelet usage`,
                          `Estimated usage` = `Adj. no. expiring in 1 day` + `Adj. no. expiring in 2 days`) %>%

            tail(n = config$history_window) %>%
            dplyr::select(date, `Actual usage`, `Estimated usage`) %>%
            tidyr::gather(key = type, value = "value", `Actual usage`, `Estimated usage`) %>%
            dplyr::filter(!is.na(value)) %>%
            dplyr::group_by(type) ->
            dd

        p3 <- ggplot2::ggplot(data = dd, mapping = aes(x = date, y = value, color = type)) +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = config$min_inventory, color = "coral", size = 1.2) +
            ggplot2::geom_hline(yintercept = 0, color = "purple", size = 1.2) +
            ggplot2::labs(x = "Actual and Estimated Usage", y = "Units") +
            ggplot2::theme(legend.position = "bottom")

        gridExtra::grid.arrange(
                       grobs = lapply(list(p1, p2, p3), ggplot2::ggplotGrob),
                       layout_matrix = matrix(c(1,3,2,3), nrow = 2))
    })

}

shinyApp(ui = ui, server = server)
