library(shiny)
library(shinydashboard)
library(plotly)

not_sel <- "Not Selected"

ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Flux Dashboard"),

                     dashboardSidebar(
                       fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
                       selectInput("site_id", h3("Select Site:"), choices = c(not_sel)),
                       selectInput("start_date", label="Forecast Horizon Date:", choices = c(not_sel)),
                       sidebarMenu(
                         menuItem("NEE Forecast", tabName = "nee_ft", icon = icon("chart-area")),
                         menuItem("NEE Scatter", tabName = "nee_sct", icon = icon("tree")),
                         menuItem("NEE Error", tabName = "nee_err", icon = icon("tree")),
                         menuItem("LE Forecast", tabName = "le_ft", icon = icon("chart-area")),
                         menuItem("LE Scatter", tabName = "le_sct", icon = icon("tree")),
                         menuItem("LE Error", tabName = "le_err", icon = icon("tree"))
                       ),
                       br(),
                       actionButton("run_button", "Run Analysis", icon = icon("play"))
                     ),

                     dashboardBody(
                       tabItems(
                         #Map Tab
                         tabItem(tabName = "nee_ft",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_ft_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "nee_sct",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_sct_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "nee_err",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("nee_err_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_ft",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_ft_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_sct",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_sct_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "le_err",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("le_err_plot")
                                   )
                                 ))
                         )
                       )
                     )
)