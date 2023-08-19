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
                       selectInput("variable", label="Select Variable", choices=c(not_sel)),
                       sidebarMenu(
                         menuItem("Forecast Plot", tabName = "ft_plt", icon = icon("chart-area")),
                         menuItem("Scatter Plot", tabName = "sct_plt", icon = icon("tree")),
                         menuItem("Error Plot", tabName = "err_plt", icon = icon("tree"))
                       ),
                       br(),
                       actionButton("run_button", "Run Analysis", icon = icon("play"))
                     ),

                     dashboardBody(
                       tabItems(
                         #Map Tab
                         tabItem(tabName = "ft_plt",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("ft_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "sct_plt",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("sct_plot")
                                   )
                                 ))
                         ),
                         tabItem(tabName = "err_plt",
                                 fluidRow(div(
                                   style = "margin: 10px;",
                                   tags$div(
                                     style = "border: 5px solid black; border-radius: 5px;",
                                     plotlyOutput("err_plot")
                                   )
                                 ))
                         )
                       )
                     )
)
