library(shiny)
library(shinydashboard)
library(plotly)
library(reshape2)
library(gridExtra)
library(leaflet)

not_sel <- "Not Selected"

ui <- navbarPage("Flux Dashboard", theme = "flatly",
                 tabPanel("Forecasting Dashboard",
                          sidebarLayout(
                            sidebarPanel(
                              # textInput("s3_input", label="Send the S3 URI"),
                              fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
                              checkboxGroupInput("site_id", label = "Select Sites to plot:", choices = NULL),
                              selectInput("start_date", label = "Forecast Horizon Date:", choices = c(not_sel)),
                              selectInput("variable", label = "Select Variable", choices = c(not_sel)),
                              actionButton("run_button", "Run Analysis", icon = icon("play")
                              )),
                            mainPanel(
                              tabsetPanel(
                                # tabPanel("Forecast Map Plot",
                                #          fluidRow(div(
                                #            style = "margin: 10px;",
                                #            tags$div(
                                #              style = "border: 5px solid black; border-radius: 5px;",
                                #              leafletOutput("mymap")
                                #            )
                                #          )),
                                # ),
                                tabPanel("Forecast Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             plotlyOutput("ft_plot")
                                           )
                                         )),
                                ),
                                tabPanel("Scatter Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             plotlyOutput("sct_plot")
                                           )
                                         )),
                                ),
                                tabPanel("Error Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             plotlyOutput("err_plot")
                                           )
                                         )),
                                ),
                                tabPanel("Forecast Grid Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             uiOutput("ft_grid_plot")
                                           )
                                         ))
                                ),
                                tabPanel("Error Grid Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             uiOutput("err_grid_plot")
                                           )
                                         ))
                                ),
                                tabPanel("Scatter Grid Plot",
                                         fluidRow(div(
                                           style = "margin: 10px;",
                                           tags$div(
                                             style = "border: 5px solid black; border-radius: 5px;",
                                             uiOutput("sct_grid_plot")
                                           )
                                         ))
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("SDA Dashboard",sidebarLayout(
                   sidebarPanel(
                     fileInput("sda_input", "Select Rdata File to Import", accept = ".Rdata"),
                     checkboxGroupInput("sda_site_id", label = "Select Sites to plot:", choices = NULL),
                     checkboxGroupInput("sda_time_points", label = "Select Time points to plot:", choices = NULL),
                     checkboxGroupInput("sda_variables", label = "Select Variables to plot:", choices = NULL),
                     actionButton("run_sda_button", "Run SDA Analysis", icon = icon("play")
                     )),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("SDA Plot",
                                fluidRow(div(
                                  style = "margin: 10px;",
                                  tags$div(
                                    style = "border: 5px solid black; border-radius: 5px;",
                                    uiOutput("sda_plot")
                                  )
                                )),
                       ))))
                       )
)
