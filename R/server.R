#' The server function for the shiny application
#'
server <- function(input, output) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  max_plots <- 6

  data <- reactive({
    req(input$csv_input)
    paste0(input$csv_input$datapath)
    df <- read.csv(input$csv_input$datapath)
    df$datetime <-
      as.POSIXct(df$datetime)  # Convert datetime column to POSIXct object
    df$time <- format(df$datetime, format = "%H:%M:%S")
    df$error <- abs(df$observation - df$mean)
    return(df)
  })

  s3_data <- reactive({
    req(input$s3_input)
    return(input$s3_input)
  })

  sda_data <- reactive({
    req(input$sda_input)
    e = new.env()
    sda_data <- load(input$sda_input$datapath, envir = e)
    return(e)
  })

  observeEvent(s3_data(), {
    site_id_choices <-
      arrow::open_dataset(s3_data(), format = "parquet") |> dplyr::distinct(site_id) |> dplyr::collect()

    site_id_choices <- c(unique(data()$site_id))
    start_date_choices <- c(unique(data()$reference_datetime))
    variable_choices <- c(unique(data()$variable))
    updateCheckboxGroupInput(inputId = "site_id",
                             choices = site_id_choices,
                             selected = site_id_choices)
    updateSelectInput(inputId = "start_date", choices = start_date_choices)
    updateSelectInput(inputId = "variable", choices = variable_choices)
  })

  observeEvent(data(), {
    site_id_choices <- c(unique(data()$site_id))
    start_date_choices <- c(unique(data()$reference_datetime))
    variable_choices <- c(unique(data()$variable))
    updateCheckboxGroupInput(inputId = "site_id",
                             choices = site_id_choices,
                             selected = site_id_choices)
    updateSelectInput(inputId = "start_date", choices = start_date_choices)
    updateSelectInput(inputId = "variable", choices = variable_choices)
  })


  observeEvent(sda_data(), {
    e <- sda_data()
    FORECAST <- e[['FORECAST']]
    sda_site_id_choices <- unique(attributes(FORECAST[[1]])$Site)
    sda_time_point_choices <- names(FORECAST)
    sda_variable_choices <-
      unique(attributes(FORECAST[[1]])$dimnames[[2]])
    updateCheckboxGroupInput(inputId = "sda_site_id",
                             choices = sda_site_id_choices,
                             selected = NULL)
    updateCheckboxGroupInput(inputId = "sda_time_points",
                             choices = sda_time_point_choices,
                             selected = NULL)
    updateCheckboxGroupInput(inputId = "sda_variables",
                             choices = sda_variable_choices,
                             selected = NULL)
  })

  site_id <- eventReactive(input$run_button, input$site_id)
  start_date <- eventReactive(input$run_button, input$start_date)
  variable <- eventReactive(input$run_button, input$variable)
  sda_site_ids <-
    eventReactive(input$run_sda_button, input$sda_site_id)
  sda_time_points <-
    eventReactive(input$run_sda_button, input$sda_time_points)
  sda_var_names <-
    eventReactive(input$run_sda_button, input$sda_variables)

  # Forecast Plot
  forecast_plot <-
    function(forecast_data,
             input_site,
             start_date,
             input_variable) {
      forecast_data <-
        filter(
          forecast_data,
          variable == input_variable &
            site_id == input_site &
            reference_datetime == start_date & observation != "NA"
        )

      ggplot(forecast_data, aes(x = datetime)) +
        geom_ribbon(
          aes(
            ymin = quantile02.5,
            ymax = quantile97.5,
            fill = "95% Confidence Interval"
          ),
          alpha = 0.4
        ) +
        geom_line(aes(y = mean, color = "Predicted")) +
        geom_line(aes(y = observation, color = "Observed Data"), size = 1) +
        ggtitle(paste0(
          "Forecast Horizon for Site: ",
          paste(input_site, collapse = ", ")
        )) +
        scale_color_manual(
          name = "Legend",
          labels = c("Observed Data", "Predicted"),
          values = c(
            "Observed Data" = "firebrick4",
            "Predicted" = "skyblue1"
          )
        ) +
        scale_fill_manual(
          labels = c("95% Confidence Interval"),
          values = c("95% Confidence Interval" = "blue1")
        ) +
        scale_y_continuous(switch(
          input_variable,
          "le" = "LE (W m-2 s-1)",
          "nee" = "NEE (kg C m-2 s-1)",
          # Add cases for other variables as needed
          "(kg C m-2 s-1)"
        )) +
        scale_x_datetime(
          name = "Date and Time",
          date_labels = "%Y-%m-%d",
          breaks = unique(as.POSIXct(as.Date(
            forecast_data$datetime
          ))),
          labels = format(unique(as.POSIXct(
            as.Date(forecast_data$datetime)
          )), "%Y-%m-%d"),
          guide = guide_axis(n.dodge = 1)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.text.x = element_text(
            size = 10,
            angle = 45,
            hjust = 1,
            vjust = 1
          ),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 12)
        )
    }

  # Scatter Plot
  scatter_plot <-
    function(scatter_data,
             input_site,
             start_date,
             input_variable) {
      scatter_data <-
        filter(
          scatter_data,
          variable == input_variable &
            site_id == input_site &
            reference_datetime == start_date & observation != "NA"
        )
      scatter_data$E <- scatter_data$mean
      scatter_data$O <- scatter_data$observation
      all <- c(scatter_data$E, scatter_data$O)
      RMSE <-
        sqrt(mean((scatter_data$E - scatter_data$O) ^ 2, na.rm = TRUE))
      Bias <- mean(scatter_data$E - scatter_data$O, na.rm = TRUE)

      # Predicted vs Observed Scatter + 1:1 line + regression
      ggplot(scatter_data, aes(x = E, y = O)) +
        geom_point(size = 1) +
        geom_line(
          data = data.frame(x = c(
            min(all, na.rm = TRUE), max(all, na.rm = TRUE)
          ),
          y = c(
            min(all, na.rm = TRUE), max(all, na.rm = TRUE)
          )),
          aes(x = x, y = y),
          color = "darkgrey",
          size = 2,
          linetype = "solid"
        ) +
        labs(
          x = "Predicted",
          y = "Observed",
          title = paste0("Scatter Plot for ", input_site)
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none") +
        annotate(
          "text",
          x = 0.05,
          y = 0.9,
          label = paste0("RMSE = ", formatC(
            RMSE, format = "e", digits = 2
          )),
          xref = "paper",
          yref = "paper"
        )
    }

  #Error Plot
  error_plot <-
    function(error_data,
             input_site,
             start_date,
             input_variable) {
      error_data <-
        filter(
          error_data,
          variable == input_variable &
            site_id == input_site &
            reference_datetime == start_date & observation != "NA"
        )

      ggplot(error_data, aes(x = datetime, y = error, group = 1)) +
        geom_point(aes(color = time), size = 1) +
        geom_hline(yintercept = 0, color = "black") +
        xlab("Date") +
        ylab(switch(
          input_variable,
          "le" = "LE Error (kg C m-2 s-1)",
          "nee" = "NEE Error (kg C m-2 s-1) ",
          # Add cases for other variables as needed
          "(kg C m-2 s-1)"
        )) +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    }

  # SDA Analysis Plot
  sda_analysis_plot <-
    function(sda_data,
             sda_site_ids,
             sda_var_names,
             sda_time_points) {
      e <- sda_data()
      FORECAST <- e[['FORECAST']]
      ANALYSIS <- e[['ANALYSIS']]
      types <- c("FORECAST", "ANALYSIS", "OBS")
      CI <- c(0.025, 0.975)
      unit <-
        list(
          AbvGrndWood = "Mg/ha",
          LAI = "m2/m2",
          SoilMoistFrac = "",
          TotSoilCarb = "kg/m2"
        )
      style <-
        list(
          general_color = c(
            "FORECAST" = "blue",
            "ANALYSIS" = "red",
            "OBS" = "black"
          ),
          fill_color = c(
            "FORECAST" = "yellow",
            "ANALYSIS" = "green",
            "OBS" = "grey"
          ),
          title_color = "red"
        )

      diag_fix <- function(vector) {
        if (length(vector) > 1) {
          return(diag(vector))
        } else if (length(vector) == 1) {
          return(vector)
        }
      }

      pft <- utils::read.csv("site_pft.csv")

      DB <- data.frame()
      obs.mean <- NULL
      obs.cov <- NULL

      for (id in sort(unique(sda_site_ids))) {
        for (time_point in sda_time_points) {
          for (var_name in sort(unique(sda_var_names))) {
            for (type in types) {
              if (type == "OBS") {
                obs_mean <- obs.mean[[time_point]][[id]][[var_name]]
                if (length(obs_mean) == 0 | is.null(obs_mean)) {
                  next
                } else{
                  obs_cov <-
                    diag_fix(obs.cov[[time_point]][[id]])[which(var_name == names(obs.mean[[time_point]][[id]]))]
                  MIN <- obs_mean - 1.96 * sqrt(obs_cov)
                  MAX <- obs_mean + 1.96 * sqrt(obs_cov)
                  MEAN <- obs_mean
                }
              } else {
                temp_Dat <- get(type)[[time_point]]
                site_ind <- which(id == sda_site_ids)
                var_ind <- which(var_name == sda_var_names)
                ind <- var_ind[which(var_ind %in% site_ind)]
                MEAN <- mean(temp_Dat[, ind])
                MIN <- stats::quantile(temp_Dat[, ind], CI[1])
                MAX <- stats::quantile(temp_Dat[, ind], CI[2])
              }
              if (MIN < 0 || is.na(MIN))
                MIN <- 0
              DB <-
                rbind(
                  DB,
                  list(
                    id = id,
                    date = time_point,
                    var_name = var_name,
                    type = type,
                    upper = MAX,
                    lower = MIN,
                    mean = MEAN
                  )
                )
            }
          }
        }
      }
    }

  # Forecast Plot
  ft_plot <- eventReactive(input$run_button, {
    forecast_plot(data(), site_id()[1], start_date(), variable())
  })


  output$ft_plot <- renderPlotly(ft_plot())

  # Scatter Plot
  sct_plot <- eventReactive(input$run_button, {
    scatter_plot(data(), site_id()[1], start_date(), variable())
  })

  output$sct_plot <- renderPlotly(sct_plot())

  # Error plot
  err_plot <- eventReactive(input$run_button, {
    error_plot(data(), site_id()[1], start_date(), variable())
  })

  output$err_plot <- renderPlotly(err_plot())


  # Forecasting Multi Grid Plot

  output$ft_grid_plot <- renderUI({
    plot_output_list <- lapply(1:length(site_id()), function(i) {
      plotname <- paste("ft_grid_plot", i, sep = "")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
  })

  for (i in 1:5) {
    local({
      my_i <- i
      plotname <- paste("ft_grid_plot", my_i, sep = "")

      output[[plotname]] <- renderPlot({
        forecast_plot(data(), site_id()[my_i], start_date(), variable())
      })
    })
  }

  # Error Multi Grid Plot

  output$err_grid_plot <- renderUI({
    plot_output_list <- lapply(1:length(site_id()), function(i) {
      plotname <- paste("err_grid_plot", i, sep = "")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
  })

  for (i in 1:5) {
    local({
      my_i <- i
      plotname <- paste("err_grid_plot", my_i, sep = "")

      output[[plotname]] <- renderPlot({
        error_plot(data(), site_id()[my_i], start_date(), variable())
      })
    })
  }

  # Scatter Multi Grid Plot

  output$sct_grid_plot <- renderUI({
    plot_output_list <- lapply(1:length(site_id()), function(i) {
      plotname <- paste("sct_grid_plot", i, sep = "")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
  })

  for (i in 1:5) {
    local({
      my_i <- i
      plotname <- paste("sct_grid_plot", my_i, sep = "")

      output[[plotname]] <- renderPlot({
        scatter_plot(data(), site_id()[my_i], start_date(), variable())
      })
    })
  }

  #-----SDA Plot-----------------

  output$sda_plot <- renderUI({
    plot_output_list <- lapply(1:length(sda_site_ids()), function(i) {
      plotname <- paste("sda_plot", i, sep = "")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
  })

  for (i in 1:max_plots) {
    local({
      my_i <- i
      plotname <- paste("sda_plot", my_i, sep = "")

      output[[plotname]] <- renderPlot({
        sda_analysis_plot(sda_data(),sda_site_ids()[my_i],sda_var_names(),sda_time_points())
      })
    })
  }
}
