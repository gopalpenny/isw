library(patchwork)
library(units)
library(dplyr)
library(ggplot2)
library(markdown)
library(tidyr)
library(expint)

# Load custom functions
files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

# Define UI
ui <- fluidPage(
  titlePanel("ISW Scenario Exploration"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x2", "Default Obs. Well Distance from Stream (x2) [mi]", 0.5, min = 0),
      numericInput("grid_size", "Grid Size (number of wells in each direction):", 2, min = 1),
      numericInput("spacing", "Well Spacing [mi]:", 0.1, min = 0.01, step = 0.01),
      numericInput("D", "Aquifer Thickness (D) [ft]:", 200, min = 0),
      numericInput("K", "Hydraulic Conductivity (K) [ft/sec]:", 0.001, min = 0, step = 0.0001),
      numericInput("V", "Specific Yield (V):", 0.2, min = 0, max = 1, step = 0.01),
      numericInput("year_x", "Time (years):", 1, min = 0, step = 0.1),
      numericInput("threshold", "Criteria Threshold:", 0.7, min = 0, max = 1, step = 0.1),
      textInput("obs_coords", "Additional Obs. Wells (x,y) [comma-separated, e.g., 0.6,0.3]", ""),
      actionButton("update_obs", "Add Observation Wells")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Grid Visualization", plotOutput("gridPlot")),
        tabPanel("Grid Criteria Visualization", plotOutput("criteriaPlot")),
        tabPanel("Depletion vs Drawdown", plotOutput("depletionPlot")),
        tabPanel("About", fluidRow(column(12, includeMarkdown("about.md"))))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive to store observation wells
  obs_wells <- reactiveVal(tibble(y_obs = 0, x2 = input$x2, `Obs. well` = "a"))
  
  observeEvent(input$update_obs, {
    coords <- strsplit(input$obs_coords, ",")[[1]]
    if (length(coords) %% 2 == 0) {
      new_obs <- matrix(as.numeric(coords), ncol = 2, byrow = TRUE) |>
        as_tibble() |>
        rename(x2 = V1, y_obs = V2) |>
        mutate(`Obs. well` = paste0("b", row_number()))
      obs_wells(bind_rows(obs_wells(), new_obs))
    } else {
      showNotification("Invalid coordinates format. Use comma-separated values: x1,y1,x2,y2,...", type = "error")
    }
  })
  
  gridData <- reactive({
    grid_size <- input$grid_size
    spacing <- input$spacing
    
    obs_data <- obs_wells()
    pump_wells <- expand.grid(
      x1 = seq(min(obs_data$x2) - grid_size * spacing, max(obs_data$x2) + grid_size * spacing, by = spacing),
      y_pump = seq(min(obs_data$y_obs) - grid_size * spacing, max(obs_data$y_obs) + grid_size * spacing, by = spacing)
    ) |> 
      as_tibble() |> 
      mutate(`Pumping well` = as.character(row_number()))
    
    crossing(pump_wells, obs_data) |>
      mutate(
        y = abs(y_pump) - y_obs,
        across(all_of(c("x1", "x2", "y")), function(x) set_units(x, "mi")),
        id = row_number()
      )
  })
  
  depletionData <- reactive({
    locations <- gridData()
    
    year <- set_units(input$year_x, "year")
    D <- set_units(input$D, 'ft')
    K <- set_units(input$K, 'ft/sec')
    
    df <- locations |> mutate(K = K, D = D, V = input$V, t = year)
    depletion <- get_depletion_from_pumping(df)  # Replace with actual function
    df_calcs <- df |> bind_cols(as_tibble(depletion))
    df_calcs |> mutate(criteria = 
                         if_else(between(stream_depletion_fraction / drop_units(aquifer_drawdown_ratio * -1), 0, input$threshold), 1, 0))
  })
  
  output$gridPlot <- renderPlot({
    df_calcs <- depletionData()
    ggplot() +
      geom_point(data = df_calcs, aes(x1, y_pump, color = `Pumping well`), size = 4) +
      geom_point(data = df_calcs, aes(x2, y_obs, shape = `Obs. well`), size = 4) +
      labs(x = "x coords", y = "y coords [mi]", title = "Observation well and pumping wells grid") +
      scale_shape_manual(values = c(0, 1)) +
      theme_bw() +
      coord_equal() +
      guides(color = "none")
  })
  
  output$criteriaPlot <- renderPlot({
    df_calcs <- depletionData()
    ggplot() +
      geom_point(data = df_calcs, aes(x1, y_pump, color = as.factor(criteria)), size = 4) +
      geom_point(data = df_calcs, aes(x2, y_obs, shape = `Obs. well`), size = 4) +
      labs(
        x = "x coords", 
        y = "y coords [mi]", 
        title = "Observation well and pumping wells grid w criteria",
        color = "Meets Criteria"
      ) +
      scale_color_manual(values = c("1" = "purple3", "0" = "gray")) +
      scale_shape_manual(values = c(0, 1)) +
      theme_bw() +
      coord_equal()
  })
  
  output$depletionPlot <- renderPlot({
    df_calcs <- depletionData()
    ggplot(df_calcs, aes(as.numeric(-aquifer_drawdown_ratio), as.numeric(stream_depletion_fraction), shape = as.factor(y))) +
      geom_point(
        data = df_calcs |> filter(abs(as.numeric(t)) %% 0.5 < 0.15),
        aes(color = as.factor(x1)), size = 3, stroke = 1
      ) +
      labs(
        x = "Drawdown (ft) per\ncusec pumping",
        y = "Fraction of well abstraction\nfrom stream depletion"
      ) +
      theme_bw() +
      geom_abline(slope = input$threshold, intercept = 0)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
