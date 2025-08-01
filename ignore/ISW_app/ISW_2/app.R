library(patchwork)
library(units)
library(dplyr)
library(ggplot2)
library(markdown)
library(remotes)
library(tidyr)
library(expint)
#library(isw)
files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

ui <- fluidPage(
  titlePanel("ISW Scenario Exploration"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("
        .input-label { font-size: 11px; }
        .shiny-input-container { margin-bottom: 5px; }
        legend { font-size: 12px; font-weight: bold; }
        .control-label { font-size: 12px; }
      ")),
      fluidRow(
        column(6,
               tags$fieldset(
                 tags$legend("Grid Settings"),
                 numericInput("x_grid", "X coordinate center of grid:", 0.5, min = 0),
                 numericInput("y_grid", "Y coordinate center of grid:", 0, min = 0),
                 numericInput("grid_size", "Grid Size (wells in each direction):", 2, min = 1),
                 numericInput("spacing", "Well Spacing [mi]:", 0.1, min = 0.01, step = 0.01)
               ),
               tags$fieldset(
                 tags$legend("Observation Settings"),
                 numericInput("num_obs", "Number of Observation Wells:", 1, min = 1),
                 uiOutput("obs_coordinates")
               )
        ),
        column(6,
               tags$fieldset(
                 tags$legend("Aquifer Properties & Pumping Time"),
                 numericInput("D", "Aquifer Thickness (D) [ft]:", 200, min = 0),
                 numericInput("K", "Hydraulic Conductivity (K) [ft/sec]:", 0.001, min = 0, step = 0.0001),
                 numericInput("V", "Specific Yield (V):", 0.2, min = 0, max = 1, step = 0.01),
                 numericInput("year_x", "Time (years):", 1, min = 0, step = 0.1)
               )
        ),
        column(6,
               tags$fieldset(
                 tags$legend("Criteria"),
                 numericInput("threshold", "Criteria Threshold:", 0.7, min = 0, max = 1, step = 0.1)
               )
        )
      )
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

server <- function(input, output) {
  # Dynamic UI for observation well coordinates
  output$obs_coordinates <- renderUI({
    num_obs <- input$num_obs
    well_inputs <- lapply(seq_len(num_obs), function(i) {
      fluidRow(
        column(6, numericInput(paste0("x_obs_", i), paste("X Coordinate for Well", i, ":"), value = 0.5)),
        column(6, numericInput(paste0("y_obs_", i), paste("Y Coordinate for Well", i, ":"), value = 0))
      )
    })
    do.call(tagList, well_inputs)
  })
  
  # Reactive observation well data
  obsWells <- reactive({
    req(input$num_obs)  # Ensure num_obs is non-null
    num_obs <- input$num_obs
    # print(input[[paste0("x_obs_", 1)]])
    well_coords <- lapply(seq_len(num_obs), function(i) {
      tibble(
        x2 = as.double(input[[paste0("x_obs_", i)]]),
        y_obs = as.double(input[[paste0("y_obs_", i)]]),
        `Obs. well` = paste("Obs. Well", i)
      )
    })
    bind_rows(well_coords)
  })
  
  gridData <- reactive({
    grid_size <- input$grid_size
    spacing <- input$spacing
    obs_wells <- obsWells()
    # print(obs_wells)
    pump_wells <- expand.grid(
      x1 = seq(input$x_grid - grid_size * spacing, input$x_grid + grid_size * spacing, by = spacing),
      y_pump = seq(input$y_grid - grid_size * spacing, input$y_grid + grid_size * spacing, by = spacing)
    ) |> 
      as_tibble() |> 
      mutate(`Pumping well` = as.character(row_number())) |> filter(x1 >= 0)
   
   crossing(pump_wells, obs_wells) |> 
      mutate(
        y = abs(y_pump - y_obs),
        across(all_of(c("x1", "x2", "y")),  function(x) set_units(x,"mi")),
        id = row_number() 
      )
  })
  
  
  # Depletion Data
  
  depletionData <- reactive({
    locations <- gridData()
    print(gridData)
    year <- set_units(input$year_x, "year")
    D <- set_units(input$D, 'ft')
    K <- set_units(input$K, 'ft/sec')
    
    df <- locations |> mutate(K = K, D = D, V = input$V, t = year)
    depletion <- get_depletion_from_pumping(df)  # Replace with actual function
    df_calcs <- df |> bind_cols(as_tibble(depletion))
    df_calcs <- df_calcs |> mutate(criteria =
                                     if_else(between(stream_depletion_fraction / drop_units(aquifer_drawdown_ratio * -1), 0, input$threshold),1,0))
  })
  
  
  # Update plots (same as before)
  output$gridPlot <- renderPlot({
    df_calcs <- depletionData()
    ggplot() +
      geom_point(data = df_calcs, aes(x1, y_pump, color = stream_depletion_fraction), size = 3) +
      geom_point(data = df_calcs, aes(x2, y_obs, shape = `Obs. well`),fill=NA, size = 4) +
      viridis::scale_color_viridis(direction=-1) +
      labs(x = "x coords", y = "y coords [mi]", title = "Fracrtion stream flow depletion",color="Fraction") +
      # scale_color_discrete("Well type") + 
      theme_bw() + 
      theme(axis.line.y = element_line(color = "lightblue", size = 4)) +
      coord_equal() 
  })
  
  # observe({
  #   assign("saved_dataframe", depletionData(), envir = .GlobalEnv)
  # })
  # 
  output$criteriaPlot <- renderPlot({
    df_calcs <- depletionData()
    df_calcs <- df_calcs |> 
      mutate(criteria_label = if_else(criteria == 1, "Yes", "No"))
    
    df_calcs <- df_calcs |>  group_by(`Pumping well`) |> 
      filter(!(1 %in% criteria) | criteria == 1) |> 
      ungroup()
    
    ggplot() +
      geom_point(data = df_calcs, aes(x1, y_pump, color = criteria_label), size = 3) + 
      geom_point(data = df_calcs, aes(x2, y_obs, shape = `Obs. well`), size = 4) +
      # scale_shape_manual(values = c(0, 5)) +
      labs(x = "x coords", y = "y coords [mi]", title = "Observation well and pumping wells grid w criteria",colour="Met criteria") +
      # scale_color_discrete("Well type") + 
      theme_bw()   + 
      theme(axis.line.y = element_line(color = "lightblue", size = 4)) +
      coord_equal() + 
      scale_color_manual(
        values = c("Yes" = "purple3",  # Define the color for "Met criteria"
                   "No" = "gray",
                   na.value = NA)  # Hide points that don't meet the criteria
      )})
  
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

shinyApp(ui = ui, server = server)
