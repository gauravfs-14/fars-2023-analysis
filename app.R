library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(viridis)
library(tidyr)
library(scales)
library(stringr)
library(readr)

# UI
ui <- dashboardPagePlus(
  skin = "purple",
  
  # Header
  dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "FARS 2023 Data"),
      span(class = "logo-mini", "FARS")
    ),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("map-marker")),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("clock")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Environmental Factors", tabName = "environmental", icon = icon("cloud-sun")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    
    # Filters (common across all tabs)
    tags$div(
      class = "sidebar-filters",
      tags$h4("Filters", style = "margin-left: 15px;"),
      
      conditionalPanel(
        condition = "input.sidebar != 'data'",
        pickerInput(
          "year_filter", 
          "Year:",
          choices = NULL, 
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          "state_filter", 
          "State:",
          choices = NULL, 
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          "pbtype_filter", 
          "Person Type:",
          choices = NULL, 
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          "weather_filter", 
          "Weather:",
          choices = NULL, 
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        actionButton("reset_filters", "Reset Filters", 
                     icon = icon("refresh"),
                     style = "margin: 15px;")
      )
    )
  ),
  
  # Main panel
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .info-box {transition: all 0.3s ease;}
        .info-box:hover {transform: translateY(-5px); box-shadow: 0 5px 15px rgba(0,0,0,0.1);}
        .box {border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05);}
        .box:hover {box-shadow: 0 5px 15px rgba(0,0,0,0.08);}
        .leaflet-container {border-radius: 10px !important;}
        .dataTables_wrapper {padding: 15px;}
        .sidebar-filters {padding: 10px 0;}
        .chart-title {font-size: 18px; font-weight: 500; margin-bottom: 15px; color: #444;}
        .small-box {border-radius: 8px; transition: all 0.3s ease;}
        .small-box:hover {transform: translateY(-5px); box-shadow: 0 5px 15px rgba(0,0,0,0.1);}
        .nav-tabs-custom {border-radius: 10px; overflow: hidden;}
      "))
    ),
    
    tabItems(
      # Overview tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_crashes_box", width = 3),
          valueBoxOutput("total_states_box", width = 3),
          valueBoxOutput("pedestrian_count_box", width = 3),
          valueBoxOutput("bicycle_count_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Crash Trend Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("crash_trend_plot", height = 300)
          ),
          box(
            title = "Crash Distribution by Type",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("crash_type_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Cities with Most Crashes",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_cities_plot", height = 300) 
          ),
          box(
            title = "Crashes by Light Condition",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("light_condition_plot", height = 300)
          )
        )
      ),
      
      # Geographic Analysis tab
      tabItem(
        tabName = "geographic",
        fluidRow(
          box(
            title = "Crash Location Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("crash_map", height = 600),
            footer = "Click on points to see crash details"
          )
        ),
        
        fluidRow(
          box(
            title = "Crashes by Rural/Urban Classification",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rural_urban_plot", height = 300)
          ),
          box(
            title = "Crashes by Functional Road System",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("road_system_plot", height = 300)
          )
        )
      ),
      
      # Temporal Analysis tab
      tabItem(
        tabName = "temporal",
        fluidRow(
          box(
            title = "Crashes by Month",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("month_plot", height = 300)
          ),
          box(
            title = "Crashes by Day of Week",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("day_week_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Crashes by Hour of Day",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hour_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Heat Calendar of Crashes",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("calendar_plot", height = 400)
          )
        )
      ),
      
      # Demographics tab
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            title = "Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("age_distribution_plot", height = 900)
          )
         
        ),
        
        fluidRow(
          box(
            title = "Gender Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("gender_distribution_plot", height = 400)
          ),
          box(
            title = "Person Position at Time of Crash",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("position_plot", height = 300)
          ),
          box(
            title = "Person Direction at Time of Crash",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("direction_plot", height = 300)
          )
        )
      ),
      
      # Environmental Factors tab
      tabItem(
        tabName = "environmental",
        fluidRow(
          box(
            title = "Weather Conditions",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("weather_plot", height = 300)
          ),
          box(
            title = "Light Conditions",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("lighting_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Crash Factors Correlation",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("correlation_plot", height = 400),
            footer = "Correlation between environmental factors and crash frequency"
          )
        ),
        
        fluidRow(
          box(
            title = "School Zone Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("school_zone_plot", height = 300)
          ),
          box(
            title = "Crosswalk Presence",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("crosswalk_plot", height = 300)
          )
        )
      ),
      
      # Raw Data tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Raw Data Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(3, pickerInput("search_column", "Search Column:", choices = NULL)),
              column(3, textInput("search_text", "Search Text:")),
              column(3, selectInput("rows_per_page", "Rows per page:", 
                                    choices = c(10, 25, 50, 100), selected = 25)),
              column(3, downloadButton("download_data", "Download Data"))
            ),
            
            hr(),
            DTOutput("data_table"),
            height = 800
          )
        )
      )
    )
  ),
  
  # Right sidebar with dashboard info
  rightSidebar(
    background = "dark",
    width = 300,
    
    tags$h3("Dashboard Information", style = "padding: 15px;"),
    
    tabsetPanel(
      tabPanel("About",
               tags$div(
                 style = "padding: 15px;",
                 tags$h4("Pedestrian & Bicycle Crash Dashboard"),
                 tags$p("This dashboard analyzes pedestrian and bicycle crash data across multiple dimensions:"),
                 tags$ul(
                   tags$li("Geographic patterns"),
                   tags$li("Temporal trends"),
                   tags$li("Demographic factors"),
                   tags$li("Environmental conditions"),
                   tags$li("Raw data exploration")
                 ),
                 tags$p("Use the filters in the sidebar to explore specific subsets of data.")
               )
      ),
      tabPanel("Help",
               tags$div(
                 style = "padding: 15px;",
                 tags$h4("Quick Tips"),
                 tags$ul(
                   tags$li("Click on map points to see crash details"),
                   tags$li("Hover over charts for detailed information"),
                   tags$li("Use filters to narrow down analysis"),
                   tags$li("Download filtered data from the Raw Data tab")
                 )
               )
      ),
      tabPanel("Legend",
               tags$div(
                 style = "padding: 15px;",
                 tags$h4("Map Markers"),
                 tags$ul(
                   tags$li(tags$span(style = "color: red;", "●"), " Pedestrian Crash"),
                   tags$li(tags$span(style = "color: blue;", "●"), " Bicycle Crash"),
                   tags$li(tags$span(style = "color: purple;", "●"), " Other/Multiple")
                 ),
                 
                 tags$h4("Chart Colors"),
                 tags$p("This dashboard uses a consistent color scheme:"),
                 tags$ul(
                   tags$li(tags$span(style = "color: #440154;", "■"), " Highest values"),
                   tags$li(tags$span(style = "color: #21908C;", "■"), " Medium values"),
                   tags$li(tags$span(style = "color: #FDE725;", "■"), " Lowest values")
                 )
               )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  crash_data <- reactive({
    req(file.exists("crash_data.csv"))
    
    data <- read_csv("crash_data.csv", show_col_types = FALSE)
    
    # Force numeric conversion and filter out invalid coordinates
    data <- data %>%
      mutate(
        LATITUDENAME = suppressWarnings(as.numeric(trimws(LATITUDENAME))),
        LONGITUDNAME = suppressWarnings(as.numeric(trimws(LONGITUDNAME)))
      ) %>%
      filter(
        !is.na(LATITUDENAME),
        !is.na(LONGITUDNAME),
        is.finite(LATITUDENAME),
        is.finite(LONGITUDNAME),
        between(LATITUDENAME, -90, 90),
        between(LONGITUDNAME, -180, 180)
      )
    
    return(data)
  })
  
  
  # Filtered data based on user selections
  filtered_data <- reactive({
    data <- crash_data()
    
    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      data <- data %>% filter(YEAR %in% input$year_filter)
    }
    
    if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
      data <- data %>% filter(STATENAME %in% input$state_filter)
    }
    
    if (!is.null(input$pbtype_filter) && length(input$pbtype_filter) > 0) {
      data <- data %>% filter(PBPTYPENAME %in% input$pbtype_filter)
    }
    
    if (!is.null(input$weather_filter) && length(input$weather_filter) > 0) {
      data <- data %>% filter(WEATHERNAME %in% input$weather_filter)
    }
    
    return(data)
  })
  
  # Initialize filter choices
  observe({
    data <- crash_data()
    
    updatePickerInput(session, "year_filter", choices = sort(unique(data$YEAR)))
    updatePickerInput(session, "state_filter", choices = sort(unique(data$STATENAME)))
    updatePickerInput(session, "pbtype_filter", choices = sort(unique(data$PBPTYPENAME)))
    updatePickerInput(session, "weather_filter", choices = sort(unique(data$WEATHERNAME)))
    
    # For the data table search dropdown
    updatePickerInput(session, "search_column", choices = names(data))
  })
  
  # Reset filters button
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "year_filter", selected = character(0))
    updatePickerInput(session, "state_filter", selected = character(0))
    updatePickerInput(session, "pbtype_filter", selected = character(0))
    updatePickerInput(session, "weather_filter", selected = character(0))
  })
  
  # Value boxes for the overview tab
  output$total_crashes_box <- renderValueBox({
    data <- filtered_data()
    
    valueBox(
      formatC(nrow(data), format = "d", big.mark = ","),
      "Total Crashes",
      icon = icon("car-crash"),
      color = "purple"
    )
  })
  
  output$total_states_box <- renderValueBox({
    data <- filtered_data()
    
    valueBox(
      length(unique(data$STATENAME)),
      "States",
      icon = icon("flag-usa"),
      color = "blue"
    )
  })
  
  output$pedestrian_count_box <- renderValueBox({
    data <- filtered_data()
    
    ped_count <- sum(grepl("Pedestrian", data$PBPTYPENAME, ignore.case = TRUE))
    
    valueBox(
      formatC(ped_count, format = "d", big.mark = ","),
      "Pedestrian Incidents",
      icon = icon("walking"),
      color = "teal"
    )
  })
  
  output$bicycle_count_box <- renderValueBox({
    data <- filtered_data()
    
    bike_count <- sum(grepl("Bicyclist|Bicycle|Bike", data$PBPTYPENAME, ignore.case = TRUE))
    
    valueBox(
      formatC(bike_count, format = "d", big.mark = ","),
      "Bicycle Incidents",
      icon = icon("bicycle"),
      color = "maroon"
    )
  })
  
  # Overview tab plots
  output$crash_trend_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(YEAR, MONTHNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(month_num = match(MONTHNAME, month.name),
             date = as.Date(paste(YEAR, month_num, "01", sep = "-"))) %>%
      arrange(date)
    
    p <- ggplot(data, aes(x = date, y = count, group = 1)) +
      geom_line(color = "#440154", size = 1) +
      geom_point(color = "#440154", size = 3) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$crash_type_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(PBPTYPENAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(PBPTYPENAME, count), y = count, fill = count,
                          text = paste("Type:", PBPTYPENAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$top_cities_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(CITYNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(CITYNAME, count), y = count, fill = count,
                          text = paste("City:", CITYNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$light_condition_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(LGT_CONDNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(LGT_CONDNAME, count), y = count, fill = count,
                          text = paste("Light Condition:", LGT_CONDNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$crash_map <- renderLeaflet({
    data <- filtered_data()
    
    library(tidyr)
    
    data <- data %>%
      mutate(
        loc = paste0(LATITUDENAME, ", ", LONGITUDNAME),
        date = paste0(MONTHNAME, " ", DAY, ", ", YEAR),
        time = HOURNAME,
        type = PBPTYPENAME,
        weather = WEATHERNAME,
        light = LGT_CONDNAME
      ) %>%
      replace_na(list(
        loc = "Unknown",
        date = "Unknown",
        time = "Unknown",
        type = "Unknown",
        weather = "Unknown",
        light = "Unknown"
      )) %>%
      mutate(
        popup_content = paste0(
          "<h3>Crash Details</h3><hr/>",
          "📍 <strong>Location:</strong> ", loc, "<br/>",
          "📅 <strong>Date:</strong> ", date, "<br/>",
          "⏰ <strong>Time:</strong> ", time, "<br/>",
          "🚶 <strong>Type:</strong> ", type, "<br/>",
          "🌤️ <strong>Weather:</strong> ", weather, "<br/>",
          "💡 <strong>Light Condition:</strong> ", light
        )
      )
    
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~as.numeric(LONGITUDNAME),
        lat = ~as.numeric(LATITUDENAME),
        color = ~ifelse(grepl("Pedestrian", type, ignore.case = TRUE), "red",
                        ifelse(grepl("Bike|Bicyclist", type, ignore.case = TRUE), "blue", "purple")),
        radius = 6,
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~popup_content,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue", "purple"),
        labels = c("Pedestrian", "Bicycle", "Other"),
        title = "Crash Type"
      )
  })
  

  
  output$rural_urban_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(RUR_URBNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(RUR_URBNAME, count), y = count, fill = count,
                          text = paste("Area Type:", RUR_URBNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$road_system_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(FUNC_SYSNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(FUNC_SYSNAME, count), y = count, fill = count,
                          text = paste("Road System:", FUNC_SYSNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Temporal Analysis tab
  output$month_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(month_order = match(MONTHNAME, month.name)) %>%
      group_by(MONTHNAME, month_order) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(month_order)
    
    p <- ggplot(data, aes(x = reorder(MONTHNAME, month_order), y = count, fill = count,
                          text = paste("Month:", MONTHNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$day_week_plot <- renderPlotly({
    day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    data <- filtered_data() %>%
      mutate(day_order = match(DAY_WEEKNAME, day_levels)) %>%
      group_by(DAY_WEEKNAME, day_order) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(day_order)
    
    p <- ggplot(data, aes(x = reorder(DAY_WEEKNAME, day_order), y = count, fill = count,
                          text = paste("Day:", DAY_WEEKNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$hour_plot <- renderPlotly({
    data <- filtered_data() %>%
      filter(HOUR != 99) %>%
      group_by(HOUR) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(HOUR)
    
    p <- ggplot(data, aes(x = HOUR, y = count, group = 1)) +
      geom_line(color = "#440154", size = 1) +
      geom_point(aes(color = count), size = 4) +
      scale_color_viridis() +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(x = "Hour of Day", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Calendar heatmap (simplified for this example)
  output$calendar_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(MONTHNAME, DAY) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(month_num = match(MONTHNAME, month.name))
    
    p <- ggplot(data, aes(x = DAY, y = reorder(MONTHNAME, -month_num), fill = count,
                          text = paste("Month:", MONTHNAME, "<br>Day:", DAY, "<br>Crashes:", count))) +
      geom_tile(color = "white") +
      scale_fill_viridis(name = "Crash Count") +
      labs(x = "Day of Month", y = "") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Demographics tab
  output$age_distribution_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(PBAGENAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(PBAGENAME, count), y = count, fill = count,
                          text = paste("Age Group:", PBAGENAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$gender_distribution_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(PBSEXNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(PBSEXNAME, count), y = count, fill = count,
                          text = paste("Gender:", PBSEXNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$position_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(position = coalesce(PEDPOSNAME, BIKEPOSNAME)) %>%
      filter(!is.na(position)) %>%
      group_by(position) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(position, count), y = count, fill = count,
                          text = paste("Position:", position, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$direction_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(direction = coalesce(PEDDIRNAME, BIKEDIRNAME)) %>%
      filter(!is.na(direction)) %>%
      group_by(direction) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(direction, count), y = count, fill = count,
                          text = paste("Direction:", direction, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Environmental Factors tab
  output$weather_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(WEATHERNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(WEATHERNAME, count), y = count, fill = count,
                          text = paste("Weather:", WEATHERNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$lighting_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(LGT_CONDNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(LGT_CONDNAME, count), y = count, fill = count,
                          text = paste("Light Condition:", LGT_CONDNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$correlation_plot <- renderPlotly({
    # Create a correlation plot between environmental factors and time factors
    data <- filtered_data() %>%
      select(WEATHERNAME, LGT_CONDNAME, HOUR, DAY_WEEKNAME, MONTHNAME) %>%
      mutate(
        weather_factor = as.integer(factor(WEATHERNAME)),
        light_factor = as.integer(factor(LGT_CONDNAME)),
        hour_factor = HOUR,
        day_factor = as.integer(factor(DAY_WEEKNAME, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))),
        month_factor = as.integer(factor(MONTHNAME, levels = month.name))
      ) %>%
      select(weather_factor, light_factor, hour_factor, day_factor, month_factor)
    
    # Calculate correlation matrix
    corr_matrix <- cor(data, use = "complete.obs")
    
    # Convert to data frame for plotting
    corr_data <- as.data.frame(as.table(corr_matrix))
    names(corr_data) <- c("Var1", "Var2", "value")
    
    # Map factor names back to readable labels
    corr_data <- corr_data %>%
      mutate(
        Var1 = factor(Var1, levels = c("weather_factor", "light_factor", "hour_factor", "day_factor", "month_factor"),
                      labels = c("Weather", "Light Condition", "Hour of Day", "Day of Week", "Month")),
        Var2 = factor(Var2, levels = c("weather_factor", "light_factor", "hour_factor", "day_factor", "month_factor"),
                      labels = c("Weather", "Light Condition", "Hour of Day", "Day of Week", "Month"))
      )
    
    # Create the heatmap
    p <- ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_viridis(limits = c(-1, 1), name = "Correlation") +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_text(aes(label = round(value, 2)), color = "white", size = 3)
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$school_zone_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(PBSZONENAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(PBSZONENAME, count), y = count, fill = count,
                          text = paste("School Zone:", PBSZONENAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$crosswalk_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(PBCWALKNAME) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    p <- ggplot(data, aes(x = reorder(PBCWALKNAME, count), y = count, fill = count,
                          text = paste("Crosswalk:", PBCWALKNAME, "<br>Crashes:", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis() +
      labs(x = "", y = "Number of Crashes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Raw Data table
  output$data_table <- renderDT({
    data <- filtered_data()
    
    # Filter based on search
    if (!is.null(input$search_column) && !is.null(input$search_text) && input$search_text != "") {
      search_column <- input$search_column
      search_text <- input$search_text
      
      data <- data %>%
        filter(grepl(search_text, get(search_column), ignore.case = TRUE))
    }
    
    datatable(
      data,
      options = list(
        pageLength = as.numeric(input$rows_per_page),
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "600px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        searchHighlight = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  return type === 'display' && data != null && data.length > 20 ?",
              "    '<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
              "}"
            )
          )
        )
      ),
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      filter = 'top',
      selection = 'none',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(data),
        backgroundColor = styleEqual(c(NA, ""), c("#f8f8f8", "#f8f8f8"))
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("crash_data_export_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Create sample data if no file exists (for demo purposes)
generate_sample_data <- function() {
  # Check if file exists
  if (!file.exists("crash_data.csv")) {
    # Create sample data
    set.seed(123)
    
    # Define possible values for categorical variables
    states <- c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Florida", "Georgia", 
                "Illinois", "Maryland", "Massachusetts", "Michigan", "New York", "Texas")
    
    cities <- list(
      "Alabama" = c("Birmingham", "Montgomery", "Mobile"),
      "Alaska" = c("Anchorage", "Fairbanks", "Juneau"),
      "Arizona" = c("Phoenix", "Tucson", "Scottsdale"),
      "California" = c("Los Angeles", "San Francisco", "San Diego"),
      "Colorado" = c("Denver", "Boulder", "Colorado Springs"),
      "Florida" = c("Miami", "Orlando", "Tampa"),
      "Georgia" = c("Atlanta", "Savannah", "Augusta"),
      "Illinois" = c("Chicago", "Springfield", "Peoria"),
      "Maryland" = c("Baltimore", "Annapolis", "Frederick"),
      "Massachusetts" = c("Boston", "Cambridge", "Worcester"),
      "Michigan" = c("Detroit", "Ann Arbor", "Grand Rapids"),
      "New York" = c("New York City", "Buffalo", "Albany"),
      "Texas" = c("Austin", "Houston", "Dallas")
    )
    
    pb_types <- c("Pedestrian", "Bicyclist", "Other Non-Motorist")
    age_groups <- c("0-4", "5-9", "10-15", "16-20", "21-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    sex <- c("Male", "Female", "Unknown")
    crosswalk <- c("Marked Crosswalk", "Unmarked Crosswalk", "No Crosswalk Present", "Unknown")
    school_zone <- c("School Zone", "Not a School Zone", "Unknown")
    sidewalk <- c("Sidewalk Present", "No Sidewalk", "Unknown")
    rur_urb <- c("Rural", "Urban", "Unknown")
    func_sys <- c("Interstate", "Principal Arterial", "Minor Arterial", "Collector", "Local", "Unknown")
    light_cond <- c("Daylight", "Dawn", "Dusk", "Dark - Lighted", "Dark - Not Lighted", "Unknown")
    weather <- c("Clear", "Cloudy", "Rain", "Snow", "Fog", "Other", "Unknown")
    ped_location <- c("Intersection", "Non-Intersection", "Driveway Access", "Shared-Use Path", "Unknown")
    bike_location <- c("Intersection", "Non-Intersection", "Bike Lane", "Sidewalk", "Unknown")
    ped_position <- c("In Roadway", "On Sidewalk", "Shoulder", "Median", "Unknown")
    bike_position <- c("In Roadway", "On Sidewalk", "Shoulder", "Bike Lane", "Unknown")
    direction <- c("North", "South", "East", "West", "Northeast", "Northwest", "Southeast", "Southwest", "Unknown")
    harm_ev <- c("Motor Vehicle In-Transport", "Pedalcycle", "Pedestrian", "Railway Train", "Animal", "Fixed Object", "Other")
    man_coll <- c("Front-to-Front", "Front-to-Rear", "Angle", "Sideswipe - Same Direction", "Sideswipe - Opposite Direction", "Other")
    sch_bus <- c("School Bus Related", "Not School Bus Related")
    
    # Generate 1000 sample records
    n_records <- 1000
    
    # Select states
    state_data <- sample(states, n_records, replace = TRUE)
    
    # Generate years spanning 5 years
    years <- sample(2018:2023, n_records, replace = TRUE)
    
    # Generate months and days
    months <- sample(1:12, n_records, replace = TRUE)
    days <- sapply(months, function(m) sample(1:days_in_month(m), 1))
    
    # Function to get days in a month
    days_in_month <- function(month) {
      if (month %in% c(4, 6, 9, 11)) return(30)
      if (month == 2) return(28)
      return(31)
    }
    
    # Generate hours
    hours <- sample(0:23, n_records, replace = TRUE)
    
    # Generate coordinates (approximate US bounds)
    latitudes <- runif(n_records, min = 25, max = 49)
    longitudes <- runif(n_records, min = -125, max = -65)
    
    # Create dataframe
    crash_data <- data.frame(
      STATE = substr(state_data, 1, 2),
      STATENAME = state_data,
      ST_CASE = sprintf("ST%06d", sample(1:999999, n_records, replace = FALSE)),
      YEAR = years,
      CRASH_NUM1 = sprintf("C%07d", sample(1:9999999, n_records, replace = FALSE)),
      VEH_NO = sample(1:5, n_records, replace = TRUE),
      PER_NO = sample(1:3, n_records, replace = TRUE),
      PBPTYPENAME = sample(pb_types, n_records, replace = TRUE, prob = c(0.6, 0.35, 0.05)),
      PBAGE = sample(1:90, n_records, replace = TRUE),
      PBAGENAME = sample(age_groups, n_records, replace = TRUE),
      PBSEXNAME = sample(sex, n_records, replace = TRUE),
      PBCWALKNAME = sample(crosswalk, n_records, replace = TRUE),
      PBSWALKNAME = sample(sidewalk, n_records, replace = TRUE),
      PBSZONENAME = sample(school_zone, n_records, replace = TRUE),
      stringsAsFactors = FALSE
    )
    
    # Add conditional columns based on person type
    crash_data$PEDCTYPENAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                      sample(c("Crossing - At marked crosswalk intersection", 
                                               "Crossing - At unmarked crosswalk intersection",
                                               "Crossing - Not at intersection",
                                               "Walking along roadway - With traffic",
                                               "Walking along roadway - Against traffic",
                                               "Standing on roadway",
                                               "Playing on roadway",
                                               "Other in roadway",
                                               "Not in roadway"), 
                                             n_records, replace = TRUE), NA)
    
    crash_data$BIKECTYPENAME <- ifelse(crash_data$PBPTYPENAME == "Bicyclist", 
                                       sample(c("Riding on roadway with traffic",
                                                "Riding on roadway against traffic",
                                                "Riding on sidewalk",
                                                "Crossing at intersection",
                                                "Crossing not at intersection",
                                                "Other"), 
                                              n_records, replace = TRUE), NA)
    
    # Add location columns
    crash_data$PEDLOCNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(ped_location, n_records, replace = TRUE), NA)
    
    crash_data$BIKELOCNAME <- ifelse(crash_data$PBPTYPENAME == "Bicyclist", 
                                     sample(bike_location, n_records, replace = TRUE), NA)
    
    crash_data$PEDPOSNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(ped_position, n_records, replace = TRUE), NA)
    
    crash_data$BIKEPOSNAME <- ifelse(crash_data$PBPTYPENAME == "Bicyclist", 
                                     sample(bike_position, n_records, replace = TRUE), NA)
    
    # Add direction columns
    crash_data$PEDDIRNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(direction, n_records, replace = TRUE), NA)
    
    crash_data$BIKEDIRNAME <- ifelse(crash_data$PBPTYPENAME == "Bicyclist", 
                                     sample(direction, n_records, replace = TRUE), NA)
    
    crash_data$MOTDIRNAME <- sample(direction, n_records, replace = TRUE)
    
    crash_data$MOTMANNAME <- sample(c("Going Straight", "Turning Right", "Turning Left", "Backing", "Changing Lanes", "Overtaking", "Other"), 
                                    n_records, replace = TRUE)
    
    # Add leg injury columns
    crash_data$PEDLEGNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(c("No injury", "Minor injury", "Moderate injury", "Serious injury", "Fatal injury", "Unknown"), 
                                           n_records, replace = TRUE), NA)
    
    crash_data$PEDSNRNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(c("Sober", "Alcohol present", "Drugs present", "Both alcohol and drugs present", "Unknown"), 
                                           n_records, replace = TRUE), NA)
    
    # Add contributing factors
    crash_data$PEDCGPNAME <- ifelse(crash_data$PBPTYPENAME == "Pedestrian", 
                                    sample(c("None", "Inattention", "Failure to yield", "Improper crossing", "Darting", "Lying in roadway", "Not visible", "Other"), 
                                           n_records, replace = TRUE), NA)
    
    crash_data$BIKECGPNAME <- ifelse(crash_data$PBPTYPENAME == "Bicyclist", 
                                     sample(c("None", "Inattention", "Failure to yield", "Improper crossing", "Wrong-way riding", "No lighting", "Not visible", "Other"), 
                                            n_records, replace = TRUE), NA)
    
    # Add location info
    crash_data$COUNTYNAME <- sapply(1:n_records, function(i) {
      paste0(crash_data$STATENAME[i], " County ", sample(1:30, 1))
    })
    
    crash_data$CITYNAME <- sapply(1:n_records, function(i) {
      state <- crash_data$STATENAME[i]
      if (state %in% names(cities)) {
        return(sample(cities[[state]], 1))
      } else {
        return(paste0("City in ", state))
      }
    })
    
    # Add date/time info
    crash_data$DAY <- days
    crash_data$MONTHNAME <- month.name[months]
    crash_data$DAY_WEEKNAME <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")[sample(1:7, n_records, replace = TRUE)]
    crash_data$HOUR <- hours
    crash_data$HOURNAME <- sapply(hours, function(h) {
      if (h == 0) return("12 AM")
      if (h < 12) return(paste0(h, " AM"))
      if (h == 12) return("12 PM")
      return(paste0(h-12, " PM"))
    })
    
    # Add environmental info
    crash_data$RUR_URBNAME <- sample(rur_urb, n_records, replace = TRUE)
    crash_data$FUNC_SYSNAME <- sample(func_sys, n_records, replace = TRUE)
    crash_data$LATITUDENAME <- latitudes
    crash_data$LONGITUDNAME <- longitudes
    crash_data$HARM_EVNAME <- sample(harm_ev, n_records, replace = TRUE)
    crash_data$MAN_COLLNAME <- sample(man_coll, n_records, replace = TRUE)
    crash_data$LGT_CONDNAME <- sample(light_cond, n_records, replace = TRUE)
    crash_data$WEATHERNAME <- sample(weather, n_records, replace = TRUE)
    crash_data$SCH_BUSNAME <- sample(sch_bus, n_records, replace = TRUE, prob = c(0.05, 0.95))
    
    # Write to CSV
    write.csv(crash_data, "crash_data.csv", row.names = FALSE)
    
    return(TRUE)
  }
  
  return(FALSE)
}

# Run before app starts
generate_sample_data()

# Run the app
shinyApp(ui = ui, server = server)