# Alzheimer's Disease & Cognitive Decline Interactive Explorer
# Shiny Dashboard Application

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(janitor)

# ============================================================================
# Data Loading and Preparation
# ============================================================================

# Check if data directory exists
if (!dir.exists("data")) {
  stop("ERROR: Data directory 'data' not found in shiny_app directory.\n",
       "Please run: source('copy_data.R') from project root to copy data files.")
}

# Load Alzheimer's data
raw_az_data <- read.csv("data/2015-2022 Alzheimer Data.csv") |>
  janitor::clean_names()

raw_az_data <- raw_az_data[-nrow(raw_az_data), ]

az_clean <- raw_az_data |>
  filter(
    !is.na(data_value),
    !is.na(year_start),
    !is.na(location_abbr)
  )

# Filter for cognitive decline/impairment
az_cog <- az_clean |>
  filter(
    str_detect(topic, regex("cognitive decline|cognitive impairment", ignore_case = TRUE))
  )

# Create base summary with age and sex groups
az_cog_summary_base <- az_cog |>
  mutate(
    age_group = case_when(
      stratification_category1 == "Age Group" ~ stratification1,
      TRUE ~ "Overall"
    ),
    sex_group = case_when(
      stratification_category2 == "Sex" ~ stratification2,
      TRUE ~ "Overall"
    )
  )

# Filter for 65+ overall
az_cog_65_overall <- az_cog_summary_base |>
  filter(
    age_group == "65 years or older",
    sex_group == "Overall"
  )

# State-year level data
az_state_year <- az_cog_65_overall |>
  group_by(
    year = year_start,
    state_abbr = location_abbr,
    state_name = location_desc
  ) |>
  summarise(
    mean_prev = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!state_abbr %in% c("US", "GU", "PR", "VI", "DC", "NRE", "SOU", "MDW", "WST"))

# Age and sex stratified data
az_age_sex <- az_cog_summary_base |>
  group_by(year = year_start, age_group, sex_group) |>
  summarise(
    mean_prev = mean(data_value, na.rm = TRUE),
    n_states = n(),
    .groups = "drop"
  )

# Load socioeconomic data
inc15 <- read_csv("data/median income-2015.csv", skip = 1) |>
  janitor::clean_names() |>
  transmute(
    year = 2015,
    state_name = geographic_area_name,
    median_income = as.numeric(median_income_dollars_estimate_households)
  )

inc16 <- read_csv("data/median income-2016.csv", skip = 1) |>
  janitor::clean_names() |>
  transmute(
    year = 2016,
    state_name = geographic_area_name,
    median_income = as.numeric(median_income_dollars_estimate_households)
  )

income_state <- bind_rows(inc15, inc16) |>
  mutate(state = str_squish(str_extract(state_name, "[^,]+$"))) |>
  group_by(year, state) |>
  summarise(median_state = median(median_income, na.rm = TRUE), .groups = "drop")

# Education data
edu15_raw <- read_csv("data/education2015.csv", skip = 1) |> janitor::clean_names()
edu16_raw <- read_csv("data/education2016.csv", skip = 1) |> janitor::clean_names()

edu_state <- bind_rows(
  edu15_raw |> filter(!str_detect(geographic_area_name, ",")) |>
    transmute(year = 2015, state = geographic_area_name,
              educ_bach = as.numeric(percent_estimate_percent_bachelors_degree_or_higher)),
  edu16_raw |> filter(!str_detect(geographic_area_name, ",")) |>
    transmute(year = 2016, state = geographic_area_name,
              educ_bach = as.numeric(percent_estimate_percent_bachelors_degree_or_higher))
)

# Air quality data
air15 <- read_csv("data/clean_air_state_2015.csv")
air16 <- read_csv("data/clean_air_state_2016.csv")

air_state <- bind_rows(air15, air16) |>
  group_by(year, state_desc) |>
  summarise(pm25_state = mean(pm25_wtd_mean, na.rm = TRUE), .groups = "drop")

# Combined dataset
az_combined <- az_state_year |>
  left_join(income_state, by = c("year", "state_name" = "state")) |>
  left_join(edu_state, by = c("year", "state_name" = "state")) |>
  left_join(air_state, by = c("year", "state_name" = "state_desc")) |>
  # Ensure state_name is always present
  mutate(state_name = coalesce(state_name, state_abbr, "Unknown"))

# Get unique values for filters
all_states <- sort(unique(az_state_year$state_name))
all_years <- sort(unique(az_state_year$year))

# Custom CSS
custom_css <- "
  .skin-blue .main-header .navbar { background-color: #d97706; }
  .skin-blue .main-header .logo { background-color: #b45309; }
  .skin-blue .main-header .logo:hover { background-color: #92400e; }
  .skin-blue .main-sidebar { background-color: #1f2937; }
  .skin-blue .sidebar-menu > li.active > a { border-left-color: #d97706; }
  .content-wrapper { background-color: #f8f4f0; }
  .box { border-top-color: #d97706; }
  .box-header { color: #d97706; }
  .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #d97706; }
  .btn-primary { background-color: #d97706; border-color: #b45309; }
  .btn-primary:hover { background-color: #b45309; border-color: #92400e; }
  .info-box { background: linear-gradient(135deg, #d97706 0%, #ea580c 100%); }
  .info-box-icon { background: rgba(0,0,0,0.1); }
  h1, h2, h3, h4 { font-family: Georgia, 'Times New Roman', serif; }
  .sidebar-menu > li > a { font-family: Georgia, 'Times New Roman', serif; }
  body { font-family: Georgia, 'Times New Roman', serif; }
"

# ============================================================================
# UI Definition
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = span(
      icon("brain"), 
      "Alzheimer's Explorer",
      style = "font-family: Georgia; font-weight: bold;"
    ),
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    tags$head(tags$style(HTML(custom_css))),
    
    sidebarMenu(
      id = "tabs",
      
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("home")),
      menuItem("Geographic Explorer", tabName = "geographic", icon = icon("map")),
      menuItem("Trends Analysis", tabName = "trends", icon = icon("chart-line")),
      menuItem("Factor Comparison", tabName = "factors", icon = icon("balance-scale")),
      menuItem("Demographic Analysis", tabName = "demographics", icon = icon("users")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      
      hr(),
      
      # Global Filters
      h4("  Filters", style = "color: #fbbf24; margin-left: 15px; font-family: Georgia;"),
      
      pickerInput(
        inputId = "selected_states",
        label = "Select States:",
        choices = all_states,
        selected = all_states,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          selectedTextFormat = "count > 3"
        )
      ),
      
      sliderInput(
        inputId = "year_range",
        label = "Year Range:",
        min = min(all_years),
        max = max(all_years),
        value = c(min(all_years), max(all_years)),
        step = 1,
        sep = ""
      ),
      
      hr(),
      
      # Info box
      div(
        style = "padding: 10px; margin: 10px; background: rgba(217, 119, 6, 0.1); border-radius: 8px;",
        p(style = "color: #fbbf24; font-size: 11px; margin: 0;",
          icon("info-circle"), " This dashboard explores Alzheimer's-related 
          cognitive decline prevalence across U.S. states from 2015-2022.")
      )
    )
  ),
  
  # Body
  dashboardBody(
    
    tabItems(
      
      # ========== Overview Tab ==========
      tabItem(
        tabName = "overview",
        
        fluidRow(
          infoBox(
            title = "States Analyzed",
            value = textOutput("n_states"),
            icon = icon("flag-usa"),
            color = "orange",
            width = 3
          ),
          infoBox(
            title = "Years Covered",
            value = textOutput("year_span"),
            icon = icon("calendar"),
            color = "orange",
            width = 3
          ),
          infoBox(
            title = "Avg Prevalence",
            value = textOutput("avg_prev"),
            icon = icon("chart-bar"),
            color = "orange",
            width = 3
          ),
          infoBox(
            title = "Data Points",
            value = textOutput("n_obs"),
            icon = icon("database"),
            color = "orange",
            width = 3
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("map-location-dot"), " Geographic Distribution"),
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("overview_map", height = "400px")
          ),
          box(
            title = span(icon("ranking-star"), " Top/Bottom States"),
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("overview_ranking", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-area"), " National Trend"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("overview_trend", height = "250px")
          )
        )
      ),
      
      # ========== Geographic Tab ==========
      tabItem(
        tabName = "geographic",
        
        fluidRow(
          box(
            title = span(icon("sliders"), " Map Controls"),
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "map_year",
              "Select Year:",
              choices = all_years,
              selected = max(all_years)
            ),
            
            radioGroupButtons(
              inputId = "map_color_scale",
              label = "Color Scale:",
              choices = c("Viridis" = "Viridis", "Inferno" = "Inferno", 
                          "Plasma" = "Plasma", "Orange" = "Orange"),
              selected = "Orange",
              justified = TRUE,
              size = "sm"
            ),
            
            hr(),
            
            h5("Quick Stats", style = "color: #d97706;"),
            verbatimTextOutput("map_stats")
          ),
          
          box(
            title = span(icon("map"), " Interactive Map"),
            status = "warning",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("geographic_map", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("images"), " Year-by-Year Comparison"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("geographic_comparison", height = "300px")
          )
        )
      ),
      
      # ========== Trends Tab ==========
      tabItem(
        tabName = "trends",
        
        fluidRow(
          box(
            title = span(icon("filter"), " Trend Options"),
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            pickerInput(
              "trend_states",
              "Compare States:",
              choices = all_states,
              selected = c("California", "Texas", "New York", "Florida", "Mississippi"),
              multiple = TRUE,
              options = pickerOptions(
                maxOptions = 10,
                liveSearch = TRUE
              )
            ),
            
            checkboxInput("show_national", "Show National Average", TRUE),
            checkboxInput("show_confidence", "Show Variability Band", TRUE),
            
            hr(),
            
            downloadButton("download_trend", "Download Data", class = "btn-warning btn-sm")
          ),
          
          box(
            title = span(icon("chart-line"), " State Trend Comparison"),
            status = "warning",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("trend_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("arrows-left-right"), " Year-over-Year Change"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("yoy_change", height = "300px")
          )
        )
      ),
      
      # ========== Factors Tab ==========
      tabItem(
        tabName = "factors",
        
        fluidRow(
          box(
            title = span(icon("cog"), " Factor Analysis Controls"),
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "factor_year",
              "Select Year:",
              choices = c(2015, 2016),
              selected = 2016
            ),
            
            radioGroupButtons(
              inputId = "factor_type",
              label = "Factor to Analyze:",
              choices = c("Income" = "income", "Education" = "education", 
                          "Air Quality" = "air"),
              selected = "income",
              direction = "vertical",
              justified = TRUE
            ),
            
            hr(),
            
            h5("Correlation", style = "color: #d97706;"),
            verbatimTextOutput("correlation_stats")
          ),
          
          box(
            title = span(icon("scatter-chart"), " Factor vs Prevalence"),
            status = "warning",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("factor_scatter", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-column"), " Factor Distribution"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("factor_histogram", height = "250px")
          ),
          box(
            title = span(icon("circle-info"), " Analysis Summary"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("factor_summary")
          )
        )
      ),
      
      # ========== Demographics Tab ==========
      tabItem(
        tabName = "demographics",
        
        fluidRow(
          box(
            title = span(icon("venus-mars"), " Prevalence by Sex"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("demo_sex", height = "350px")
          ),
          box(
            title = span(icon("user-clock"), " Prevalence by Age Group"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("demo_age", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("layer-group"), " Combined View"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("demo_combined", height = "350px")
          )
        )
      ),
      
      # ========== Data Explorer Tab ==========
      tabItem(
        tabName = "data",
        
        fluidRow(
          box(
            title = span(icon("table"), " Full Dataset"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                4,
                downloadButton("download_data", "Download Filtered Data", class = "btn-warning")
              ),
              column(
                8,
                p(style = "color: #666; font-size: 12px; padding-top: 10px;",
                  "Use the filters below each column to narrow down the data. Click column headers to sort.")
              )
            ),
            
            hr(),
            
            DTOutput("data_table")
          )
        )
      )
    )
  )
)

# ============================================================================
# Server Logic
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive: filtered data based on selections
  filtered_data <- reactive({
    az_state_year |>
      filter(
        state_name %in% input$selected_states,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  filtered_combined <- reactive({
    az_combined |>
      filter(
        state_name %in% input$selected_states,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  # ========== Overview Tab ==========
  
  output$n_states <- renderText({
    n_distinct(filtered_data()$state_name)
  })
  
  output$year_span <- renderText({
    paste0(min(filtered_data()$year), " - ", max(filtered_data()$year))
  })
  
  output$avg_prev <- renderText({
    paste0(round(mean(filtered_data()$mean_prev, na.rm = TRUE), 1), "%")
  })
  
  output$n_obs <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$overview_map <- renderPlotly({
    latest_year <- max(filtered_data()$year)
    map_data <- filtered_data() |> filter(year == latest_year)
    
    plot_ly(
      type = "choropleth",
      locationmode = "USA-states",
      locations = map_data$state_abbr,
      z = map_data$mean_prev,
      text = paste0(map_data$state_name, "<br>Prevalence: ", round(map_data$mean_prev, 1), "%"),
      hoverinfo = "text",
      colorscale = list(c(0, "#fef3c7"), c(0.5, "#f59e0b"), c(1, "#92400e")),
      colorbar = list(title = "Prevalence (%)")
    ) |>
      layout(
        title = list(text = paste0("<b>Cognitive Prevalence (", latest_year, ")</b>"),
                     font = list(family = "Georgia", color = "#d97706")),
        geo = list(scope = "usa", showlakes = TRUE, lakecolor = toRGB("white")),
        font = list(family = "Georgia")
      )
  })
  
  output$overview_ranking <- renderPlotly({
    ranking <- filtered_data() |>
      group_by(state_name) |>
      summarise(avg_prev = mean(mean_prev, na.rm = TRUE)) |>
      arrange(desc(avg_prev))
    
    top_bottom <- bind_rows(
      ranking |> head(5) |> mutate(group = "Highest"),
      ranking |> tail(5) |> mutate(group = "Lowest")
    ) |>
      mutate(state_name = factor(state_name, levels = state_name))
    
    plot_ly(top_bottom, y = ~state_name, x = ~avg_prev, color = ~group,
            colors = c("Highest" = "#dc2626", "Lowest" = "#16a34a"),
            type = "bar", orientation = "h",
            text = ~paste0(round(avg_prev, 1), "%"),
            textposition = "outside",
            hovertemplate = "%{y}<br>Prevalence: %{x:.1f}%<extra></extra>") |>
      layout(
        title = list(text = "<b>Top & Bottom States</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Average Prevalence (%)", titlefont = list(family = "Georgia")),
        yaxis = list(title = "", categoryorder = "total ascending"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.15),
        font = list(family = "Georgia"),
        barmode = "group"
      )
  })
  
  output$overview_trend <- renderPlotly({
    national <- filtered_data() |>
      group_by(year) |>
      summarise(
        mean_prev = mean(mean_prev, na.rm = TRUE),
        sd_prev = sd(mean_prev, na.rm = TRUE)
      )
    
    plot_ly(national, x = ~year) |>
      add_ribbons(ymin = ~mean_prev - sd_prev, ymax = ~mean_prev + sd_prev,
                  fillcolor = "rgba(217, 119, 6, 0.2)", line = list(color = "transparent"),
                  name = "±1 SD") |>
      add_trace(y = ~mean_prev, type = "scatter", mode = "lines+markers",
                line = list(color = "#d97706", width = 3),
                marker = list(color = "#d97706", size = 10),
                name = "National Mean",
                hovertemplate = "Year: %{x}<br>Prevalence: %{y:.1f}%<extra></extra>") |>
      layout(
        title = list(text = "<b>National Trend Over Time</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1, titlefont = list(family = "Georgia")),
        yaxis = list(title = "Prevalence (%)", titlefont = list(family = "Georgia")),
        font = list(family = "Georgia"),
        hovermode = "x unified"
      )
  })
  
  # ========== Geographic Tab ==========
  
  output$geographic_map <- renderPlotly({
    map_data <- filtered_data() |> filter(year == input$map_year)
    
    color_scales <- list(
      "Viridis" = "Viridis",
      "Inferno" = "Inferno", 
      "Plasma" = "Plasma",
      "Orange" = list(c(0, "#fef3c7"), c(0.25, "#fcd34d"), c(0.5, "#f59e0b"), 
                      c(0.75, "#d97706"), c(1, "#92400e"))
    )
    
    plot_ly(
      type = "choropleth",
      locationmode = "USA-states",
      locations = map_data$state_abbr,
      z = map_data$mean_prev,
      text = paste0("<b>", map_data$state_name, "</b><br>",
                    "Prevalence: ", round(map_data$mean_prev, 1), "%"),
      hoverinfo = "text",
      colorscale = color_scales[[input$map_color_scale]],
      colorbar = list(title = "Prevalence (%)", titlefont = list(family = "Georgia"))
    ) |>
      layout(
        title = list(text = paste0("<b>State-Level Prevalence (", input$map_year, ")</b>"),
                     font = list(family = "Georgia", size = 18, color = "#d97706")),
        geo = list(scope = "usa", showlakes = TRUE, lakecolor = toRGB("white")),
        font = list(family = "Georgia")
      )
  })
  
  output$map_stats <- renderPrint({
    map_data <- filtered_data() |> filter(year == input$map_year)
    cat("Year:", input$map_year, "\n")
    cat("States shown:", n_distinct(map_data$state_name), "\n")
    cat("Mean:", round(mean(map_data$mean_prev), 1), "%\n")
    cat("Min:", round(min(map_data$mean_prev), 1), "%\n")
    cat("Max:", round(max(map_data$mean_prev), 1), "%\n")
    cat("Range:", round(max(map_data$mean_prev) - min(map_data$mean_prev), 1), "pp")
  })
  
  output$geographic_comparison <- renderPlotly({
    years_data <- filtered_data() |>
      group_by(year) |>
      summarise(
        mean = mean(mean_prev),
        min = min(mean_prev),
        max = max(mean_prev)
      )
    
    plot_ly(years_data, x = ~year) |>
      add_trace(y = ~min, type = "bar", name = "Min", marker = list(color = "#fcd34d")) |>
      add_trace(y = ~mean, type = "bar", name = "Mean", marker = list(color = "#d97706")) |>
      add_trace(y = ~max, type = "bar", name = "Max", marker = list(color = "#92400e")) |>
      layout(
        title = list(text = "<b>Prevalence Range by Year</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Prevalence (%)"),
        barmode = "group",
        font = list(family = "Georgia"),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # ========== Trends Tab ==========
  
  output$trend_plot <- renderPlotly({
    state_data <- filtered_data() |>
      filter(state_name %in% input$trend_states)
    
    p <- plot_ly()
    
    if (input$show_national) {
      national <- filtered_data() |>
        group_by(year) |>
        summarise(mean_prev = mean(mean_prev, na.rm = TRUE), .groups = "drop")
      
      if (input$show_confidence) {
        nat_sd <- filtered_data() |>
          group_by(year) |>
          summarise(
            mean_prev = mean(mean_prev),
            sd = sd(mean_prev)
          )
        p <- p |>
          add_ribbons(data = nat_sd, x = ~year, 
                      ymin = ~mean_prev - sd, ymax = ~mean_prev + sd,
                      fillcolor = "rgba(100,100,100,0.2)", line = list(color = "transparent"),
                      name = "National ±1 SD", showlegend = TRUE)
      }
      
      p <- p |>
        add_trace(data = national, x = ~year, y = ~mean_prev,
                  type = "scatter", mode = "lines",
                  line = list(color = "gray", width = 3, dash = "dash"),
                  name = "National Average")
    }
    
    colors <- viridis(length(input$trend_states))
    
    for (i in seq_along(input$trend_states)) {
      st <- input$trend_states[i]
      st_data <- state_data |> filter(state_name == st)
      p <- p |>
        add_trace(data = st_data, x = ~year, y = ~mean_prev,
                  type = "scatter", mode = "lines+markers",
                  line = list(color = colors[i], width = 2),
                  marker = list(color = colors[i], size = 8),
                  name = st,
                  hovertemplate = paste0("<b>", st, "</b><br>Year: %{x}<br>Prevalence: %{y:.1f}%<extra></extra>"))
    }
    
    p |>
      layout(
        title = list(text = "<b>State Prevalence Trends</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1, titlefont = list(family = "Georgia")),
        yaxis = list(title = "Prevalence (%)", titlefont = list(family = "Georgia")),
        font = list(family = "Georgia"),
        hovermode = "closest",
        legend = list(orientation = "v", x = 1.02, y = 0.5)
      )
  })
  
  output$yoy_change <- renderPlotly({
    yoy <- filtered_data() |>
      arrange(state_name, year) |>
      group_by(state_name) |>
      mutate(change = mean_prev - lag(mean_prev)) |>
      filter(!is.na(change)) |>
      group_by(year) |>
      summarise(
        avg_change = mean(change),
        pos_states = sum(change > 0),
        neg_states = sum(change < 0)
      )
    
    plot_ly(yoy, x = ~year) |>
      add_trace(y = ~avg_change, type = "bar", name = "Avg Change (pp)",
                marker = list(color = ifelse(yoy$avg_change > 0, "#dc2626", "#16a34a")),
                hovertemplate = "Year: %{x}<br>Change: %{y:.2f} pp<extra></extra>") |>
      layout(
        title = list(text = "<b>Year-over-Year Change in Prevalence</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Average Change (percentage points)"),
        font = list(family = "Georgia"),
        showlegend = FALSE
      )
  })
  
  output$download_trend <- downloadHandler(
    filename = function() { paste0("alzheimer_trends_", Sys.Date(), ".csv") },
    content = function(file) {
      write_csv(filtered_data() |> filter(state_name %in% input$trend_states), file)
    }
  )
  
  # ========== Factors Tab ==========
  
  factor_data <- reactive({
    df <- filtered_combined() |> filter(year == as.numeric(input$factor_year))
    
    # Ensure state_name exists - if not, use state_abbr or create from state_name
    if (!"state_name" %in% names(df)) {
      if ("state_abbr" %in% names(df)) {
        df <- df |> mutate(state_name = state_abbr)
      } else {
        df <- df |> mutate(state_name = as.character(row_number()))
      }
    }
    
    if (input$factor_type == "income") {
      df <- df |> filter(!is.na(median_state)) |>
        mutate(factor_value = median_state, factor_label = "Median Income ($)")
    } else if (input$factor_type == "education") {
      df <- df |> filter(!is.na(educ_bach)) |>
        mutate(factor_value = educ_bach, factor_label = "% with Bachelor's Degree")
    } else {
      df <- df |> filter(!is.na(pm25_state)) |>
        mutate(factor_value = pm25_state, factor_label = "PM2.5 (µg/m³)")
    }
    df
  })
  
  output$factor_scatter <- renderPlotly({
    df <- factor_data()
    if (nrow(df) == 0) return(NULL)
    
    # Ensure state_name exists for hover text
    if (!"state_name" %in% names(df)) {
      if ("state_abbr" %in% names(df)) {
        df$state_name <- df$state_abbr
      } else {
        df$state_name <- paste("State", 1:nrow(df))
      }
    }
    
    plot_ly(df, x = ~factor_value, y = ~mean_prev,
            text = ~state_name, type = "scatter", mode = "markers",
            marker = list(size = 12, color = "#d97706", opacity = 0.7,
                          line = list(color = "white", width = 1)),
            hovertemplate = paste0("<b>%{text}</b><br>",
                                   df$factor_label[1], ": %{x}<br>",
                                   "Prevalence: %{y:.1f}%<extra></extra>")) |>
      layout(
        title = list(text = paste0("<b>", df$factor_label[1], " vs Cognitive Prevalence (", 
                                   input$factor_year, ")</b>"),
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = df$factor_label[1], titlefont = list(family = "Georgia")),
        yaxis = list(title = "Prevalence (%)", titlefont = list(family = "Georgia")),
        font = list(family = "Georgia"),
        hovermode = "closest"
      )
  })
  
  output$correlation_stats <- renderPrint({
    df <- factor_data()
    if (nrow(df) < 3) {
      cat("Insufficient data")
      return()
    }
    
    cor_val <- cor(df$factor_value, df$mean_prev, use = "complete.obs")
    model <- lm(mean_prev ~ factor_value, data = df)
    
    cat("r =", round(cor_val, 3), "\n")
    cat("R² =", round(summary(model)$r.squared, 3), "\n")
    cat("Slope =", round(coef(model)[2], 4), "\n")
    cat("p-value =", format.pval(summary(model)$coefficients[2, 4], 3))
  })
  
  output$factor_histogram <- renderPlotly({
    df <- factor_data()
    if (nrow(df) == 0) return(NULL)
    
    plot_ly(df, x = ~factor_value, type = "histogram",
            marker = list(color = "#d97706", line = list(color = "white", width = 1))) |>
      layout(
        title = list(text = paste0("<b>Distribution of ", df$factor_label[1], "</b>"),
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = df$factor_label[1]),
        yaxis = list(title = "Count"),
        font = list(family = "Georgia")
      )
  })
  
  output$factor_summary <- renderUI({
    df <- factor_data()
    if (nrow(df) < 3) return(HTML("<p>Insufficient data for analysis</p>"))
    
    cor_val <- cor(df$factor_value, df$mean_prev, use = "complete.obs")
    direction <- ifelse(cor_val < 0, "negative", "positive")
    strength <- case_when(
      abs(cor_val) < 0.3 ~ "weak",
      abs(cor_val) < 0.6 ~ "moderate", 
      TRUE ~ "strong"
    )
    
    factor_name <- switch(input$factor_type,
                          "income" = "household income",
                          "education" = "educational attainment",
                          "air" = "PM2.5 air pollution")
    
    HTML(paste0(
      "<div style='padding: 15px; font-family: Georgia;'>",
      "<h4 style='color: #d97706;'>Analysis Summary</h4>",
      "<p>There is a <strong>", strength, " ", direction, "</strong> correlation ",
      "(r = ", round(cor_val, 3), ") between state-level ", factor_name, 
      " and Alzheimer's-related cognitive prevalence in ", input$factor_year, ".</p>",
      "<p style='color: #666; font-size: 12px;'><em>Note: This is an ecological analysis ",
      "at the state level. Individual-level associations may differ.</em></p>",
      "</div>"
    ))
  })
  
  # ========== Demographics Tab ==========
  
  output$demo_sex <- renderPlotly({
    sex_data <- az_age_sex |>
      filter(
        sex_group %in% c("Male", "Female"),
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) |>
      group_by(year, sex_group) |>
      summarise(mean_prev = mean(mean_prev), .groups = "drop")
    
    plot_ly(sex_data, x = ~year, y = ~mean_prev, color = ~sex_group,
            colors = c("Female" = "#d97706", "Male" = "#2563eb"),
            type = "scatter", mode = "lines+markers",
            marker = list(size = 10),
            hovertemplate = "%{text}<br>Year: %{x}<br>Prevalence: %{y:.1f}%<extra></extra>",
            text = ~sex_group) |>
      layout(
        title = list(text = "<b>Prevalence by Sex Over Time</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Prevalence (%)"),
        font = list(family = "Georgia"),
        legend = list(orientation = "h", y = -0.15)
      )
  })
  
  output$demo_age <- renderPlotly({
    age_data <- az_age_sex |>
      filter(
        age_group %in% c("50-64 years", "65 years or older"),
        sex_group == "Overall",
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    plot_ly(age_data, x = ~year, y = ~mean_prev, color = ~age_group,
            colors = c("50-64 years" = "#d97706", "65 years or older" = "#2563eb"),
            type = "scatter", mode = "lines+markers",
            marker = list(size = 10),
            hovertemplate = "%{text}<br>Year: %{x}<br>Prevalence: %{y:.1f}%<extra></extra>",
            text = ~age_group) |>
      layout(
        title = list(text = "<b>Prevalence by Age Group Over Time</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Prevalence (%)"),
        font = list(family = "Georgia"),
        legend = list(orientation = "h", y = -0.15)
      )
  })
  
  output$demo_combined <- renderPlotly({
    combined <- az_age_sex |>
      filter(
        age_group != "Overall",
        sex_group != "Overall",
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) |>
      group_by(age_group, sex_group) |>
      summarise(mean_prev = mean(mean_prev), .groups = "drop")
    
    plot_ly(combined, x = ~age_group, y = ~mean_prev, color = ~sex_group,
            colors = c("Female" = "#d97706", "Male" = "#2563eb"),
            type = "bar",
            text = ~paste0(round(mean_prev, 1), "%"),
            textposition = "outside",
            hovertemplate = "%{x}<br>%{fullData.name}: %{y:.1f}%<extra></extra>") |>
      layout(
        title = list(text = "<b>Average Prevalence by Age and Sex</b>",
                     font = list(family = "Georgia", color = "#d97706")),
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Prevalence (%)"),
        font = list(family = "Georgia"),
        barmode = "group",
        legend = list(orientation = "h", y = -0.15)
      )
  })
  
  # ========== Data Explorer Tab ==========
  
  output$data_table <- renderDT({
    display_data <- filtered_combined() |>
      select(
        Year = year,
        State = state_name,
        `State Code` = state_abbr,
        `Prevalence (%)` = mean_prev,
        `Median Income ($)` = median_state,
        `Education (% BA+)` = educ_bach,
        `PM2.5 (µg/m³)` = pm25_state
      ) |>
      mutate(
        `Prevalence (%)` = round(`Prevalence (%)`, 1),
        `Median Income ($)` = round(`Median Income ($)`, 0),
        `Education (% BA+)` = round(`Education (% BA+)`, 1),
        `PM2.5 (µg/m³)` = round(`PM2.5 (µg/m³)`, 1)
      )
    
    datatable(
      display_data,
      filter = "top",
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) |>
      formatStyle(
        'Prevalence (%)',
        background = styleColorBar(range(display_data$`Prevalence (%)`, na.rm = TRUE), '#fcd34d'),
        backgroundSize = '98% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("alzheimer_data_", Sys.Date(), ".csv") },
    content = function(file) {
      write_csv(filtered_combined(), file)
    }
  )
}

# ============================================================================
# Run the App
# ============================================================================

shinyApp(ui = ui, server = server)
