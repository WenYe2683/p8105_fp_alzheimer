# Alzheimer's Disease & Cognitive Decline Interactive Explorer

A Shiny dashboard application for exploring Alzheimer's-related cognitive decline prevalence across U.S. states from 2015-2022.

## Features

### Dashboard Overview
- Key statistics summary (states analyzed, years covered, average prevalence)
- Interactive choropleth map showing geographic distribution
- State ranking comparison (highest vs lowest prevalence)
- National trend visualization

### Geographic Explorer
- Year-by-year interactive maps
- Multiple color scale options
- Quick statistics panel
- Year comparison view

### Trends Analysis
- Multi-state trend comparison
- National average overlay option
- Year-over-year change analysis
- Data download functionality

### Factor Comparison
- Income vs prevalence analysis
- Education vs prevalence analysis
- Air quality (PM2.5) vs prevalence analysis
- Correlation statistics
- Regression trend lines

### Demographic Analysis
- Prevalence by sex over time
- Prevalence by age group over time
- Combined demographic view

### Data Explorer
- Full interactive data table
- Column filtering and sorting
- Data download option

## Running the App

### Prerequisites

Install required R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "tidyverse",
  "plotly",
  "DT",
  "viridis",
  "janitor"
))
```

### Launch Locally

From the project root directory:

```r
shiny::runApp("shiny_app")
```

Or from within the `shiny_app` directory:

```r
shiny::runApp()
```

### Deploy to shinyapps.io

1. Create an account at [shinyapps.io](https://www.shinyapps.io/)
2. Install rsconnect package: `install.packages("rsconnect")`
3. Configure your account:
   ```r
   rsconnect::setAccountInfo(
     name = "your-account-name",
     token = "your-token",
     secret = "your-secret"
   )
   ```
4. Deploy:
   ```r
   rsconnect::deployApp("shiny_app")
   ```

## Data Sources

- **Alzheimer's Data**: CDC Alzheimer's Disease and Healthy Aging Data Portal (2015-2022)
- **Income Data**: U.S. Census American Community Survey
- **Education Data**: U.S. Census American Community Survey
- **Air Quality Data**: EPA Air Quality System (AQS) Data

## Project Structure

```
shiny_app/
├── app.R           # Main Shiny application
├── README.md       # This file
└── www/            # Static assets (if needed)
```

## Color Theme

The dashboard uses an orange/amber color palette to match the project's overall design:
- Primary: #d97706 (Amber 600)
- Secondary: #ea580c (Orange 600)
- Accent: #fbbf24 (Amber 400)
- Dark: #92400e (Amber 800)


