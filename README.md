# Pedestrian & Bicycle Crash Dashboard

## Overview
The **Pedestrian & Bicycle Crash Dashboard** is an interactive web application built using R and Shiny to analyze pedestrian and bicycle crash data across the United States. The dashboard provides a comprehensive exploration of crash patterns through geographic, temporal, demographic, and environmental analyses, enabling users to identify trends, hotspots, and contributing factors to improve road safety.

## Features
- **Interactive Visualizations**: Includes maps, bar charts, line plots, and heatmaps powered by Leaflet, Plotly, and ggplot2.
- **Multiple Analysis Tabs**:
  - **Overview**: Summary statistics, crash trends, and distributions by type, city, and light conditions.
  - **Geographic Analysis**: Interactive map of crash locations and breakdowns by rural/urban areas and road types.
  - **Temporal Analysis**: Crashes by month, day of the week, hour, and a calendar heatmap.
  - **Demographics**: Age, gender, position, and direction distributions of crash victims.
  - **Environmental Factors**: Analysis of weather, lighting, school zones, crosswalks, and correlations.
  - **Raw Data**: Filterable and downloadable data table for detailed exploration.
- **Filters**: Sidebar filters for year, state, person type (pedestrian/bicyclist), and weather, applicable across all tabs.
- **Right Sidebar**: Contains About, Help, and Legend sections for dashboard information and usage tips.
- **Responsive Design**: Built with `shinydashboard` and `shinydashboardPlus` for a modern, user-friendly interface.
- **Sample Data**: Includes a script to generate sample data for demonstration purposes if no data file is provided.

## Installation
### Prerequisites
- **R**: Version 4.0 or higher.
- **R Packages**: Install the required packages listed below.
- **Shiny Server** (optional): For deploying the app on a server.

### Required R Packages
Install the necessary packages by running the following command in R:

```R
install.packages(c(
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
  "leaflet", "dplyr", "ggplot2", "plotly", "DT", "lubridate",
  "viridis", "tidyr", "scales", "stringr", "readr"
))
```

### Setup
1. Clone or download the project repository to your local machine.
2. Place the `app.R` file (containing the dashboard code) in your working directory.
3. Ensure the `crash_data.csv` file is in the same directory, or let the app generate sample data (see Data Requirements).
4. Open R or RStudio and set the working directory to the project folder:
   ```R
   setwd("path/to/project/folder")
   ```
5. Run the Shiny app:
   ```R
   library(shiny)
   runApp("app.R")
   ```

The app will launch in your default web browser.

## Usage
1. **Launch the App**: Run the app as described above. It will open in a browser at `http://127.0.0.1:XXXX` (port number varies).
2. **Navigate Tabs**:
   - Use the sidebar menu to switch between Overview, Geographic Analysis, Temporal Analysis, Demographics, Environmental Factors, and Raw Data tabs.
   - Each tab contains interactive visualizations with hover tooltips and clickable elements (e.g., map markers).
3. **Apply Filters**:
   - Use the sidebar filters to narrow down the data by year, state, person type, or weather.
   - Click "Reset Filters" to clear selections.
4. **Explore Data**:
   - Hover over charts for detailed information.
   - Click map markers to view crash details.
   - Use the Raw Data tab to search, filter, and download the dataset.
5. **Access Help**:
   - Open the right sidebar (gear icon) for About, Help, and Legend information.

## Data Requirements
- **Input File**: The dashboard expects a `crash_data.csv` file in the working directory with the following columns (partial list):
  - `YEAR`: Crash year (numeric).
  - `STATENAME`: State name (e.g., "California").
  - `PBPTYPENAME`: Person type (e.g., "Pedestrian", "Bicyclist").
  - `WEATHERNAME`: Weather condition (e.g., "Clear", "Rain").
  - `LATITUDENAME`: Latitude (numeric, -90 to 90).
  - `LONGITUDNAME`: Longitude (numeric, -180 to 180).
  - `CITYNAME`: City name.
  - `LGT_CONDNAME`: Light condition (e.g., "Daylight", "Dark - Lighted").
  - `MONTHNAME`: Month name (e.g., "January").
  - `DAY`: Day of the month (numeric).
  - `DAY_WEEKNAME`: Day of the week (e.g., "Monday").
  - `HOUR`: Hour of the day (0–23, or 99 for unknown).
  - `PBAGENAME`: Age group (e.g., "0-4", "25-34").
  - `PBSEXNAME`: Gender (e.g., "Male", "Female").
  - `RUR_URBNAME`: Rural/urban classification.
  - `FUNC_SYSNAME`: Road functional system.
  - `PBSZONENAME`: School zone indicator.
  - `PBCWALKNAME`: Crosswalk presence.
  - And other columns as used in the visualizations.
- **Sample Data**: If `crash_data.csv` is not provided, the app generates a sample dataset with 1000 records using the `generate_sample_data()` function. This data simulates realistic crash scenarios across multiple states, years, and conditions.
- **Data Validation**: The app filters out invalid coordinates (e.g., non-numeric or out-of-range latitude/longitude) and handles missing values gracefully.

## Directory Structure
```
project_folder/
├── app.R              # Main Shiny app script
├── crash_data.csv     # Input data file (optional, generated if missing)
└── README.md          # This file
```

## Customization
- **Data Source**: Replace `crash_data.csv` with your own dataset, ensuring column names match those expected by the app.
- **Styling**: Modify the CSS in the `dashboardBody` `tags$style` section to change the dashboard's appearance.
- **Visualizations**: Adjust plot parameters (e.g., colors, sizes, themes) in the server code to customize charts.
- **Filters**: Add or modify filters in the sidebar by updating the `pickerInput` components and corresponding server logic.

## Troubleshooting
- **Value Box Shows 0**: If the "Bicycle Incidents" or other value boxes show `0`, verify that `PBPTYPENAME` contains expected values (e.g., "Bicyclist"). Check the console for unique values using:
  ```R
  print(unique(filtered_data()$PBPTYPENAME))
  ```
- **Overlapping Labels**: If y-axis labels overlap (e.g., in Age Distribution), adjust text size or plot height as described in the server code.
- **Missing Data**: Ensure `crash_data.csv` exists or allow sample data generation. Check for missing or invalid columns.
- **Performance**: For large datasets, consider optimizing data processing (e.g., pre-filtering) or increasing server resources.

## Contributing
Contributions are welcome! To contribute:
1. Fork the repository.
2. Create a new branch for your feature or bug fix:
   ```bash
   git checkout -b feature/your-feature
   ```
3. Make changes and test thoroughly.
4. Commit changes with descriptive messages:
   ```bash
   git commit -m "Add feature X to dashboard"
   ```
5. Push to your fork and submit a pull request.
