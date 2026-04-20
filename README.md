# Data-Visualisation-Project

📌 OVERVIEW
- Interactive R Shiny dashboard exploring mental health trends across NSW
- Data sourced from HealthStats NSW, maintained by the NSW Ministry of Health
- Designed to make mental health data accessible to non-specialists including policy advisors, researchers, and the general public

Dashboard covers five tabs:
- Overview — statewide KPIs and psychological distress trends by Local Health District
- Regional Map — interactive choropleth showing distress and suicide rates across all 15 NSW LHDs
- Demographics — ED visits and self-harm hospitalisations broken down by age group and sex
- COVID Impact — before and after comparison of mental health crisis presentations around March 2020
- Wellbeing Explorer — life satisfaction and self-rated mental health at the state level

🌐 LIVE DASHBOARD
You can access the fully deployed dashboard here: https://pandasdataviz.shinyapps.io/project/

📁 PROJECT STRUCTURE 
Below is a list of all major files and folders contained in the ZIP/GitHub repository, along with short summaries of their purpose.

1. app/ (Shiny Application Folder)
This folder contains everything required to run the dashboard.
- app.R (The main Shiny application file that builds the UI, server logic, charts, maps, filters, and KPIs.)
- data/ (Contains all datasets used by the dashboard.)
  - data/processed/
 - time_trends.csv (Cleaned and aggregated tap-on/tap-off counts by date, mode, type, and day category.)
 - stop_taps.csv (Stop-level tap activity with matched stop IDs and GTFS coordinates.)
  - data/raw/
 - stop_locations.csv (Extracted GTFS stop coordinates (lat/lon) for bubble mapping.)

2. scripts/ (Data Preprocessing Scripts)
This script prepares raw Opal and GTFS data before feeding them into the Shiny app.
- 02_data_prep.R (Cleans and merges Opal data files).
- 03_match_stops_gtfs.R (Matches Opal stop identifiers/postcodes with GTFS stop names and coordinates.)

3. README.md
The file you are reading now (describes project purpose, structure, and usage instructions.)

🖥️ INSTRUCTIONS FOR RUNNING LOCALLY
1. Install required packages:
 install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", "dplyr", "tidyr", "RColorBrewer", "scales", "sf", "nswgeo"))
2. Run the application
 shiny::runApp()

🧩 DATA SOURCES
All raw data comes from the official NSW Open Data Portal:
- NSW HealthStats (Mental Health) - https://www.healthstats.nsw.gov.au/topic-overview/Mental%20health#M

👥 CONTRIBUTERS
- Smriti Lotlikar
- Sushmitha
