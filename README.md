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

1. app.R
The main Shiny application file that builds the UI, server logic, charts, maps, filters, data processing and KPIs.

2. data/ (Contains all datasets used by the dashboard)
- Intentional_self-harm_hospitalisations_by_Sex_and_Age.csv — Age-standardised hospitalisation rates for intentional self-harm in NSW, broken down by sex and age group over time.
- Life_satisfaction_by_Category.csv — Proportion of NSW adults reporting each level of life satisfaction (Delighted to Terrible) by year.
- Mental_Health_related_emergency_department_visits_annual.csv — Annual crude rates of mental health related ED presentations in NSW by age group and sex.
- Mental_health_related_emergency_department_visits_monthly.csv — Quarterly mental health ED presentation rates by age, sex, and Local Health District, used for the COVID impact comparison.
- Psychological_distress_in_adults_by_Category_and_LHD.csv — Proportion of adults reporting high or very high psychological distress, broken down by Local Health District and year.
- Psychological_distress_in_adults_by_level.csv — Statewide breakdown of psychological distress across all four levels (Low, Moderate, High, Very High) by year.
- Self-rated_mental_health_status_by_Category.csv — Proportion of NSW adults self-reporting each category of mental health status (Excellent to Very Poor) by year.
- Suicide_by_LHD.csv — Age-standardised suicide rates per 100,000 population for each NSW Local Health District by year.

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
