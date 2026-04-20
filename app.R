# ============================================================
# The Silent Crisis: An Interactive Look at Mental Health in NSW
# DATA5002 26T1 | Pandas Group (Smriti and Sushmitha)
# Full Dashboard - All 5 Tabs
# ============================================================
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)
library(sf)
library(nswgeo)

# ============================================================
# HELPER FUNCTIONS
# ============================================================
read_hs <- function(path, ...) {
  if (!file.exists(path)) {
    warning("File not found: ", path,
            "\n  -> Place it in the data/ folder next to app.R")
    return(NULL)
  }
  read.csv(path, ...)
}

clean_hs <- function(df, value_col, dataset_name, is_percent = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  original_rows <- nrow(df)
  
  suppression_codes <- c("*", "n.p.", "n.p", "N.P.", "n.a.", "n.a", "N.A.", "")
  df[[value_col]] <- ifelse(
    trimws(as.character(df[[value_col]])) %in% suppression_codes,
    NA, df[[value_col]]
  )
  suppressed_count <- sum(is.na(suppressWarnings(as.numeric(as.character(df[[value_col]])))))
  df[[value_col]]  <- suppressWarnings(as.numeric(as.character(df[[value_col]])))
  df <- df %>% filter(!is.na(.data[[value_col]]))
  after_na <- nrow(df)
  df <- df %>% filter(.data[[value_col]] >= 0)
  after_negative <- nrow(df)
  if (is_percent) df <- df %>% filter(.data[[value_col]] <= 100)
  after_implausible <- nrow(df)
  df <- df %>% distinct()
  after_dedup <- nrow(df)
  
  cat("\n=== Data Quality Report:", dataset_name, "===\n")
  cat("  Rows loaded          :", original_rows, "\n")
  cat("  Suppressed/invalid   :", suppressed_count, "\n")
  cat("  Removed (NA)         :", original_rows - after_na, "\n")
  cat("  Removed (negative)   :", after_na - after_negative, "\n")
  cat("  Removed (>100%)      :", after_negative - after_implausible, "\n")
  cat("  Removed (duplicates) :", after_implausible - after_dedup, "\n")
  cat("  Final clean rows     :", after_dedup, "\n")
  return(df)
}

# ============================================================
# TAB 1: OVERVIEW — DATA
# ============================================================
raw_distress_level <- read_hs("data/Psychological distress in adults by level.csv", skip = 1)

if (!is.null(raw_distress_level)) {
  overview_distress <- raw_distress_level %>%
    rename(level = Psychological.distress, year = Period, value = Per.cent) %>%
    filter(level %in% c(
      "Low level of psych distress",
      "Moderate level of psych distress",
      "High level of psych distress",
      "Very high level of psych distress"
    )) %>%
    clean_hs("value", "Psychological Distress by Level", is_percent = TRUE) %>%
    mutate(
      year  = suppressWarnings(as.integer(trimws(as.character(year)))),
      level = case_when(
        level == "Low level of psych distress"        ~ "Low",
        level == "Moderate level of psych distress"   ~ "Medium",
        level == "High level of psych distress"       ~ "High",
        level == "Very high level of psych distress"  ~ "Very High"
      )
    ) %>%
    filter(!is.na(year)) %>%
    distinct(level, year, .keep_all = TRUE) %>%
    arrange(level, year)
  
  kpi_distress_val <- overview_distress %>%
    filter(year == max(year), level %in% c("High", "Very High")) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    pull(total) %>%
    round(1)
}

raw_distress_lhd <- read_hs("data/Psychological distress in adults by Category and LHD.csv", skip = 1)

if (!is.null(raw_distress_lhd)) {
  distress_lhd <- raw_distress_lhd %>%
    rename(category = Category, LHD_NAME = LHD, year = Period, value = Per.cent) %>%
    filter(
      trimws(category) == "High or very high level",
      grepl("^[0-9]{4}$", trimws(as.character(year)))
    ) %>%
    clean_hs("value", "Distress by LHD", is_percent = TRUE) %>%
    mutate(
      year     = as.integer(trimws(as.character(year))),
      LHD_NAME = trimws(gsub(" LHD$", "", LHD_NAME))
    ) %>%
    filter(!is.na(year)) %>%
    distinct(LHD_NAME, year, .keep_all = TRUE) %>%
    arrange(LHD_NAME, year)
}

year_min <- min(overview_distress$year, na.rm = TRUE)
year_max <- max(overview_distress$year, na.rm = TRUE)

raw_ed_annual <- read_hs("data/Mental Health related emergency department visits (annual) by Age (years) for Total mental health presentations by Sex.csv", skip = 1)

if (!is.null(raw_ed_annual)) {
  ed_annual <- raw_ed_annual %>%
    rename(
      age  = Age..years.,
      type = Presentation.type,
      sex  = Sex,
      year = Period,
      rate = Crude.rate.per.100.000.population
    ) %>%
    filter(
      trimws(age)  == "All ages",
      trimws(type) == "Total mental health presentations",
      trimws(sex)  == "Persons",
      grepl("^[0-9]{2}/[0-9]{2}$", trimws(year))
    ) %>%
    mutate(rate = gsub(",", "", trimws(as.character(rate)))) %>%
    clean_hs("rate", "Annual ED Visits", is_percent = FALSE) %>%
    distinct(year, .keep_all = TRUE) %>%
    arrange(year)
  
  kpi_ed_val <- ed_annual %>%
    filter(year == max(year)) %>%
    pull(rate) %>%
    round(1)
}

raw_selfharm <- read_hs("data/Intentional self-harm hospitalisations by Sex and Age (years).csv", skip = 1)

if (!is.null(raw_selfharm)) {
  selfharm_all <- raw_selfharm %>%
    rename(
      sex    = Sex,
      age    = Age..years.,
      period = Period,
      rate   = Age.standardised.rate.per.100.000.population
    ) %>%
    filter(
      trimws(age)  == "All ages",
      trimws(sex) %in% c("Persons", "Males", "Females"),
      grepl("^[0-9]{2}/[0-9]{2}$", trimws(period))
    ) %>%
    clean_hs("rate", "Self-Harm Hospitalisations", is_percent = FALSE) %>%
    distinct(sex, period, .keep_all = TRUE) %>%
    arrange(sex, period)
  
  kpi_selfharm_val <- selfharm_all %>%
    filter(trimws(sex) == "Persons", period == max(period)) %>%
    pull(rate) %>%
    round(1)
}

# ============================================================
# TAB 2: REGIONAL MAP — DATA
# ============================================================
map_distress <- raw_distress_lhd %>%
  rename(category = Category, LHD_NAME = LHD, year = Period, value = Per.cent) %>%
  filter(
    trimws(category) == "High or very high level",
    grepl("^[0-9]{4}$", trimws(as.character(year)))
  ) %>%
  clean_hs("value", "Map Distress", is_percent = TRUE) %>%
  mutate(
    year     = as.integer(trimws(as.character(year))),
    LHD_NAME = trimws(gsub(" LHD$", "", LHD_NAME))
  ) %>%
  filter(!is.na(year), LHD_NAME != "All LHDs") %>%
  distinct(LHD_NAME, year, .keep_all = TRUE) %>%
  arrange(LHD_NAME, year)

# Suicide by LHD
raw_suicide_lhd <- read_hs("data/Suicide by LHD.csv", skip = 1)

if (!is.null(raw_suicide_lhd)) {
  map_suicide <- raw_suicide_lhd %>%
    rename(
      LHD_NAME = LHD,
      year     = Period,
      value    = Age.standardised.rate.per.100.000.population
    ) %>%
    filter(grepl("^[0-9]{4}$", trimws(as.character(year)))) %>%
    clean_hs("value", "Suicide by LHD", is_percent = FALSE) %>%
    mutate(
      year     = as.integer(trimws(as.character(year))),
      LHD_NAME = trimws(gsub(" LHD$", "", LHD_NAME))
    ) %>%
    filter(!is.na(year), LHD_NAME != "All LHDs") %>%
    distinct(LHD_NAME, year, .keep_all = TRUE) %>%
    arrange(LHD_NAME, year)
} else {
  map_suicide <- data.frame(LHD_NAME = character(), year = integer(), value = numeric())
}

map_distress_years <- sort(unique(map_distress$year))
map_suicide_years  <- sort(unique(map_suicide$year))

lhd_sf_base <- nswgeo::lhd %>%
  st_transform(crs = 4326) %>%
  mutate(LHD_NAME = trimws(gsub(" Local Health District$", "", lhd_name)))

# ============================================================
# TAB 3: DEMOGRAPHICS — DATA
# ============================================================
demo_ed <- raw_ed_annual %>%
  rename(
    age  = Age..years.,
    type = Presentation.type,
    sex  = Sex,
    year = Period,
    rate = Crude.rate.per.100.000.population
  ) %>%
  filter(
    trimws(type) == "Total mental health presentations",
    trimws(sex)  %in% c("Males", "Females", "Persons"),
    trimws(age)  %in% c("12-17 years", "18-24 years", "25-34 years",
                        "35-64 years", "65 years and over"),
    grepl("^[0-9]{2}/[0-9]{2}$", trimws(year))
  ) %>%
  mutate(rate = gsub(",", "", trimws(as.character(rate)))) %>%
  clean_hs("rate", "Demo ED Visits", is_percent = FALSE) %>%
  mutate(
    age        = trimws(age),
    sex        = trimws(sex),
    year_label = paste0("20", substr(trimws(year), 1, 2), "/",
                        substr(trimws(year), 4, 5))
  ) %>%
  distinct(age, sex, year, .keep_all = TRUE) %>%
  arrange(sex, age, year)

demo_selfharm <- raw_selfharm %>%
  rename(
    sex    = Sex,
    age    = Age..years.,
    period = Period,
    rate   = Age.standardised.rate.per.100.000.population
  ) %>%
  filter(
    trimws(sex)  %in% c("Males", "Females", "Persons"),
    trimws(age)  %in% c("10-14 years", "15-19 years", "20-24 years",
                        "25-34 years", "35-44 years", "45-64 years"),
    grepl("^[0-9]{2}/[0-9]{2}$", trimws(period))
  ) %>%
  clean_hs("rate", "Demo Self-Harm", is_percent = FALSE) %>%
  mutate(
    age        = trimws(age),
    sex        = trimws(sex),
    year_label = paste0("20", substr(trimws(period), 1, 2), "/",
                        substr(trimws(period), 4, 5))
  ) %>%
  distinct(sex, age, period, .keep_all = TRUE) %>%
  arrange(sex, age, period)

demo_ed_latest       <- max(demo_ed$year,        na.rm = TRUE)
demo_selfharm_latest <- max(demo_selfharm$period, na.rm = TRUE)

# ============================================================
# TAB 4: COVID IMPACT — DATA
# ============================================================
raw_covid_lhd <- read_hs(
  "data/Mental health related emergency department visits (monthly) by Age (years) for Total mental health presentations by Sex and LHD.csv",
  skip = 1
)

if (!is.null(raw_covid_lhd)) {
  covid_ed_lhd <- raw_covid_lhd %>%
    rename(
      age    = Age..years.,
      type   = Presentation.type,
      sex    = Sex,
      lhd    = LHD,
      period = Period,
      rate   = Crude.rate.per.100.000.population
    ) %>%
    filter(
      trimws(type) == "Total mental health presentations",
      trimws(sex)  %in% c("Males", "Females", "Persons"),
      grepl("^[0-9]{4}-Q[1-4]$", trimws(period))
    ) %>%
    clean_hs("rate", "COVID ED by LHD", is_percent = FALSE) %>%
    mutate(
      quarter = as.integer(gsub(".*-Q", "", period)),
      yr      = as.integer(substr(period, 1, 4)),
      month   = as.Date(paste0(yr, "-", sprintf("%02d", (quarter - 1) * 3 + 1), "-01")),
      lhd     = trimws(lhd),
      sex     = trimws(sex),
      age     = trimws(age),
      period  = ifelse(month < as.Date("2020-03-01"), "Pre-COVID", "Post-COVID")
    ) %>%
    distinct(lhd, sex, age, month, .keep_all = TRUE) %>%
    arrange(lhd, sex, age, month) %>%
    rename(ed_visits = rate) %>%
    select(lhd, sex, age, month, ed_visits, period)
}

# ============================================================
# TAB 5: WELLBEING — DATA
# ============================================================
raw_life_sat <- read_hs("data/Life satisfaction by Category.csv", skip = 1)

if (!is.null(raw_life_sat)) {
  wellbeing_data <- raw_life_sat %>%
    rename(category = Category, year = Period, value = Per.cent) %>%
    filter(
      trimws(category) %in% c(
        "Delightful", "Pleased", "Mostly satisfied",
        "Mixed", "Mostly dissatisfied", "Unhappy", "Terrible"
      ),
      grepl("^[0-9]{4}$", trimws(as.character(year)))
    ) %>%
    clean_hs("value", "Life Satisfaction by Category", is_percent = TRUE) %>%
    mutate(
      year     = as.integer(trimws(as.character(year))),
      category = case_when(
        trimws(category) == "Delightful"          ~ "Delighted",
        trimws(category) == "Pleased"             ~ "Pleased",
        trimws(category) == "Mostly satisfied"    ~ "Mostly Satisfied",
        trimws(category) == "Mixed"               ~ "Mixed",
        trimws(category) == "Mostly dissatisfied" ~ "Mostly Dissatisfied",
        trimws(category) == "Unhappy"             ~ "Unhappy",
        trimws(category) == "Terrible"            ~ "Terrible"
      )
    ) %>%
    filter(!is.na(year)) %>%
    distinct(category, year, .keep_all = TRUE) %>%
    arrange(category, year)
}

wb_year_min <- min(wellbeing_data$year, na.rm = TRUE)
wb_year_max <- max(wellbeing_data$year, na.rm = TRUE)

raw_selfrated_mh <- read_hs("data/Self-rated mental health status by Category.csv", skip = 1)

if (!is.null(raw_selfrated_mh)) {
  selfrated_mh <- raw_selfrated_mh %>%
    rename(category = Category, year = Period, value = Per.cent) %>%
    filter(
      trimws(category) %in% c(
        "Excellent", "Very good", "Good", "Fair", "Poor", "Very poor"
      ),
      grepl("^[0-9]{4}$", trimws(as.character(year)))
    ) %>%
    clean_hs("value", "Self-Rated MH by Category", is_percent = TRUE) %>%
    mutate(year = as.integer(trimws(as.character(year)))) %>%
    filter(!is.na(year)) %>%
    distinct(category, year, .keep_all = TRUE) %>%
    arrange(category, year)
}

cat("\n=== All datasets loaded and cleaned successfully ===\n\n")

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span("The Silent Crisis", style = "font-size:15px; font-weight:bold;")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",     tabName = "overview",     icon = icon("chart-line")),
      menuItem("Regional Map", tabName = "regional_map", icon = icon("map")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("COVID Impact", tabName = "covid",        icon = icon("virus")),
      menuItem("Wellbeing",    tabName = "wellbeing",    icon = icon("heart"))
    ),
    hr(),
    p(style = "color:#aaa; font-size:11px; padding:10px;",
      "DATA5002 | Pandas | T1 2026")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .small-box { border-radius: 8px; }
    "))),
    
    tabItems(
      
      # ======================================================
      # TAB 1: OVERVIEW
      # ======================================================
      tabItem(tabName = "overview",
              
              fluidRow(
                valueBoxOutput("kpi_distress", width = 4),
                valueBoxOutput("kpi_ed",       width = 4),
                valueBoxOutput("kpi_selfharm", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary", solidHeader = TRUE,
                  selectInput("overview_lhd", "Local Health District:",
                              choices  = sort(unique(distress_lhd$LHD_NAME)),
                              selected = "All LHDs"),
                  sliderInput("overview_year", "Year Range:",
                              min = year_min, max = year_max,
                              value = c(year_min, year_max), sep = "", step = 1, ticks = FALSE)
                ),
                box(
                  title = "Psychological Distress in Adults Over Time (High & Very High)",
                  width = 9, status = "primary", solidHeader = TRUE,
                  plotlyOutput("overview_line", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distress Level Breakdown (Selected Year)",
                  width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("overview_bar", height = "260px")
                )
              )
      ),
      
      # ======================================================
      # TAB 2: REGIONAL MAP
      # ======================================================
      tabItem(tabName = "regional_map",
              
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary", solidHeader = TRUE,
                  
                  selectInput(
                    "map_indicator", "Indicator:",
                    choices = c(
                      "Psychological Distress (High/Very High %)" = "distress",
                      "Suicide Rate (per 100,000)"                = "suicide"
                    ),
                    selected = "distress"
                  ),
                  
                  hr(),
                  uiOutput("map_year_ui"),
                  hr(),
                  
                  p(style = "font-size:11px; color:grey;",
                    strong("Distress:"), " % adults with high or very high psychological distress.",
                    br(), br(),
                    strong("Suicide:"), " Age-standardised deaths per 100,000. Available from 2011.",
                    br(), br(),
                    "Click a region on the map for details.",
                    br(), br(),
                    "Note: Far West LHD suppressed in suicide data due to small numbers.")
                ),
                
                box(
                  title = uiOutput("map_title"),
                  width = 9, status = "warning", solidHeader = TRUE,
                  leafletOutput("regional_map_plot", height = "480px")
                )
              ),
              
              fluidRow(
                box(
                  title = "LHD Rankings — Selected Year (highest to lowest)",
                  width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("map_ranking_bar", height = "280px")
                )
              )
      ),
      
      # ======================================================
      # TAB 3: DEMOGRAPHICS
      # ======================================================
      tabItem(tabName = "demographics",
              
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary", solidHeader = TRUE,
                  
                  radioButtons(
                    "demo_sex", "Sex:",
                    choices  = c("Persons", "Males", "Females"),
                    selected = "Persons"
                  ),
                  
                  hr(),
                  p(style = "font-size:11px; color:grey;",
                    "Both charts show data for the latest available financial year.",
                    br(), br(),
                    strong("ED Visits:"), " Crude rate per 100,000 population.",
                    br(),
                    strong("Self-Harm:"), " Age-standardised rate per 100,000.",
                    br(), br(),
                    "Switch between Males / Females / Persons to compare how",
                    "the age burden shifts by sex.")
                ),
                
                box(
                  title = "Mental Health ED Visits by Age Group (Latest Year)",
                  width = 9, status = "primary", solidHeader = TRUE,
                  plotlyOutput("demo_ed_chart", height = "320px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Self-Harm Hospitalisations by Age Group (Latest Year)",
                  width = 12, status = "danger", solidHeader = TRUE,
                  plotlyOutput("demo_selfharm_chart", height = "320px")
                )
              )
      ),
      
      # ======================================================
      # TAB 4: COVID IMPACT
      # ======================================================
      tabItem(tabName = "covid",
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary", solidHeader = TRUE,
                  radioButtons("covid_view", "View:",
                               choices  = c("Both", "Pre-COVID only", "Post-COVID only"),
                               selected = "Both"),
                  hr(),
                  selectInput("covid_lhd", "Local Health District:",
                              choices  = c("All LHDs", sort(unique(
                                covid_ed_lhd$lhd[covid_ed_lhd$lhd != "All LHDs"]
                              ))),
                              selected = "All LHDs"),
                  hr(),
                  radioButtons("covid_gender", "Gender:",
                               choices  = c("Persons", "Males", "Females"),
                               selected = "Persons"),
                  hr(),
                  selectInput("covid_age", "Age Group:",
                              choices  = c("Total: 12 years and over",
                                           "12-17 years",
                                           "18-24 years",
                                           "25-34 years",
                                           "35-64 years",
                                           "65 years and over"),
                              selected = "Total: 12 years and over"),
                  hr(),
                  p(style = "font-size:11px; color:grey;",
                    "The dotted vertical line marks March 2020 —",
                    "the start of COVID-19 restrictions in NSW.",
                    br(), br(),
                    "Data is reported quarterly.")
                ),
                box(
                  title = "Mental Health ED Visits — Before vs After March 2020",
                  width = 9, status = "danger", solidHeader = TRUE,
                  plotlyOutput("covid_chart", height = "400px")
                )
              )
      ),
      
      # ======================================================
      # TAB 5: WELLBEING
      # ======================================================
      tabItem(tabName = "wellbeing",
              
              fluidRow(
                box(
                  title = "Life Satisfaction by Category (2022)",
                  width = 12, status = "success", solidHeader = TRUE,
                  plotlyOutput("wellbeing_bar_cat", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Self-Rated Mental Health Status by Category (2022)",
                  width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("selfrated_mh_chart", height = "300px")
                )
              )
      )
      
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ============================================================
  # TAB 1: OVERVIEW
  # ============================================================
  output$kpi_distress <- renderValueBox({
    valueBox(
      paste0(kpi_distress_val, "%"),
      paste0("Adults with High/Very High Distress (", year_max, ")"),
      icon = icon("brain"), color = "red"
    )
  })
  
  output$kpi_ed <- renderValueBox({
    valueBox(
      paste0(kpi_ed_val, " per 100k"),
      paste0("Mental Health ED Visit Rate (All ages) (", year_max, ")"),
      icon = icon("hospital"), color = "yellow"
    )
  })
  
  output$kpi_selfharm <- renderValueBox({
    valueBox(
      paste0(kpi_selfharm_val, " per 100k"),
      paste0("Self-Harm Hospitalisation Rate (All ages) (", year_max, ")"),
      icon = icon("heartbeat"), color = "orange"
    )
  })
  
  output$overview_line <- renderPlotly({
    validate(need(nrow(distress_lhd) > 0, "Distress by LHD data not loaded."))
    
    df <- distress_lhd %>%
      filter(
        LHD_NAME == input$overview_lhd,
        year >= input$overview_year[1],
        year <= input$overview_year[2]
      )
    
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    show_boundary <- input$overview_year[1] < 2011 && input$overview_year[2] >= 2011
    
    p <- plot_ly(df, x = ~year, y = ~value,
                 type = "scatter", mode = "lines+markers",
                 line   = list(width = 2.5, color = "#e74c3c"),
                 marker = list(color = "#e74c3c"),
                 hovertemplate = paste0(
                   "<b>%{x}</b><br>",
                   "High/Very High Distress: %{y:.1f}%<extra></extra>"
                 )) %>%
      layout(
        xaxis     = list(title = "Year"),
        yaxis     = list(title = "High/Very High Distress (%)", rangemode = "tozero"),
        hovermode = "x unified",
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
    
    if (show_boundary) {
      p <- p %>% layout(
        shapes = list(list(
          type = "line",
          x0 = 2011, x1 = 2011,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#7f8c8d", dash = "dash", width = 1.5)
        )),
        annotations = list(list(
          x = 2011, y = 0.98, yref = "paper",
          text = "LHD boundaries\nreorganised (2011)",
          showarrow = FALSE, xanchor = "left",
          font = list(size = 10, color = "#7f8c8d")
        ))
      )
    }
    p
  })
  
  output$overview_bar <- renderPlotly({
    df <- overview_distress %>%
      filter(year == input$overview_year[2]) %>%
      mutate(level = factor(level, levels = c("Low", "Medium", "High", "Very High")))
    
    validate(need(nrow(df) > 0, "No data available for selected year."))
    
    plot_ly(df, x = ~level, y = ~value,
            type = "bar", color = ~level,
            colors = c("Low" = "#2ecc71", "Medium" = "#f1c40f",
                       "High" = "#e67e22", "Very High" = "#e74c3c")) %>%
      layout(
        title = list(
          text = paste("Distress Level Breakdown —", input$overview_year[2]),
          font = list(size = 13)
        ),
        xaxis = list(title = "Distress Level"),
        yaxis = list(title = "Proportion (%)", rangemode = "tozero"),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  # ============================================================
  # TAB 2: REGIONAL MAP
  # ============================================================
  output$map_year_ui <- renderUI({
    years <- if (input$map_indicator == "distress") map_distress_years
    else map_suicide_years
    sliderInput(
      "map_year", "Year:",
      min = min(years), max = max(years),
      value = max(years), sep = "", step = 1, ticks = FALSE
    )
  })
  
  output$map_title <- renderUI({
    req(input$map_year)
    label <- if (input$map_indicator == "distress")
      paste("Psychological Distress — High/Very High % —", input$map_year)
    else
      paste("Suicide Rate per 100,000 —", input$map_year)
    tags$span(label)
  })

  output$regional_map_plot <- renderLeaflet({
    req(input$map_indicator, input$map_year)
    
    df <- if (input$map_indicator == "distress") {
      map_distress %>%
        filter(year == input$map_year) %>%
        select(LHD_NAME, value)
    } else {
      map_suicide %>%
        filter(year == input$map_year) %>%
        select(LHD_NAME, value)
    }
    
    validate(need(nrow(df) > 0, "No data available for the selected year."))

    lhd_sf <- lhd_sf_base %>%
      left_join(df, by = "LHD_NAME")
    
    pal_name     <- if (input$map_indicator == "distress") "YlOrRd" else "PuRd"
    units        <- if (input$map_indicator == "distress") "%" else "per 100k"
    value_label  <- if (input$map_indicator == "distress")
      "High/Very High Distress" else "Suicide Rate"
    
    pal <- colorNumeric(
      palette  = pal_name,
      domain   = range(df$value, na.rm = TRUE),
      na.color = "#cccccc"
    )
    
    popup_text <- paste0(
      "<b>", lhd_sf$LHD_NAME, " LHD</b><br>",
      value_label, ": ",
      ifelse(
        is.na(lhd_sf$value),
        "<i>Data suppressed</i>",
        paste0(round(lhd_sf$value, 1), " ", units)
      )
    )
    
    leaflet(lhd_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = 140.9, lat1 = -37.6, lng2 = 153.7, lat2 = -28.1) %>%
      addPolygons(
        fillColor    = ~pal(value),
        fillOpacity  = 0.75,
        color        = "white",
        weight       = 1.5,
        smoothFactor = 0.3,
        popup        = popup_text,
        highlight    = highlightOptions(
          weight       = 3,
          color        = "#444",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = df$value,
        title    = units,
        position = "bottomright",
        opacity  = 0.8
      ) %>%
      setView(lng = 146.5, lat = -32.5, zoom = 6)
  })
  
  output$map_ranking_bar <- renderPlotly({
    req(input$map_indicator, input$map_year)
    
    df <- if (input$map_indicator == "distress") {
      map_distress %>%
        filter(year == input$map_year) %>%
        select(LHD_NAME, value)
    } else {
      map_suicide %>%
        filter(year == input$map_year) %>%
        select(LHD_NAME, value)
    }
    
    validate(need(nrow(df) > 0, "No data for selected year."))
    
    df <- df %>% arrange(desc(value))
    
    bar_color    <- if (input$map_indicator == "distress") "#e67e22" else "#8e44ad"
    y_title      <- if (input$map_indicator == "distress")
      "High/Very High Distress (%)" else "Suicide Rate (per 100,000)"
    hover_suffix <- if (input$map_indicator == "distress") "%" else " per 100k"
    
    plot_ly(df,
            x = ~reorder(LHD_NAME, value),
            y = ~value,
            type = "bar",
            marker = list(color = bar_color, opacity = 0.85),
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              y_title, ": %{y:.1f}", hover_suffix, "<extra></extra>"
            )
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = y_title, rangemode = "tozero"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(b = 110)
      )
  })
  
  # ============================================================
  # TAB 3: DEMOGRAPHICS
  # ============================================================
  output$demo_ed_chart <- renderPlotly({
    validate(need(nrow(demo_ed) > 0, "ED visit data not loaded."))
    
    df <- demo_ed %>%
      filter(year == demo_ed_latest, sex == input$demo_sex) %>%
      mutate(age = factor(age, levels = c(
        "12-17 years", "18-24 years", "25-34 years",
        "35-64 years", "65 years and over"
      )))
    
    validate(need(nrow(df) > 0, "No data for selected sex."))
    
    age_colors <- c(
      "12-17 years"       = "#2c7bb6",
      "18-24 years"       = "#74add1",
      "25-34 years"       = "#fee090",
      "35-64 years"       = "#fdae61",
      "65 years and over" = "#d7191c"
    )
    
    plot_ly(df,
            x = ~age, y = ~rate,
            type   = "bar",
            color  = ~age,
            colors = age_colors,
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              "ED Visit Rate: %{y:,.1f} per 100,000<extra></extra>"
            )
    ) %>%
      layout(
        xaxis = list(title = "Age Group", tickangle = -20),
        yaxis = list(title = "Crude Rate per 100,000", rangemode = "tozero"),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        annotations = list(list(
          x = 0.5, y = 1.07, xref = "paper", yref = "paper",
          text = paste0("Financial Year: ", df$year_label[1],
                        "  |  Sex: ", input$demo_sex),
          showarrow = FALSE,
          font = list(size = 11, color = "#555")
        ))
      )
  })
  
  output$demo_selfharm_chart <- renderPlotly({
    validate(need(nrow(demo_selfharm) > 0, "Self-harm data not loaded."))
    
    df <- demo_selfharm %>%
      filter(period == demo_selfharm_latest, sex == input$demo_sex) %>%
      mutate(age = factor(age, levels = c(
        "10-14 years", "15-19 years", "20-24 years",
        "25-34 years", "35-44 years", "45-64 years"
      )))
    
    validate(need(nrow(df) > 0, "No data for selected sex."))
    
    age_colors <- c(
      "10-14 years" = "#2c7bb6",
      "15-19 years" = "#74add1",
      "20-24 years" = "#ffffbf",
      "25-34 years" = "#fee090",
      "35-44 years" = "#fdae61",
      "45-64 years" = "#d7191c"
    )
    
    plot_ly(df,
            x = ~age, y = ~rate,
            type   = "bar",
            color  = ~age,
            colors = age_colors,
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              "Hospitalisation Rate: %{y:.1f} per 100,000<extra></extra>"
            )
    ) %>%
      layout(
        xaxis = list(title = "Age Group", tickangle = -20),
        yaxis = list(title = "Age-Standardised Rate per 100,000", rangemode = "tozero"),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        annotations = list(list(
          x = 0.5, y = 1.07, xref = "paper", yref = "paper",
          text = paste0("Financial Year: ", df$year_label[1],
                        "  |  Sex: ", input$demo_sex),
          showarrow = FALSE,
          font = list(size = 11, color = "#555")
        ))
      )
  })
  
  # ============================================================
  # TAB 4: COVID IMPACT
  # ============================================================
  output$covid_chart <- renderPlotly({
    validate(need(nrow(covid_ed_lhd) > 0, "COVID ED data not loaded."))
    
    df <- covid_ed_lhd %>%
      filter(
        lhd == input$covid_lhd,
        sex == input$covid_gender,
        age == input$covid_age
      )
    
    df <- switch(input$covid_view,
                 "Pre-COVID only"  = df %>% filter(period == "Pre-COVID"),
                 "Post-COVID only" = df %>% filter(period == "Post-COVID"),
                 df
    )
    
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    
    plot_ly(df, x = ~month, y = ~ed_visits, color = ~period,
            colors = c("Pre-COVID" = "#3498db", "Post-COVID" = "#e74c3c"),
            type = "scatter", mode = "lines",
            line = list(width = 2)) %>%
      layout(
        xaxis     = list(title = "Quarter"),
        yaxis     = list(title = "Crude Rate per 100,000", rangemode = "tozero"),
        hovermode = "x unified",
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        shapes = list(list(
          type = "line",
          x0 = "2020-03-01", x1 = "2020-03-01",
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "black", dash = "dot", width = 2)
        )),
        annotations = list(list(
          x = "2020-03-01", y = 0.97, yref = "paper",
          text = "COVID-19 Start (Mar 2020)",
          showarrow = FALSE, xanchor = "left",
          font = list(size = 11, color = "black")
        ))
      )
  })
  
  # ============================================================
  # TAB 5: WELLBEING
  # ============================================================
  output$wellbeing_bar_cat <- renderPlotly({
    validate(need(nrow(wellbeing_data) > 0, "Life satisfaction data not loaded."))
    
    df <- wellbeing_data %>%
      mutate(category = factor(category, levels = c(
        "Delighted", "Pleased", "Mostly Satisfied",
        "Mixed", "Mostly Dissatisfied", "Unhappy", "Terrible"
      )))
    
    validate(need(nrow(df) > 0, "No data available."))
    
    plot_ly(df, x = ~category, y = ~value, color = ~category,
            colors = c(
              "Delighted"           = "#1a9850",
              "Pleased"             = "#91cf60",
              "Mostly Satisfied"    = "#d9ef8b",
              "Mixed"               = "#fee08b",
              "Mostly Dissatisfied" = "#fc8d59",
              "Unhappy"             = "#d73027",
              "Terrible"            = "#7b0000"
            ),
            type = "bar") %>%
      layout(
        xaxis = list(title = "Satisfaction Category", tickangle = -20),
        yaxis = list(title = "Proportion (%)", rangemode = "tozero"),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$selfrated_mh_chart <- renderPlotly({
    df <- selfrated_mh %>%
      filter(year == max(year)) %>%
      mutate(category = factor(category,
                               levels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Very poor")))
    
    validate(need(nrow(df) > 0, "No data available."))
    
    plot_ly(df, x = ~category, y = ~value, color = ~category,
            colors = c(
              "Excellent" = "#1a9850",
              "Very good" = "#91cf60",
              "Good"      = "#d9ef8b",
              "Fair"      = "#fee08b",
              "Poor"      = "#fc8d59",
              "Very poor" = "#d73027"
            ),
            type = "bar") %>%
      layout(
        xaxis = list(title = "Self-Rated Mental Health"),
        yaxis = list(title = "Proportion (%)", rangemode = "tozero"),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)