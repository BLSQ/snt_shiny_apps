
# Code for  https://blsq-giuliapuntin.shinyapps.io/SNT_Outliers_Explorer_v0_1/ 

# Shiny app code version improvement: 
#  super LIGHT ON MEMORY, so it can handle the full country (DRC in this case) dataset
#  even on the server of the free shinyapps.io account!
#   
# How: Avoid loading the full table from the OH database at once. 
#      Instead, only imports a subset based on user inputs.
# 
# Specifically:
#   * at start, runs just 2 tiny database queries:
#     1. to get the list of all unique ADM1 elements (for 1st filter)
#     2. to get the list of outlier methods based on column names ("OUTLIER_") (for 4th filter)
# 
#   * then, the main data only loads after the user picks a specific region from the dropdown.
# 
#   * additionally: imported data is pre-filtered to 
#     import only the rows for the 4 essential indicators ('CONF', 'SUSP', 'TEST', 'PRES').
#     (note that now the other indicators are not available as options in the app filter, 
#     if we want to add them we need to change the code here in this script)
# 
# Credentials for OH database:
#  * Imported from json file which is uploaded in the shinyapps.io server. 
#  * This file is not accessible to anyone viewing the app. 
#  * Currently connecting to DB of the "SNT development" ws, but editing the json file 
#    (e.g., adding nodes) could allow to log in to the DB of other workspaces, hence making this 
#    a generic app. However, how do we deal with user's permissions?



# 1. LOAD LIBRARIES --------------------------------------------------------------

library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(glue)
library(scales)
library(jsonlite)


# 2. LOAD AND PREPARE DATA --------------------------------------------------------------------------

# DB credentials from json file (not tracked by git)
CREDENTIALS_FILE_PATH <- "./connection_params_dev.json"

# Initialize database connection variables to NULL/empty
# (these will be populated from the JSON file if it exists and is valid)
dbname <- NULL
host <- NULL
port <- NULL
username <- NULL
password <- NULL

# Attempt to read credentials from the JSON file
if (file.exists(CREDENTIALS_FILE_PATH)) {
  tryCatch({
    creds <- fromJSON(CREDENTIALS_FILE_PATH)
    # Access nested elements for database credentials
    dbname <- creds$database$dbname
    host <- creds$database$host
    port <- creds$database$port
    username <- creds$database$username
    password <- creds$database$password
  }, error = function(e) {
    message(paste("Error reading credentials from", CREDENTIALS_FILE_PATH, ":", e$message))
  })
} else {
  message(paste("Credentials file not found at", CREDENTIALS_FILE_PATH))
}

# --- LIGHTWEIGHT INITIAL DATA FETCH ---
# Instead of loading all data, only load what's needed for the initial UI controls.
# The main data will be loaded reactively in the server section.

# Initialize choices as empty (to handle connection errors)
adm1_choices <- data.frame(ADM1_ID = character(0), ADM1_NAME = character(0))
outlier_method_choices <- character(0)

# Establish connection & read initial data
con <- NULL # Initialize connection to NULL
tryCatch({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = dbname,
                        host = host,
                        port = port,
                        user = username,
                        password = password,
                        sslmode = 'require')
  
  if (is.null(con) || !inherits(con, "PqConnection")) {
    stop("Failed to establish a valid PostgreSQL connection.")
  }
  
  # Query 1: Get only the ADM1 choices to populate the first filter. This is a small, fast query.
  adm1_query <- "SELECT DISTINCT \"ADM1_NAME\", \"ADM1_ID\" FROM flagged_outliers_allmethods_name_date ORDER BY \"ADM1_NAME\""
  adm1_choices <- DBI::dbGetQuery(con, adm1_query)
  
  # Query 2: Get column names to find outlier methods without loading any data rows (LIMIT 0).
  column_query <- "SELECT * FROM flagged_outliers_allmethods_name_date LIMIT 0"
  column_names <- colnames(DBI::dbGetQuery(con, column_query))
  outlier_method_choices <- column_names[startsWith(column_names, "OUTLIER_")]
  
  
}, error = function(e) {
  message("Error connecting to database or reading initial data: ", e$message)
}, finally = {
  # Disconnect from the database
  if (!is.null(con) && inherits(con, "PqConnection")) {
    DBI::dbDisconnect(con)
    message("Initial database connection closed.")
  }
})



# 3. UI (with Tabs) ----------------------------------------------------

ui <- fluidPage(
  titlePanel("SNT Outliers Explorer"),
  
  if (nrow(adm1_choices) == 0) {
    h3(glue("Error: Could not load initial data from the database. Please check connection details and table existence."))
  } else {
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Global Filters"),
        p("Filters apply across all tabs!"),
        hr(),
        selectInput(inputId = "method", label = "1. Select Outlier Detection Method",
                    choices = outlier_method_choices, selected = outlier_method_choices[4]),
        selectInput(inputId = "adm1", label = "2. Select ADM 1",
                    choices = setNames(adm1_choices$ADM1_ID, adm1_choices$ADM1_NAME)),
        uiOutput("adm2_ui"),
        uiOutput("indicator_ui"),
        uiOutput("ou_names_ui")
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "main_tabs",
          tabPanel(
            "Summary View",
            uiOutput("summary_plot_ui")
          ),
          tabPanel(
            "Time Series View",
            uiOutput("outlier_plot_ui")
          )
        )
      )
    )
  }
)



# 4. SERVER LOGIC (Using ggplot2) -----------------------

server <- function(input, output, session) {
  
  # --- REACTIVE DATA LOADING ---
  # This eventReactive block triggers ONLY when input$adm1 changes!
  # It connects to the DB, fetches data for the selected ADM1, and disconnects (cool ay?)
  data_reactive <- eventReactive(input$adm1, {
    req(input$adm1) # Ensure an ADM1 is selected.
    
    # Show a notification to the user that data is loading.
    id <- showNotification("Fetching data from database...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    con_reactive <- NULL
    data_for_adm1 <- data.frame()
    
    tryCatch({
      con_reactive <- DBI::dbConnect(RPostgres::Postgres(),
                                     dbname = dbname, host = host, port = port,
                                     user = username, password = password, sslmode = 'require')
      
      if (is.null(con_reactive) || !inherits(con_reactive, "PqConnection")) {
        stop("Failed to establish a valid PostgreSQL connection.")
      }
      
      # Highly specific query: fetch only the data for selected ADM1
      sql_query <- glue("SELECT * FROM flagged_outliers_allmethods_name_date WHERE \"INDICATOR\" IN ('CONF', 'SUSP', 'TEST', 'PRES') AND \"ADM1_ID\" = '{input$adm1}'")
      data_for_adm1 <- DBI::dbGetQuery(con_reactive, statement = sql_query)
      
      # Ensure DATE col is in Date format - unnecessary but yeah ...
      if ("DATE" %in% colnames(data_for_adm1)) {
        data_for_adm1$DATE <- as.Date(data_for_adm1$DATE)
      }
      return(data_for_adm1)
      
    }, error = function(e) {
      message("Error fetching data for ADM1: ", e$message)
      return(data.frame())
    }, finally = {
      if (!is.null(con_reactive) && inherits(con_reactive, "PqConnection")) {
        DBI::dbDisconnect(con_reactive)
        message(glue("Database connection for ADM1 '{input$adm1}' closed."))
      }
    })
  })
  
  
  # --- DYNAMIC UI (driven by data_reactive) ---
  adm2_choices_reactive <- reactive({
    req(data_reactive())
    data_reactive() %>%
      distinct(ADM2_NAME, ADM2_ID) %>%
      arrange(ADM2_NAME)
  })
  
  output$adm2_ui <- renderUI({
    # This UI element will only appear after loading data for an ADM1 
    req(nrow(data_reactive()) > 0)
    choices <- adm2_choices_reactive()
    selectInput(inputId = "adm2", label = "3. Select ADM 2",
                choices = setNames(choices$ADM2_ID, choices$ADM2_NAME))
  })
  
  indicator_choices_reactive <- reactive({
    req(data_reactive(), input$adm2)
    data_reactive() %>%
      filter(ADM2_ID == input$adm2) %>%
      distinct(INDICATOR) %>%
      arrange(INDICATOR) %>%
      pull(INDICATOR)
  })
  
  output$indicator_ui <- renderUI({
    req(nrow(data_reactive()) > 0)
    choices <- indicator_choices_reactive()
    selectizeInput(
      inputId = "indicators",
      label = "4. Select/Deselect Indicators",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })
  
  ou_name_choices_reactive <- reactive({
    req(data_reactive(), input$adm2)
    data_reactive() %>%
      filter(ADM2_ID == input$adm2) %>%
      distinct(OU_NAME) %>%
      arrange(OU_NAME) %>%
      pull(OU_NAME)
  })
  
  output$ou_names_ui <- renderUI({
    req(nrow(data_reactive()) > 0)
    choices <- ou_name_choices_reactive()
    selectizeInput(
      inputId = "ou_names",
      label = "5. Select/Deselect Facilities",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })
  
  # REACTIVE DATA FILTERING (uses data_reactive as source)
  filtered_data <- reactive({
    req(data_reactive(), input$method, input$adm2, input$indicators, input$ou_names)
    
    small_wide_data <- data_reactive() %>%
      filter(
        ADM2_ID == input$adm2,
        INDICATOR %in% input$indicators,
        OU_NAME %in% input$ou_names
      )
    
    small_long_data <- small_wide_data %>%
      pivot_longer(
        starts_with("OUTLIER_"),
        names_to = "OUTLIER_METHOD",
        values_to = "OUTLIER"
      )
    
    final_data <- small_long_data %>%
      filter(OUTLIER_METHOD == input$method)
    
    return(final_data)
  })
  
  # --- DYNAMIC HEIGHT CALCULATION ---
  HEIGHT_PER_ROW <- 100
  
  total_height_reactive <- reactive({
    req(input$ou_names)
    n_ous <- length(input$ou_names)
    n_rows <- if (n_ous > 0) ceiling(n_ous / 3) else 1
    base_height <- 150
    max(500, base_height + n_rows * HEIGHT_PER_ROW)
  })
  
  # --- DYNAMIC UI RENDERING ---
  output$summary_plot_ui <- renderUI({
    req(filtered_data())
    plotOutput("summary_plot", height = paste0(total_height_reactive(), "px"))
  })
  
  output$outlier_plot_ui <- renderUI({
    req(filtered_data())
    plotOutput("outlier_plot", height = paste0(total_height_reactive(), "px"))
  })
  
  # --- PLOT 1: PROPORTIONAL BAR CHARTS (TAB 1) ---
  output$summary_plot <- renderPlot({
    plot_data <- filtered_data()
    req(nrow(plot_data) > 0)
    summary_data <- plot_data %>%
      count(OU_NAME, INDICATOR, OUTLIER) %>%
      group_by(OU_NAME, INDICATOR) %>%
      mutate(proportion = n / sum(n)) %>%
      ungroup() %>%
      mutate(Status = ifelse(OUTLIER, "Outlier", "Not an Outlier"),
             label_text = if_else(
               OUTLIER == TRUE & proportion > 0,
               scales::percent(proportion, accuracy = 1),
               NA_character_
             )
      )
    adm1_name <- adm1_choices %>% filter(ADM1_ID == input$adm1) %>% pull(ADM1_NAME) %>% first()
    adm2_name <- data_reactive() %>% filter(ADM2_ID == input$adm2) %>% pull(ADM2_NAME) %>% first()
    main_title <- glue("Proportion of Outliers by Indicator")
    sub_title <- glue("Displaying Facilities (OU_NAME) within ADM2: {adm2_name} (ADM1: {adm1_name})")
    ggplot(summary_data, aes(x = proportion, y = INDICATOR, fill = Status)) +
      geom_col(color = "#333333", linewidth = 0.1) +
      geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "white") +
      geom_text(
        aes(label = label_text),
        hjust = -0.1, color = "#6A1B9A", size = 3.5, fontface = "bold"
      ) +
      facet_wrap(~ OU_NAME, ncol = 3) +
      scale_x_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .1))) +
      scale_fill_manual(values = c("Outlier" = "#6A1B9A", "Not an Outlier" = "#F5F5F5"), na.value = "yellow") +
      labs(title = main_title, subtitle = sub_title, x = NULL, y = NULL, fill = NULL) +
      theme(
        legend.position = "top",
        strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold", color = "#333333"),
        panel.grid.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()
      )
  }, res = 96)
  
  
  # --- PLOT 2: TIME SERIES (TAB 2) ---
  output$outlier_plot <- renderPlot({
    plot_data <- filtered_data()
    req(nrow(plot_data) > 0)
    adm1_name <- adm1_choices %>% filter(ADM1_ID == input$adm1) %>% pull(ADM1_NAME) %>% first()
    adm2_name <- data_reactive() %>% filter(ADM2_ID == input$adm2) %>% pull(ADM2_NAME) %>% first()
    main_title <- glue("Outlier Method: {input$method}")
    sub_title <- glue("Displaying Facilities (OU_NAME) within ADM2: {adm2_name} (ADM1: {adm1_name})")
    ggplot(plot_data, aes(x = DATE, y = VALUE, color = INDICATOR, group = INDICATOR)) +
      geom_line() +
      geom_point(data = . %>% filter(OUTLIER), shape = 1, size = 4, stroke = 1.2) +
      facet_wrap(~ OU_NAME, ncol = 3, scales = "free_y") +
      scale_color_viridis_d() +
      labs(title = main_title, subtitle = sub_title, x = NULL, y = NULL, color = "INDICATOR: ") +
      theme(
        legend.position = "top",
        strip.background = element_rect(fill = NA),
        strip.text = element_text(face = "bold", color = "#333333"),
        panel.grid.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5")
      )
  }, res = 96)
}

# 5. RUN THE APP --------------------------------------------------------------------------------

shinyApp(ui, server)
