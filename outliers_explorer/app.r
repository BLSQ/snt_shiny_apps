
# Code for https://blsq-giuliapuntin.shinyapps.io/SNT_Outliers_Explorer_v0_2/ 

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
# Credentials for OH database & access to multiple workspaces' DB's:
#  * Imported from json file which is uploaded in the shinyapps.io server. 
#  * This file is not accessible to anyone viewing the app (so I guess creds are safe). 
#  * Possible to access the DB of different OH workspace thanks to json file 
#    *** HOWEVER, how do we deal with user's permissions??? ***


# 1. LOAD LIBRARIES --------------------------------------------------------------

library(shiny)
library(tidyverse)
library(DBI)
library(RPostgres)
library(glue)
library(scales)
library(jsonlite)


# 2. LOAD AND PREPARE DATA --------------------------------------------------------------------------

# --- Read all workspace configurations ---

CREDENTIALS_FILE_PATH <- "./connection_params.json"
workspaces_config <- list() # Initialize as an empty list
workspace_names <- character(0) # Initialize empty vector for names

if (file.exists(CREDENTIALS_FILE_PATH)) {
  tryCatch({
    # simplifyDataFrame to FALSE for robust parsing into lists
    config_data <- fromJSON(CREDENTIALS_FILE_PATH, simplifyDataFrame = FALSE)
    workspaces_config <- config_data$workspaces
    # Extract names for the UI selector
    workspace_names <- sapply(workspaces_config, `[[`, "name")
  }, error = function(e) {
    message(paste("Error reading or parsing credentials from", CREDENTIALS_FILE_PATH, ":", e$message))
  })
} else {
  message(paste("Credentials file not found at", CREDENTIALS_FILE_PATH))
}

# The app now starts with NO data loaded. 
# Data loading is fully reactive (user must first select a workspace from the drop-down list).


# 3. UI (with Tabs) ----------------------------------------------------

ui <- fluidPage(
  titlePanel("SNT Outliers Explorer"),
  
  # Check if any workspace configurations were loaded.
  if (length(workspace_names) == 0) {
    h3(glue("Error: Could not load any workspace configurations. Please check the '{CREDENTIALS_FILE_PATH}' file."))
  } else {
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Global Filters"),
        p("Filters apply across all tabs!"),
        hr(),
        
        # --- Workspace Selector ---
        # The user must select a workspace first!
        selectInput(inputId = "workspace", label = "1. Select Workspace",
                    choices = c("Select a workspace" = "", workspace_names),
                    selected = ""), # Nothing selected initially
        
        # these UI elements are rendered dynamically based on (after) workspace selection
        uiOutput("method_ui"),
        uiOutput("adm1_ui"),
        uiOutput("adm2_ui"),
        uiOutput("indicator_ui"),
        uiOutput("ou_names_ui")
      ),
      
      mainPanel(
        width = 9,
        # conditionalPanel: show a message until a workspace is selected
        conditionalPanel(
          condition = "input.workspace == ''",
          h4("Please select a workspace to begin.")
        ),
        conditionalPanel(
          condition = "input.workspace != ''",
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Summary View", uiOutput("summary_plot_ui")),
            tabPanel("Time Series View", uiOutput("outlier_plot_ui"))
          )
        )
      )
    )
  }
)



# 4. SERVER LOGIC (Using ggplot2) -----------------------

server <- function(input, output, session) {
  
  # get the connection details for the selected workspace
  selected_credentials <- reactive({
    req(input$workspace)
    # Find the list element that matches the selected workspace name
    selected_ws <- Filter(function(ws) ws$name == input$workspace, workspaces_config)
    # Ensure a unique match was found
    if (length(selected_ws) != 1) return(NULL)
    # Return the credentials list, which is now guaranteed to be a list object
    return(selected_ws[[1]]$credentials)
  })
  
  # --- Reactive Initial Choices ---
  # connect to the selected DB to get the first-level filter choices 
  initial_choices <- eventReactive(input$workspace, {
    req(input$workspace) # Require a workspace to be selected before running.
    creds <- selected_credentials()
    req(creds) # Stop if credentials weren't found
    
    id <- showNotification("Fetching initial choices from database...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    con <- NULL
    choices <- list()
    
    tryCatch({
      # `creds` is guaranteed to be a named list
      con <- DBI::dbConnect(RPostgres::Postgres(),
                            dbname = creds$dbname, host = creds$host, port = creds$port,
                            user = creds$username, password = creds$password, sslmode = 'require')
      
      adm1_query <- "SELECT DISTINCT \"ADM1_NAME\", \"ADM1_ID\" FROM flagged_outliers_allmethods_name_date ORDER BY \"ADM1_NAME\""
      choices$adm1 <- DBI::dbGetQuery(con, adm1_query)
      
      column_query <- "SELECT * FROM flagged_outliers_allmethods_name_date LIMIT 0"
      column_names <- colnames(DBI::dbGetQuery(con, column_query))
      choices$methods <- column_names[startsWith(column_names, "OUTLIER_")]
      
      return(choices)
    }, error = function(e) {
      message("Error fetching initial choices: ", e)
      return(list(error = as.character(e)))
    }, finally = {
      if (!is.null(con)) DBI::dbDisconnect(con)
    })
  })
  
  # --- DYNAMIC UI RENDERED FROM initial_choices() ---
  output$method_ui <- renderUI({
    choices_obj <- initial_choices()
    req(choices_obj)
    if (!is.null(choices_obj$error)) {
      return(p(style="color:red;", "DB Error: ", choices_obj$error))
    }
    selectInput(inputId = "method", label = "2. Select Outlier Detection Method",
                choices = choices_obj$methods, selected = choices_obj$methods[4])
  })
  
  output$adm1_ui <- renderUI({
    choices_obj <- initial_choices()
    req(choices_obj)
    # Don't render if there was an error or no ADM1 choices were found
    if (!is.null(choices_obj$error) || length(choices_obj$adm1) == 0) return()
    
    adm1_ch <- choices_obj$adm1
    selectInput(inputId = "adm1", label = "3. Select ADM 1",
                choices = setNames(adm1_ch$ADM1_ID, adm1_ch$ADM1_NAME))
  })
  
  data_reactive <- reactive({
    req(input$adm1)
    creds <- selected_credentials()
    req(creds) # Stop if credentials aren't valid
    
    id <- showNotification("Fetching data for ADM1...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    con <- NULL
    tryCatch({
      con <- DBI::dbConnect(RPostgres::Postgres(),
                            dbname = creds$dbname, host = creds$host, port = creds$port,
                            user = creds$username, password = creds$password, sslmode = 'require')
      
      sql_query <- glue("SELECT * FROM flagged_outliers_allmethods_name_date WHERE \"INDICATOR\" IN ('CONF', 'SUSP', 'TEST', 'PRES') AND \"ADM1_ID\" = '{input$adm1}'")
      data_for_adm1 <- DBI::dbGetQuery(con, statement = sql_query)
      
      if ("DATE" %in% colnames(data_for_adm1)) {
        data_for_adm1$DATE <- as.Date(data_for_adm1$DATE)
      }
      return(data_for_adm1)
    }, error = function(e) {
      message("Error fetching data for ADM1: ", e)
      showNotification(paste("Data fetch error:", as.character(e)), type = "error", duration = 10)
      return(data.frame())
    }, finally = {
      if (!is.null(con)) DBI::dbDisconnect(con)
    })
  })
  
  # The rest of the server logic: driven by the reactives above
  adm2_choices_reactive <- reactive({
    req(data_reactive())
    data_reactive() %>% distinct(ADM2_NAME, ADM2_ID) %>% arrange(ADM2_NAME)
  })
  
  output$adm2_ui <- renderUI({
    req(nrow(data_reactive()) > 0)
    choices <- adm2_choices_reactive()
    selectInput(inputId = "adm2", label = "4. Select ADM 2",
                choices = setNames(choices$ADM2_ID, choices$ADM2_NAME))
  })
  
  indicator_choices_reactive <- reactive({
    req(data_reactive(), input$adm2)
    data_reactive() %>% filter(ADM2_ID == input$adm2) %>% distinct(INDICATOR) %>% arrange(INDICATOR) %>% pull(INDICATOR)
  })
  
  output$indicator_ui <- renderUI({
    req(nrow(data_reactive()) > 0)
    selectizeInput(inputId = "indicators", label = "5. Select/Deselect Indicators",
                   choices = indicator_choices_reactive(), selected = indicator_choices_reactive(), multiple = TRUE)
  })
  
  ou_name_choices_reactive <- reactive({
    req(data_reactive(), input$adm2)
    data_reactive() %>% filter(ADM2_ID == input$adm2) %>% distinct(OU_NAME) %>% arrange(OU_NAME) %>% pull(OU_NAME)
  })
  
  output$ou_names_ui <- renderUI({
    req(nrow(data_reactive()) > 0)
    selectizeInput(inputId = "ou_names", label = "6. Select/Deselect Facilities",
                   choices = ou_name_choices_reactive(), selected = ou_name_choices_reactive(), multiple = TRUE)
  })
  
  filtered_data <- reactive({
    req(data_reactive(), input$method, input$adm2, input$indicators, input$ou_names)
    small_wide_data <- data_reactive() %>%
      filter(ADM2_ID == input$adm2, INDICATOR %in% input$indicators, OU_NAME %in% input$ou_names)
    small_long_data <- small_wide_data %>%
      pivot_longer(starts_with("OUTLIER_"), names_to = "OUTLIER_METHOD", values_to = "OUTLIER")
    small_long_data %>% filter(OUTLIER_METHOD == input$method)
  })
  
  total_height_reactive <- reactive({
    req(input$ou_names)
    n_ous <- length(input$ou_names)
    n_rows <- if (n_ous > 0) ceiling(n_ous / 3) else 1
    max(500, 150 + n_rows * 100)
  })
  
  output$summary_plot_ui <- renderUI({
    req(filtered_data())
    plotOutput("summary_plot", height = paste0(total_height_reactive(), "px"))
  })
  
  output$outlier_plot_ui <- renderUI({
    req(filtered_data())
    plotOutput("outlier_plot", height = paste0(total_height_reactive(), "px"))
  })
  
  output$summary_plot <- renderPlot({
    plot_data <- filtered_data(); req(nrow(plot_data) > 0)
    summary_data <- plot_data %>% count(OU_NAME, INDICATOR, OUTLIER) %>% group_by(OU_NAME, INDICATOR) %>%
      mutate(proportion = n / sum(n)) %>% ungroup() %>%
      mutate(Status = ifelse(OUTLIER, "Outlier", "Not an Outlier"),
             label_text = if_else(OUTLIER == TRUE & proportion > 0, scales::percent(proportion, accuracy = 1), NA_character_))
    adm1_ch <- initial_choices()$adm1
    adm1_name <- adm1_ch %>% filter(ADM1_ID == input$adm1) %>% pull(ADM1_NAME) %>% first()
    adm2_name <- data_reactive() %>% filter(ADM2_ID == input$adm2) %>% pull(ADM2_NAME) %>% first()
    main_title <- glue("Proportion of Outliers by Indicator"); sub_title <- glue("Displaying Facilities (OU_NAME) within ADM2: {adm2_name} (ADM1: {adm1_name})")
    ggplot(summary_data, aes(x = proportion, y = INDICATOR, fill = Status)) +
      geom_col(color = "#333333", linewidth = 0.1) + geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "white") +
      geom_text(aes(label = label_text), hjust = -0.1, color = "#6A1B9A", size = 3.5, fontface = "bold") +
      facet_wrap(~ OU_NAME, ncol = 3) + scale_x_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .1))) +
      scale_fill_manual(values = c("Outlier" = "#6A1B9A", "Not an Outlier" = "#F5F5F5"), na.value = "yellow") +
      labs(title = main_title, subtitle = sub_title, x = NULL, y = NULL, fill = NULL) +
      theme(legend.position = "top", strip.background = element_rect(fill = NA), strip.text = element_text(face = "bold", color = "#333333"),
            panel.grid.y = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
  }, res = 96)
  
  output$outlier_plot <- renderPlot({
    plot_data <- filtered_data(); req(nrow(plot_data) > 0)
    adm1_ch <- initial_choices()$adm1
    adm1_name <- adm1_ch %>% filter(ADM1_ID == input$adm1) %>% pull(ADM1_NAME) %>% first()
    adm2_name <- data_reactive() %>% filter(ADM2_ID == input$adm2) %>% pull(ADM2_NAME) %>% first()
    main_title <- glue("Outlier Method: {input$method}"); sub_title <- glue("Displaying Facilities (OU_NAME) within ADM2: {adm2_name} (ADM1: {adm1_name})")
    ggplot(plot_data, aes(x = DATE, y = VALUE, color = INDICATOR, group = INDICATOR)) +
      geom_line() + geom_point(data = . %>% filter(OUTLIER), shape = 1, size = 4, stroke = 1.2) +
      facet_wrap(~ OU_NAME, ncol = 3, scales = "free_y") + scale_color_viridis_d() +
      labs(title = main_title, subtitle = sub_title, x = NULL, y = NULL, color = "INDICATOR: ") +
      theme(legend.position = "top", strip.background = element_rect(fill = NA), strip.text = element_text(face = "bold", color = "#333333"),
            panel.grid.y = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "#F5F5F5"))
  }, res = 96)
}


# 5. RUN THE APP --------------------------------------------------------------------------------

shinyApp(ui, server)
