rm(list = ls())

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)
library(scales)

# Load the adoption data
adoption_data <- readRDS("adoption_data.rds")

# Define color palette
colors <- list(
  blue = "#1984a2",
  navy = "#003b4a",
  yellow = "#ffbd59",
  teal = "#26a69a",
  grey = "#7a96a4",
  light_blue = "#e6f2f5",
  light_yellow = "#fff4e6",
  light_teal = "#e6f5f3",
  light_grey = "#f5f7f8"
)

# Helper functions for UI components
infoBox <- function(title, text, icon = NULL, color = "blue", width = 6) {
  bg_color <- switch(color,
                     "blue" = colors$light_blue,
                     "yellow" = colors$light_yellow,
                     "teal" = colors$light_teal,
                     colors$light_grey)
  border_color <- switch(color,
                         "blue" = colors$blue,
                         "yellow" = colors$yellow,
                         "teal" = colors$teal,
                         colors$grey)
  
  div(class = paste0("col-sm-", width),
      div(class = "info-box",
          style = paste0("background: ", bg_color, 
                         "; border-left: 4px solid ", border_color,
                         "; padding: 15px; border-radius: 8px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
          if(!is.null(icon)) tags$h5(icon, title, style = paste0("color: ", colors$navy)) else tags$h5(title, style = paste0("color: ", colors$navy)),
          tags$p(text, style = paste0("margin: 0; color: ", colors$grey))
      )
  )
}

valueBox <- function(value, subtitle, icon = NULL, color = "blue") {
  bg_color <- switch(color,
                     "blue" = colors$light_blue,
                     "yellow" = colors$light_yellow,
                     "teal" = colors$light_teal,
                     "navy" = colors$light_grey,
                     colors$light_grey)
  border_color <- switch(color,
                         "blue" = colors$blue,
                         "yellow" = colors$yellow,
                         "teal" = colors$teal,
                         "navy" = colors$navy,
                         colors$grey)
  text_color <- switch(color,
                       "navy" = colors$navy,
                       border_color)
  
  div(class = "metric-box",
      style = paste0("background: ", bg_color, "; border-left: 4px solid ", 
                     border_color, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
      if(!is.null(icon)) div(style = paste0("float: right; color: ", colors$grey, ";"), icon),
      div(class = "metric-value", style = paste0("color: ", text_color, ";"), value),
      div(style = paste0("color: ", colors$grey, "; font-size: 14px;"), subtitle)
  )
}

renderValueBox <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  renderUI(func())
}

valueBoxOutput <- function(outputId, width = 12) {
  div(class = paste0("col-sm-", width),
      uiOutput(outputId)
  )
}

# Custom CSS with new color scheme
custom_css <- paste0("
  body {
    background-color: #fafbfc;
  }
  
  .navbar-default {
    background-color: ", colors$navy, " !important;
    border: none !important;
  }
  
  .navbar-default .navbar-brand,
  .navbar-default .navbar-nav > li > a {
    color: white !important;
  }
  
  .navbar-default .navbar-nav > .active > a {
    background-color: ", colors$blue, " !important;
    color: white !important;
  }
  
  h1, h2, h3, h4, h5 {
    color: ", colors$navy, ";
  }
  
  .well {
    background-color: white;
    border: 1px solid ", colors$light_grey, ";
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .btn-primary {
    background-color: ", colors$blue, ";
    border-color: ", colors$blue, ";
  }
  
  .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
    background-color: ", colors$navy, " !important;
    border-color: ", colors$navy, " !important;
  }
  
  .btn-success {
    background-color: ", colors$teal, ";
    border-color: ", colors$teal, ";
  }
  
  .btn-info {
    background-color: ", colors$yellow, ";
    border-color: ", colors$yellow, ";
    color: ", colors$navy, ";
  }
  
  .metric-box {
    background: white;
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 15px;
    transition: transform 0.2s;
  }
  
  .metric-box:hover {
    transform: translateY(-2px);
  }
  
  .metric-title {
    color: ", colors$navy, ";
    font-weight: bold;
    font-size: 14px;
    margin-bottom: 5px;
  }
  
  .metric-value {
    font-size: 24px;
    font-weight: bold;
  }
  
  .recommendation-box {
    background: ", colors$light_teal, ";
    border-radius: 8px;
    padding: 15px;
    margin: 10px 0;
    border-left: 4px solid ", colors$teal, ";
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .warning-box {
    background: ", colors$light_yellow, ";
    border-radius: 8px;
    padding: 15px;
    margin: 10px 0;
    border-left: 4px solid ", colors$yellow, ";
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .nav-tabs > li.active > a {
    background-color: ", colors$blue, " !important;
    color: white !important;
    border-color: ", colors$blue, " !important;
  }
  
  .nav-tabs > li > a {
    color: ", colors$grey, ";
    background-color: ", colors$light_grey, ";
    margin-right: 2px;
  }
  
  .nav-tabs > li > a:hover {
    background-color: ", colors$light_blue, ";
    color: ", colors$navy, ";
  }
  
  /* Radio buttons styling */
  input[type='radio']:checked + span {
    color: ", colors$blue, ";
    font-weight: bold;
  }
  
  /* Select input styling */
  .selectize-input {
    border-color: ", colors$grey, " !important;
  }
  
  .selectize-input.focus {
    border-color: ", colors$blue, " !important;
    box-shadow: 0 0 0 0.2rem rgba(25, 132, 162, 0.25) !important;
  }
  
  /* DataTable styling */
  table.dataTable thead th {
    background-color: ", colors$navy, " !important;
    color: white !important;
  }
  
  table.dataTable tbody tr:hover {
    background-color: ", colors$light_blue, " !important;
  }
  
  /* Title panel styling */
  .container-fluid > h2 {
    background: linear-gradient(135deg, ", colors$navy, ", ", colors$blue, ");
    color: white;
    padding: 20px;
    margin: -15px -15px 20px -15px;
    border-radius: 0;
    text-align: center;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  }
  
  /* Tooltip styling for metric boxes */
  .metric-box[title] {
    cursor: help;
  }
")

# Define region colors using the new palette
region_colors <- list(
  "Sub-Saharan Africa" = colors$yellow,
  "Middle East, North Africa, Afghanistan & Pakistan" = colors$teal,
  "South Asia" = colors$blue,
  "East Asia & Pacific" = colors$grey,
  "Latin America & Caribbean" = colors$yellow,
  "North America" = colors$teal,
  "Europe & Central Asia" = colors$navy
)

# Plotly layout theme
plotly_theme <- function() {
  list(
    plot_bgcolor = colors$light_grey,
    paper_bgcolor = 'white',
    font = list(color = colors$navy),
    xaxis = list(gridcolor = 'white', linecolor = colors$grey),
    yaxis = list(gridcolor = 'white', linecolor = colors$grey)
  )
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(custom_css))),
  
  titlePanel("Digital Divide Insights"),
  
  navbarPage("",
             tabPanel("Country Analysis",
                      fluidRow(
                        column(12,
                               wellPanel(
                                 style = "background: white; border-radius: 10px;",
                                 fluidRow(
                                   column(4,
                                          selectInput("country", "Select Country:", 
                                                      choices = NULL, selected = NULL,
                                                      width = "100%")
                                   ),
                                   column(4,
                                          selectizeInput("comparison_countries", "Compare with:",
                                                         choices = NULL, selected = NULL,
                                                         multiple = TRUE, width = "100%",
                                                         options = list(placeholder = "Select countries to compare"))
                                   ),
                                   column(4,
                                          radioButtons("view_mode", "View Mode:",
                                                       choices = c("Country Analysis" = "single", 
                                                                   "Comparative View" = "compare"),
                                                       selected = "single", inline = TRUE)
                                   )
                                 )
                               )
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.view_mode == 'single'",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   style = "background: white; border-radius: 10px;",
                                   h4("Actions", style = paste0("color: ", colors$navy, ";")),
                                   actionButton("run_scenarios", "Run Policy Scenarios", 
                                                class = "btn-primary btn-block",
                                                style = paste0("background-color: ", colors$blue, ";")),
                                   br(),
                                   downloadButton("download_data", "Download Raw Data",
                                                  class = "btn-info btn-block"),
                                   hr(style = paste0("border-color: ", colors$light_grey, ";")),
                                   h4("Key Metrics", style = paste0("color: ", colors$navy, ";")),
                                   uiOutput("key_metrics")
                                 )
                          ),
                          column(9,
                                 tabsetPanel(id = "main_tabs",
                                             tabPanel("Overview",
                                                      fluidRow(
                                                        column(12,
                                                               h3("Ownership and Use of Digital Technologies"),
                                                               plotlyOutput("adoption_plot", height = "400px"),
                                                               tags$p("Source: Findex Global Digital Connectivity Tracker 2025", 
                                                                      style = paste0("font-size: 11px; color: ", colors$grey, "; font-style: italic; margin-top: 5px;"))
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               infoBox(
                                                                 "Understanding the Gaps",
                                                                 "Coverage Gap: People without network access | Usage Gap: People with coverage but not using internet",
                                                                 icon = icon("info-circle"),
                                                                 color = "blue",
                                                                 width = 12
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               h4("Population in Coverage Gap"),
                                                               plotlyOutput("coverage_gap_chart", height = "350px"),
                                                               valueBoxOutput("coverage_gap_total")
                                                        ),
                                                        column(6,
                                                               h4("Population in Usage Gap"),
                                                               plotlyOutput("usage_gap_chart", height = "350px"),
                                                               valueBoxOutput("usage_gap_total")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               tags$p("Source: Calculated using data from the International Telecommunication Union (2022-24) and Findex Global Digital Connectivity Tracker 2025.", 
                                                                      style = paste0("font-size: 11px; color: ", colors$grey, "; font-style: italic; margin-top: 15px;"))
                                                        )
                                                      )
                                             ),
                                             
                                             tabPanel("Gender Analysis",
                                                      fluidRow(
                                                        column(12,
                                                               h3("Digital Access Gaps by Population Group"),
                                                               tags$p("Coverage gap: People without network access | Usage gap: People with coverage but not using internet", 
                                                                      style = paste0("font-size: 12px; color: ", colors$grey, "; margin-bottom: 20px;"))
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(4,
                                                               h4("All Adults Offline", style = "text-align: center;"),
                                                               plotlyOutput("adults_gap_donut", height = "350px")
                                                        ),
                                                        column(4,
                                                               h4("Women Offline", style = "text-align: center;"),
                                                               plotlyOutput("women_gap_donut", height = "350px")
                                                        ),
                                                        column(4,
                                                               h4("Men Offline", style = "text-align: center;"),
                                                               plotlyOutput("men_gap_donut", height = "350px")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               hr(),
                                                               h4("Absolute Impact: Women Affected by Digital Gaps"),
                                                               infoBox(
                                                                 "Understanding the Scale",
                                                                 "These numbers show the actual population of women affected by various digital gaps",
                                                                 icon = icon("users"),
                                                                 color = "teal",
                                                                 width = 12
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(4,
                                                               valueBoxOutput("women_no_internet")
                                                        ),
                                                        column(4,
                                                               valueBoxOutput("women_no_smartphone")
                                                        ),
                                                        column(4,
                                                               valueBoxOutput("women_gap_potential")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               hr(),
                                                               h3("Gender Gaps in Ownership and Use of Digital Technologies"),
                                                               plotlyOutput("gender_gap_comparison", height = "400px"),
                                                               tags$p("Note: Positive values indicate men have higher usage rates than women", 
                                                                      style = paste0("font-size: 11px; color: ", colors$grey, "; font-style: italic; margin-top: 5px;"))
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               h4("Detailed Gender Metrics"),
                                                               tags$p("Note: 'Women Without' and 'Men Without' columns show the total population (in millions) who do NOT have access to each technology", 
                                                                      style = paste0("font-size: 12px; color: ", colors$grey, "; font-style: italic; margin-bottom: 10px;")),
                                                               DT::dataTableOutput("gender_detailed_table")
                                                        )
                                                      )
                                             ),
                                             
                                             tabPanel("Policy Scenarios",
                                                      fluidRow(
                                                        column(12,
                                                               h3("Policy Impact Modeling"),
                                                               p("Click 'Run Policy Scenarios' in the sidebar to analyze different intervention strategies")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               h4("Key Insights"),
                                                               uiOutput("key_insights_box")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               h4("How Many Men and Women Can We Bring Online?"),
                                                               plotlyOutput("scenarios_comparison", height = "400px")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(12,
                                                               h6("Note: Scenarios 1 and 2 both rely on strong assumptions, modeling the limits of what is possible when infrastructure and demand-side end-user adoption policies are perfectly implemented without explicit gender intentionality. Scenario 1 assumes that expanding coverage to unconnected areas would yield use rates comparable to those in better-served regions, but use in underserved areas may be lower due to rurality, poverty, and lower education levels. Scenario 2 posits that there exist policies that equally benefit men and women while pursuing demand-side interventions. Scenario 3 specifically models the effects of closing gender gaps across all countries where men’s use exceeds women’s. To do so, we close gender gaps by bringing women's internet usage up to men's current levels, without any infrastructure expansion or increases in male use.  Scenarios are modelled with these strict assumptions to understand ranges; they do not provide exact numbers that will come online as a result."),
                                                               DT::dataTableOutput("scenarios_table")
                                                        )
                                                      )
                                             )
                                 )
                          )
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.view_mode == 'compare'",
                        fluidRow(
                          column(12,
                                 h3("Comparative Analysis"),
                                 tabsetPanel(
                                   tabPanel("Adoption Comparison",
                                            plotlyOutput("compare_adoption", height = "500px"),
                                            hr(),
                                            DT::dataTableOutput("compare_table")
                                   ),
                                   tabPanel("Gender Gap Comparison",
                                            plotlyOutput("compare_gender", height = "500px")
                                   ),
                                   tabPanel("Regional Benchmarking",
                                            plotlyOutput("regional_benchmark", height = "500px")
                                   )
                                 )
                          )
                        )
                      )
             ),
             
             tabPanel("Global Analysis",
                      fluidRow(
                        column(12,
                               h2("Global Digital Divide Analysis", 
                                  style = paste0("text-align: center; margin-bottom: 30px; color: ", colors$navy, ";"))
                        )
                      ),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 style = "background: white; border-radius: 10px;",
                                 h3("Coverage Gap by Region and Country", style = "text-align: center;"),
                                 plotlyOutput("global_sunburst_coverage", height = "500px"),
                                 tags$p("Population without network access (Millions)", 
                                        style = paste0("font-size: 11px; color: ", colors$grey, "; font-style: italic; text-align: center;"))
                               )
                        ),
                        column(6,
                               wellPanel(
                                 style = "background: white; border-radius: 10px;",
                                 h3("Usage Gap by Region and Country", style = "text-align: center;"),
                                 plotlyOutput("global_sunburst_usage", height = "500px"),
                                 tags$p("Population not using internet despite coverage (Millions)", 
                                        style = paste0("font-size: 11px; color: ", colors$grey, "; font-style: italic; text-align: center;"))
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(
                                 style = "background: white; border-radius: 10px;",
                                 h3("Regional Statistics", style = "text-align: center;"),
                                 fluidRow(
                                   column(6,
                                          h4("Coverage Gap Rankings"),
                                          plotlyOutput("regional_bar_chart_coverage", height = "350px"),
                                          hr(style = paste0("border-color: ", colors$light_grey, ";")),
                                          h5("Top 10 Countries - Coverage Gap"),
                                          DT::dataTableOutput("top_countries_table_coverage")
                                   ),
                                   column(6,
                                          h4("Usage Gap Rankings"),
                                          plotlyOutput("regional_bar_chart_usage", height = "350px"),
                                          hr(style = paste0("border-color: ", colors$light_grey, ";")),
                                          h5("Top 10 Countries - Usage Gap"),
                                          DT::dataTableOutput("top_countries_table_usage")
                                   )
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(
                                 style = "background: white; border-radius: 10px;",
                                 h4("Regional Summary Comparison"),
                                 fluidRow(
                                   column(6,
                                          h5("Coverage Gap Summary"),
                                          DT::dataTableOutput("regional_summary_table_coverage")
                                   ),
                                   column(6,
                                          h5("Usage Gap Summary"),
                                          DT::dataTableOutput("regional_summary_table_usage")
                                   )
                                 )
                               )
                        )
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize country choices
  observe({
    countries <- sort(unique(adoption_data$country_name))
    updateSelectInput(session, "country",
                      choices = countries,
                      selected = countries[1])
    updateSelectizeInput(session, "comparison_countries",
                         choices = countries)
  })
  
  # Reactive: filtered data
  country_data <- reactive({
    req(input$country)
    adoption_data %>% filter(country_name == input$country)
  })
  
  # Reactive: comparison data
  comparison_data <- reactive({
    if(length(input$comparison_countries) > 0) {
      adoption_data %>% 
        filter(country_name %in% c(input$country, input$comparison_countries))
    } else {
      country_data()
    }
  })
  
  # Prepare sunburst data for coverage
  sunburst_data_coverage <- reactive({
    adoption_data %>%
      filter(!is.na(adults_no_dominant_millions) & adults_no_dominant_millions > 0) %>%
      mutate(
        region = gsub("\\s*\\(.*", "", regionwb24_hi),  # Remove text after parenthesis
        value = adults_no_dominant_millions,
        percentage = gap_dominant_pct
      ) %>%
      select(country_name, region, value, percentage)
  })
  
  # Prepare sunburst data for usage
  sunburst_data_usage <- reactive({
    adoption_data %>%
      filter(!is.na(internet_usage_gap_all_millions) & internet_usage_gap_all_millions > 0) %>%
      mutate(
        region = gsub("\\s*\\(.*", "", regionwb24_hi),  # Remove text after parenthesis
        value = internet_usage_gap_all_millions,
        percentage = internet_usage_gap_all_pct
      ) %>%
      select(country_name, region, value, percentage)
  })
  
  # Global sunburst chart for coverage
  output$global_sunburst_coverage <- renderPlotly({
    data <- sunburst_data_coverage()
    
    # Debug: Check if we have data
    if(nrow(data) == 0) {
      return(plot_ly() %>% 
               layout(
                 title = "No data available for coverage gaps",
                 font = list(size = 14, color = colors$grey)
               ))
    }
    
    # Debug: Print to console for checking
    print(paste("Coverage data rows:", nrow(data)))
    print(paste("Unique regions:", paste(unique(data$region), collapse = ", ")))
    
    # Build hierarchical structure
    labels <- c("World")
    parents <- c("")
    values <- c(sum(data$value, na.rm = TRUE))
    
    # Add regions
    regions <- unique(data$region)
    for(r in regions) {
      labels <- c(labels, r)
      parents <- c(parents, "World")
      values <- c(values, sum(data$value[data$region == r], na.rm = TRUE))
    }
    
    # Add countries
    for(i in 1:nrow(data)) {
      labels <- c(labels, data$country_name[i])
      parents <- c(parents, data$region[i])
      values <- c(values, data$value[i])
    }
    
    # Debug: Print structure
    print(paste("Total labels:", length(labels)))
    print(paste("Total parents:", length(parents)))
    print(paste("Total values:", length(values)))
    
    # Create the sunburst
    plot_ly(
      labels = labels,
      parents = parents,
      values = values,
      type = 'sunburst',
      branchvalues = "total",
      hovertemplate = '%{label}<br>%{value:.1f}M<br>%{percentParent}<extra></extra>'
    ) %>%
      layout(
        margin = list(l = 0, r = 0, b = 0, t = 0),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Global sunburst chart for usage
  output$global_sunburst_usage <- renderPlotly({
    data <- sunburst_data_usage()
    
    # Debug: Check if we have data
    if(nrow(data) == 0) {
      return(plot_ly() %>% 
               layout(
                 title = "No data available for usage gaps",
                 font = list(size = 14, color = colors$grey)
               ))
    }
    
    # Debug: Print to console for checking
    print(paste("Usage data rows:", nrow(data)))
    print(paste("Unique regions:", paste(unique(data$region), collapse = ", ")))
    
    # Build hierarchical structure
    labels <- c("World")
    parents <- c("")
    values <- c(sum(data$value, na.rm = TRUE))
    
    # Add regions
    regions <- unique(data$region)
    for(r in regions) {
      labels <- c(labels, r)
      parents <- c(parents, "World")
      values <- c(values, sum(data$value[data$region == r], na.rm = TRUE))
    }
    
    # Add countries
    for(i in 1:nrow(data)) {
      labels <- c(labels, data$country_name[i])
      parents <- c(parents, data$region[i])
      values <- c(values, data$value[i])
    }
    
    # Debug: Print structure
    print(paste("Total labels:", length(labels)))
    print(paste("Total parents:", length(parents)))
    print(paste("Total values:", length(values)))
    
    # Create the sunburst
    plot_ly(
      labels = labels,
      parents = parents,
      values = values,
      type = 'sunburst',
      branchvalues = "total",
      hovertemplate = '%{label}<br>%{value:.1f}M<br>%{percentParent}<extra></extra>'
    ) %>%
      layout(
        margin = list(l = 0, r = 0, b = 0, t = 0),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Regional bar chart for coverage
  output$regional_bar_chart_coverage <- renderPlotly({
    data <- sunburst_data_coverage()
    
    regional_summary <- data %>%
      group_by(region) %>%
      summarise(
        total_millions = sum(value, na.rm = TRUE),
        avg_percentage = mean(percentage, na.rm = TRUE),
        countries = n()
      ) %>%
      arrange(desc(total_millions))
    
    plot_ly(regional_summary, 
            x = ~reorder(region, total_millions), 
            y = ~total_millions,
            type = 'bar',
            marker = list(color = sapply(regional_summary$region, 
                                         function(r) region_colors[[r]] %||% colors$grey)),
            text = paste0(round(regional_summary$total_millions, 1), "M<br>",
                          round(regional_summary$avg_percentage, 1), "% avg<br>",
                          regional_summary$countries, " countries"),
            textposition = 'outside',
            hovertemplate = '%{x}<br>Total: %{y:.1f}M<br>%{text}<extra></extra>') %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Population (Millions)"),
        margin = list(b = 120),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Regional bar chart for usage
  output$regional_bar_chart_usage <- renderPlotly({
    data <- sunburst_data_usage()
    
    regional_summary <- data %>%
      group_by(region) %>%
      summarise(
        total_millions = sum(value, na.rm = TRUE),
        avg_percentage = mean(percentage, na.rm = TRUE),
        countries = n()
      ) %>%
      arrange(desc(total_millions))
    
    plot_ly(regional_summary, 
            x = ~reorder(region, total_millions), 
            y = ~total_millions,
            type = 'bar',
            marker = list(color = sapply(regional_summary$region, 
                                         function(r) region_colors[[r]] %||% colors$grey)),
            text = paste0(round(regional_summary$total_millions, 1), "M<br>",
                          round(regional_summary$avg_percentage, 1), "% avg<br>",
                          regional_summary$countries, " countries"),
            textposition = 'outside',
            hovertemplate = '%{x}<br>Total: %{y:.1f}M<br>%{text}<extra></extra>') %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Population (Millions)"),
        margin = list(b = 120),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Top countries table for coverage
  output$top_countries_table_coverage <- DT::renderDataTable({
    data <- sunburst_data_coverage()
    
    top_countries <- data %>%
      arrange(desc(value)) %>%
      head(10) %>%
      mutate(
        Rank = row_number(),
        Country = country_name,
        Region = substr(region, 1, 20),
        `Pop (M)` = round(value, 2),
        `Gap %` = round(percentage, 1)
      ) %>%
      select(Rank, Country, `Pop (M)`, `Gap %`)
    
    DT::datatable(top_countries,
                  options = list(dom = 't', paging = FALSE, searching = FALSE),
                  rownames = FALSE) %>%
      formatStyle('Pop (M)', fontWeight = 'bold', color = colors$blue)
  })
  
  # Top countries table for usage
  output$top_countries_table_usage <- DT::renderDataTable({
    data <- sunburst_data_usage()
    
    top_countries <- data %>%
      arrange(desc(value)) %>%
      head(10) %>%
      mutate(
        Rank = row_number(),
        Country = country_name,
        Region = substr(region, 1, 20),
        `Pop (M)` = round(value, 2),
        `Gap %` = round(percentage, 1)
      ) %>%
      select(Rank, Country, `Pop (M)`, `Gap %`)
    
    DT::datatable(top_countries,
                  options = list(dom = 't', paging = FALSE, searching = FALSE),
                  rownames = FALSE) %>%
      formatStyle('Pop (M)', fontWeight = 'bold', color = colors$blue)
  })
  
  # Regional summary table for coverage
  output$regional_summary_table_coverage <- DT::renderDataTable({
    data <- sunburst_data_coverage()
    
    regional_stats <- data %>%
      group_by(Region = region) %>%
      summarise(
        N = n(),
        `Total (M)` = round(sum(value, na.rm = TRUE), 1),
        `Avg %` = round(mean(percentage, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(`Total (M)`))
    
    DT::datatable(regional_stats,
                  options = list(dom = 't', pageLength = 10),
                  rownames = FALSE) %>%
      formatStyle('Total (M)', fontWeight = 'bold', color = colors$blue)
  })
  
  # Regional summary table for usage
  output$regional_summary_table_usage <- DT::renderDataTable({
    data <- sunburst_data_usage()
    
    regional_stats <- data %>%
      group_by(Region = region) %>%
      summarise(
        N = n(),
        `Total (M)` = round(sum(value, na.rm = TRUE), 1),
        `Avg %` = round(mean(percentage, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(`Total (M)`))
    
    DT::datatable(regional_stats,
                  options = list(dom = 't', pageLength = 10),
                  rownames = FALSE) %>%
      formatStyle('Total (M)', fontWeight = 'bold', color = colors$blue)
  })
  
  # Key metrics boxes with new colors and tooltips
  output$key_metrics <- renderUI({
    data <- country_data()
    
    # Definitions for tooltips
    definitions <- list(
      total_pop = "Total number of people living in the country",
      adult_pop = "Population aged 15 years and older",
      income_group = "World Bank income classification based on GNI per capita",
      region = "World Bank geographic region classification",
      smartphone = "Percentage of adults who own a smartphone capable of accessing the internet",
      internet = "Percentage of adults who have used the internet in the past 3 months",
      coverage_gap = "Percentage of adults without access to broadband network coverage (3G or better)",
      usage_gap = "Percentage of adults who have network coverage but do not use the internet",
      gender_gap = "Difference in internet usage rates between men and women (in percentage points)"
    )
    
    # Demographics at the top
    demo_metrics <- list(
      list(title = "Total Population", 
           value = paste0(round(data$total_population[1]/1e6, 2), "M"),
           subtitle = "Total country population",
           color = "grey",
           tooltip = definitions$total_pop),
      list(title = "Adult Population", 
           value = paste0(round(data$adult_population[1]/1e6, 2), "M"),
           subtitle = "Population 15+ years",
           color = "grey",
           tooltip = definitions$adult_pop),
      list(title = "Income Group", 
           value = gsub(" income", "", data$incomegroupwb24[1]),
           subtitle = "World Bank classification",
           color = "grey",
           tooltip = definitions$income_group),
      list(title = "Region", 
           value = gsub("\\s*\\(.*", "", data$regionwb24_hi[1]),
           subtitle = "Geographic region",
           color = "grey",
           tooltip = definitions$region)
    )
    
    # Key metrics in new order
    metrics <- list(
      list(title = "Smartphone Ownership", 
           value = paste0(round(data$smartphone_ownership_all_pct[1], 1), "%"),
           subtitle = paste0(round(data$smartphone_ownership_all_millions[1], 1), "M owners"),
           color = "navy",
           tooltip = definitions$smartphone),
      list(title = "Internet Usage", 
           value = paste0(round(data$internet_usage_all_pct[1], 1), "%"),
           subtitle = paste0(round(data$internet_usage_all_millions[1], 1), "M users"),
           color = "blue",
           tooltip = definitions$internet),
      list(title = "Coverage Gap", 
           value = paste0(round(data$gap_dominant_pct[1], 1), "%"),
           subtitle = paste0(round(data$adults_no_dominant_millions[1], 1), "M offline"),
           color = "yellow",
           tooltip = definitions$coverage_gap),
      list(title = "Usage Gap", 
           value = paste0(round(data$internet_usage_gap_all_pct[1], 1), "%"),
           subtitle = paste0(round(data$internet_usage_gap_all_millions[1], 1), "M offline"),
           color = "teal",
           tooltip = definitions$usage_gap),
      list(title = "Gender Gap", 
           value = paste0(round(abs(data$internet_usage_male_pct[1] - 
                                      data$internet_usage_female_pct[1]), 1), "pp"),
           subtitle = paste0(round(data$internet_usage_gap_female_millions[1], 1), "M women offline"),
           color = "blue",
           tooltip = definitions$gender_gap)
    )
    
    tagList(
      # Demographics heading
      h5("Country Demographics", style = paste0("color: ", colors$navy, "; margin-bottom: 15px;")),
      
      # Demographic metrics with panel colors and tooltips
      lapply(demo_metrics, function(m) {
        div(class = "metric-box",
            title = m$tooltip,
            style = paste0("background: white; border-left: 4px solid ", colors$grey, 
                           "; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: help;"),
            div(class = "metric-title", m$title, 
                style = paste0("color: ", colors$grey, "; font-size: 13px; margin-bottom: 5px;")),
            div(class = "metric-value", 
                style = paste0("color: ", colors$navy, "; font-size: ", 
                               ifelse(nchar(m$value) > 15, "18px", "24px"), ";"),
                m$value),
            div(style = paste0("color: ", colors$grey, "; font-size: 12px;"), m$subtitle)
        )
      }),
      
      # Separator
      hr(style = paste0("border: 0; height: 1px; background: ", colors$light_grey, "; margin: 20px 0;")),
      
      # Key Metrics heading
      h5("Digital Metrics", style = paste0("color: ", colors$navy, "; margin-bottom: 15px;")),
      
      # Digital metrics with colored panels and tooltips
      lapply(metrics, function(m) {
        div(class = "metric-box",
            title = m$tooltip,
            style = paste0("background: ", 
                           switch(m$color,
                                  "blue" = colors$light_blue,
                                  "yellow" = colors$light_yellow,
                                  "teal" = colors$light_teal,
                                  "navy" = colors$light_grey,
                                  colors$light_grey),
                           "; border-left: 4px solid ",
                           switch(m$color,
                                  "blue" = colors$blue,
                                  "yellow" = colors$yellow,
                                  "teal" = colors$teal,
                                  "navy" = colors$navy,
                                  colors$grey),
                           "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); cursor: help;"),
            div(class = "metric-title", m$title, 
                style = paste0("color: ", colors$navy, "; font-size: 13px; margin-bottom: 5px;")),
            div(class = "metric-value", 
                style = paste0("color: ",
                               switch(m$color,
                                      "navy" = colors$navy,
                                      "blue" = colors$blue,
                                      "yellow" = colors$yellow,
                                      "teal" = colors$teal,
                                      colors$grey), ";"),
                m$value),
            div(style = paste0("color: ", colors$grey, "; font-size: 14px;"), m$subtitle)
        )
      })
    )
  })
  
  # Main adoption plot with new colors
  output$adoption_plot <- renderPlotly({
    data <- country_data()
    
    categories <- c("Internet", "Any Phone", "Smartphone", "Mobile Money Account")
    
    plot_data <- data.frame(
      Category = rep(categories, 3),
      Gender = rep(c("All Adults", "Women", "Men"), each = 4),
      Percentage = c(
        data$internet_usage_all_pct[1],
        data$phone_ownership_all_pct[1],
        data$smartphone_ownership_all_pct[1],
        ifelse(!is.na(data$mobileaccount_all[1]), data$mobileaccount_all[1]*100, 0),
        data$internet_usage_female_pct[1],
        data$phone_ownership_female_pct[1],
        data$smartphone_ownership_female_pct[1],
        ifelse(!is.na(data$mobileaccount_female[1]), data$mobileaccount_female[1]*100, 0),
        data$internet_usage_male_pct[1],
        data$phone_ownership_male_pct[1],
        data$smartphone_ownership_male_pct[1],
        ifelse(!is.na(data$mobileaccount_male[1]), data$mobileaccount_male[1]*100, 0)
      ),
      Millions = c(
        data$internet_usage_all_millions[1],
        data$phone_ownership_all_millions[1],
        data$smartphone_ownership_all_millions[1],
        ifelse(!is.na(data$mobileaccount_all[1]), 
               data$mobileaccount_all[1] * data$adult_population[1] / 1e6, 0),
        data$internet_usage_female_millions[1],
        data$phone_ownership_female_millions[1],
        data$smartphone_ownership_female_millions[1],
        ifelse(!is.na(data$mobileaccount_female[1]), 
               data$mobileaccount_female[1] * data$adult_pop_female[1] / 1e6, 0),
        data$internet_usage_male_millions[1],
        data$phone_ownership_male_millions[1],
        data$smartphone_ownership_male_millions[1],
        ifelse(!is.na(data$mobileaccount_male[1]), 
               data$mobileaccount_male[1] * data$adult_pop_male[1] / 1e6, 0)
      )
    )
    
    plot_data$Category <- factor(plot_data$Category, 
                                 levels = c("Internet", "Any Phone", "Smartphone", "Mobile Money Account"))
    plot_data$Gender <- factor(plot_data$Gender, levels = c("All Adults", "Women", "Men"))
    
    plot_data <- plot_data %>% arrange(Category, Gender)
    
    # Create text for bars - showing percentages
    plot_data$TextLabel <- paste0(round(plot_data$Percentage, 1), "%")
    
    plot_ly(plot_data, x = ~Category, y = ~Percentage, color = ~Gender,
            type = 'bar', 
            colors = c("All Adults" = colors$navy, "Women" = colors$teal, "Men" = colors$blue),
            text = ~TextLabel,
            textposition = 'outside',
            textfont = list(size = 11, color = colors$navy),
            hovertemplate = paste('%{x}<br>',
                                  '%{fullData.name}: %{y:.1f}%<br>',
                                  round(plot_data$Millions, 1), 'M people<extra></extra>')) %>%
      layout(
        yaxis = list(title = 'Adoption Rate (%)', range = c(0, 105), tickformat = '.0f'),
        xaxis = list(title = ''),
        barmode = 'group',
        showlegend = TRUE,
        legend = list(orientation = "h", y = 1.05, x = 0.5, xanchor = "center"),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Coverage gap chart with new colors
  output$coverage_gap_chart <- renderPlotly({
    data <- country_data()
    
    gap_data <- data.frame(
      Category = c("All Adults", "Women", "Men"),
      Population = c(
        data$adults_no_dominant_millions[1],
        data$women_no_dominant_millions[1],
        data$men_no_dominant_millions[1]
      )
    )
    
    gap_data$Category <- factor(gap_data$Category, levels = c("All Adults", "Women", "Men"))
    
    plot_ly(gap_data, x = ~Category, y = ~Population, type = 'bar',
            marker = list(color = c(colors$navy, colors$teal, colors$blue)),
            text = paste0(round(gap_data$Population, 2), "M"),
            textposition = 'outside',
            hovertemplate = '%{x}: %{y:.2f} million people<extra></extra>') %>%
      layout(
        yaxis = list(title = 'Population (Millions)'),
        xaxis = list(title = ''),
        showlegend = FALSE,
        title = list(text = "People Without Network Coverage", font = list(size = 14)),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Usage gap chart with new colors
  output$usage_gap_chart <- renderPlotly({
    data <- country_data()
    
    gap_data <- data.frame(
      Category = c("All Adults", "Women", "Men"),
      Population = c(
        data$internet_usage_gap_all_millions[1],
        data$internet_usage_gap_female_millions[1],
        data$internet_usage_gap_male_millions[1]
      )
    )
    
    gap_data$Category <- factor(gap_data$Category, levels = c("All Adults", "Women", "Men"))
    
    plot_ly(gap_data, x = ~Category, y = ~Population, type = 'bar',
            marker = list(color = c(colors$navy, colors$teal, colors$blue)),
            text = paste0(round(gap_data$Population, 2), "M"),
            textposition = 'outside',
            hovertemplate = '%{x}: %{y:.2f} million people<extra></extra>') %>%
      layout(
        yaxis = list(title = 'Population (Millions)'),
        xaxis = list(title = ''),
        showlegend = FALSE,
        title = list(text = "People Not Using Internet (Despite Coverage)", font = list(size = 14)),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Value boxes for gap totals
  output$coverage_gap_total <- renderValueBox({
    data <- country_data()
    valueBox(
      value = paste0(round(data$adults_no_dominant_millions[1], 1), "M"),
      subtitle = paste0("Coverage Gap (", round(data$gap_dominant_pct[1], 1), "% of adults)"),
      icon = icon("signal"),
      color = "yellow"
    )
  })
  
  output$usage_gap_total <- renderValueBox({
    data <- country_data()
    valueBox(
      value = paste0(round(data$internet_usage_gap_all_millions[1], 1), "M"),
      subtitle = paste0("Usage Gap (", round(data$internet_usage_gap_all_pct[1], 1), "% of adults)"),
      icon = icon("users"),
      color = "teal"
    )
  })
  
  # Gender gap comparison chart with new colors
  output$gender_gap_comparison <- renderPlotly({
    data <- country_data()
    
    gaps <- c(
      data$internet_usage_male_pct[1] - data$internet_usage_female_pct[1],
      data$phone_ownership_male_pct[1] - data$phone_ownership_female_pct[1],
      data$smartphone_ownership_male_pct[1] - data$smartphone_ownership_female_pct[1],
      ifelse(!is.na(data$mobileaccount_male[1]) & !is.na(data$mobileaccount_female[1]),
             (data$mobileaccount_male[1] - data$mobileaccount_female[1])*100, NA)
    )
    
    gap_data <- data.frame(
      Technology = c("Internet", "Any Phone", "Smartphone", "Mobile Money Account"),
      Gap = gaps,
      stringsAsFactors = FALSE
    )
    
    gap_data <- gap_data[!is.na(gap_data$Gap), ]
    gap_data$Technology <- factor(gap_data$Technology, levels = gap_data$Technology)
    gap_data$Color <- ifelse(gap_data$Gap > 0, colors$yellow, colors$teal)
    
    gap_data$HoverText <- ifelse(
      gap_data$Gap > 0,
      paste0(gap_data$Technology, "<br>Gender gap: ", round(abs(gap_data$Gap), 1), 
             " percentage points<br>Men have ", round(abs(gap_data$Gap), 1), 
             "pp higher usage than women"),
      paste0(gap_data$Technology, "<br>Gender gap: ", round(abs(gap_data$Gap), 1), 
             " percentage points<br>Women have ", round(abs(gap_data$Gap), 1), 
             "pp higher usage than men")
    )
    
    plot_ly(gap_data, x = ~Technology, y = ~Gap, type = 'bar',
            marker = list(color = ~Color),
            text = paste0(round(gap_data$Gap, 1), " pp"),
            textposition = 'outside',
            hovertemplate = ~HoverText,
            hoverinfo = 'text') %>%
      layout(
        yaxis = list(title = 'Gender Gap (percentage points)', zeroline = TRUE, 
                     zerolinewidth = 2, zerolinecolor = colors$grey),
        xaxis = list(title = ''),
        showlegend = FALSE,
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Absolute impact value boxes
  output$women_no_internet <- renderValueBox({
    data <- country_data()
    valueBox(
      value = paste0(round(data$internet_usage_gap_female_millions[1], 1), "M"),
      subtitle = "Total women offline",
      icon = icon("wifi"),
      color = "yellow"
    )
  })
  
  output$women_no_smartphone <- renderValueBox({
    data <- country_data()
    valueBox(
      value = paste0(round(data$women_no_dominant_millions[1], 1), "M"),
      subtitle = "Women in coverage gap",
      icon = icon("signal"),
      color = "teal"
    )
  })
  
  output$women_gap_potential <- renderValueBox({
    data <- country_data()
    women_usage_gap <- data$internet_usage_gap_female_millions[1] - data$women_no_dominant_millions[1]
    valueBox(
      value = paste0(round(women_usage_gap, 1), "M"),
      subtitle = "Women in usage gap",
      icon = icon("user-times"),
      color = "blue"
    )
  })
  
  # Adults offline donut chart with new colors
  output$adults_gap_donut <- renderPlotly({
    data <- country_data()
    
    total_offline <- data$internet_usage_gap_all_millions[1]
    coverage_gap <- data$adults_no_dominant_millions[1]
    usage_gap <- total_offline - coverage_gap
    
    coverage_pct <- (coverage_gap / total_offline) * 100
    usage_pct <- (usage_gap / total_offline) * 100
    
    donut_data <- data.frame(
      Category = c("Coverage Gap", "Usage Gap"),
      Value = c(coverage_gap, usage_gap),
      Percentage = c(coverage_pct, usage_pct),
      Label = c(
        paste0("Coverage Gap<br>", round(coverage_gap, 1), "M (", round(coverage_pct, 1), "%)"),
        paste0("Usage Gap<br>", round(usage_gap, 1), "M (", round(usage_pct, 1), "%)")
      )
    )
    
    plot_ly(donut_data, 
            labels = ~Label,
            values = ~Value,
            type = 'pie',
            hole = 0.65,
            marker = list(colors = c(colors$yellow, colors$teal), line = list(color = 'white', width = 2)),
            textinfo = 'none',
            hovertemplate = '%{label}<extra></extra>') %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.05, y = 0.5, font = list(size = 11)),
        annotations = list(
          list(text = paste0("<b>", round(total_offline, 1), "M</b>"),
               showarrow = FALSE,
               font = list(size = 16, color = colors$navy)),
          list(text = "Offline",
               showarrow = FALSE,
               font = list(size = 16, color = colors$grey),
               y = 0.4)
        ),
        paper_bgcolor = 'white',
        font = list(color = colors$navy),
        margin = list(l = 0, r = 100, t = 0, b = 0)
      )
  })
  
  # Women offline donut chart with new colors
  output$women_gap_donut <- renderPlotly({
    data <- country_data()
    
    total_offline <- data$internet_usage_gap_female_millions[1]
    coverage_gap <- data$women_no_dominant_millions[1]
    usage_gap <- total_offline - coverage_gap
    
    coverage_pct <- (coverage_gap / total_offline) * 100
    usage_pct <- (usage_gap / total_offline) * 100
    
    donut_data <- data.frame(
      Category = c("Coverage Gap", "Usage Gap"),
      Value = c(coverage_gap, usage_gap),
      Percentage = c(coverage_pct, usage_pct),
      Label = c(
        paste0("Coverage Gap<br>", round(coverage_gap, 1), "M (", round(coverage_pct, 1), "%)"),
        paste0("Usage Gap<br>", round(usage_gap, 1), "M (", round(usage_pct, 1), "%)")
      )
    )
    
    plot_ly(donut_data, 
            labels = ~Label,
            values = ~Value,
            type = 'pie',
            hole = 0.65,
            marker = list(colors = c(colors$yellow, colors$teal), line = list(color = 'white', width = 2)),
            textinfo = 'none',
            hovertemplate = '%{label}<extra></extra>') %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.05, y = 0.5, font = list(size = 11)),
        annotations = list(
          list(text = paste0("<b>", round(total_offline, 1), "M</b>"),
               showarrow = FALSE,
               font = list(size = 18, color = colors$navy)),
          list(text = "Offline",
               showarrow = FALSE,
               font = list(size = 12, color = colors$grey),
               y = 0.4)
        ),
        paper_bgcolor = 'white',
        font = list(color = colors$navy),
        margin = list(l = 0, r = 100, t = 0, b = 0)
      )
  })
  
  # Men offline donut chart with new colors
  output$men_gap_donut <- renderPlotly({
    data <- country_data()
    
    total_offline <- data$internet_usage_gap_male_millions[1]
    coverage_gap <- data$men_no_dominant_millions[1]
    usage_gap <- total_offline - coverage_gap
    
    coverage_pct <- (coverage_gap / total_offline) * 100
    usage_pct <- (usage_gap / total_offline) * 100
    
    donut_data <- data.frame(
      Category = c("Coverage Gap", "Usage Gap"),
      Value = c(coverage_gap, usage_gap),
      Percentage = c(coverage_pct, usage_pct),
      Label = c(
        paste0("Coverage Gap<br>", round(coverage_gap, 1), "M (", round(coverage_pct, 1), "%)"),
        paste0("Usage Gap<br>", round(usage_gap, 1), "M (", round(usage_pct, 1), "%)")
      )
    )
    
    plot_ly(donut_data, 
            labels = ~Label,
            values = ~Value,
            type = 'pie',
            hole = 0.65,
            marker = list(colors = c(colors$yellow, colors$teal), line = list(color = 'white', width = 2)),
            textinfo = 'none',
            hovertemplate = '%{label}<extra></extra>') %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.05, y = 0.5, font = list(size = 11)),
        annotations = list(
          list(text = paste0("<b>", round(total_offline, 1), "M</b>"),
               showarrow = FALSE,
               font = list(size = 18, color = colors$navy)),
          list(text = "Offline",
               showarrow = FALSE,
               font = list(size = 12, color = colors$grey),
               y = 0.4)
        ),
        paper_bgcolor = 'white',
        font = list(color = colors$navy),
        margin = list(l = 0, r = 100, t = 0, b = 0)
      )
  })
  
  # Gender detailed table
  output$gender_detailed_table <- DT::renderDataTable({
    data <- country_data()
    
    gender_metrics <- tibble(
      Metric = c("Internet Usage", "Phone Ownership", "Smartphone Ownership", "Mobile Account"),
      `Men (%)` = round(c(data$internet_usage_male_pct[1], 
                          data$phone_ownership_male_pct[1],
                          data$smartphone_ownership_male_pct[1], 
                          ifelse(!is.na(data$mobileaccount_male[1]), 
                                 data$mobileaccount_male[1]*100, NA)), 1),
      `Women (%)` = round(c(data$internet_usage_female_pct[1], 
                            data$phone_ownership_female_pct[1],
                            data$smartphone_ownership_female_pct[1], 
                            ifelse(!is.na(data$mobileaccount_female[1]), 
                                   data$mobileaccount_female[1]*100, NA)), 1),
      `Gap (pp)` = round(c(data$internet_usage_male_pct[1] - data$internet_usage_female_pct[1],
                           data$phone_ownership_male_pct[1] - data$phone_ownership_female_pct[1],
                           data$smartphone_ownership_male_pct[1] - data$smartphone_ownership_female_pct[1],
                           ifelse(!is.na(data$mobileaccount_male[1]) & !is.na(data$mobileaccount_female[1]),
                                  (data$mobileaccount_male[1] - data$mobileaccount_female[1])*100, NA)), 1),
      `Women Without (M)` = round(c(data$internet_usage_gap_female_millions[1],
                                    data$phone_adoption_gap_female_millions[1],
                                    data$smartphone_adoption_gap_female_millions[1],
                                    ifelse(!is.na(data$mobileaccount_female[1]),
                                           (1 - data$mobileaccount_female[1]) * data$adult_pop_female[1]/1e6, NA)), 2),
      `Men Without (M)` = round(c(data$internet_usage_gap_male_millions[1],
                                  data$phone_adoption_gap_male_millions[1],
                                  data$smartphone_adoption_gap_male_millions[1],
                                  ifelse(!is.na(data$mobileaccount_male[1]),
                                         (1 - data$mobileaccount_male[1]) * data$adult_pop_male[1]/1e6, NA)), 2)
    )
    
    gender_metrics <- gender_metrics[!is.na(gender_metrics$`Men (%)`) & !is.na(gender_metrics$`Women (%)`), ]
    
    DT::datatable(gender_metrics, 
                  options = list(dom = 't', paging = FALSE),
                  rownames = FALSE)
  })
  
  # Scenarios calculation
  scenarios_results <- eventReactive(input$run_scenarios, {
    data <- country_data()
    
    internet_male <- data$internet_usage_male_pct[1] / 100
    internet_female <- data$internet_usage_female_pct[1] / 100
    coverage_pct <- data$coverage_dominant_pct[1] / 100
    gap_pct <- data$gap_dominant_pct[1] / 100
    
    # Scenario 1: Close coverage gaps only (maintaining current usage patterns)
    # The proportional increase in coverage leads to proportional increase in usage
    s1_male_usage <- min(internet_male + (gap_pct * internet_male / coverage_pct), 1)
    s1_female_usage <- min(internet_female + (gap_pct * internet_female / coverage_pct), 1)
    s1_men_gain <- (s1_male_usage - internet_male) * data$adult_pop_male[1] / 1e6
    s1_women_gain <- (s1_female_usage - internet_female) * data$adult_pop_female[1] / 1e6
    
    # Scenario 2: Maximize usage with gender-agnostic approach (maintaining gender RATIO)
    gender_ratio <- internet_female / internet_male  # This is the ratio we maintain
    s2_men_final <- min(coverage_pct, 1)  # Men reach 100% of coverage
    s2_women_final <- min(s2_men_final * gender_ratio, 1)  # Women maintain the same ratio
    s2_men_gain <- (s2_men_final - internet_male) * data$adult_pop_male[1] / 1e6
    s2_women_gain <- (s2_women_final - internet_female) * data$adult_pop_female[1] / 1e6
    s2_women_offline <- (1 - s2_women_final) * data$adult_pop_female[1] / 1e6
    s2_men_offline <- (1 - s2_men_final) * data$adult_pop_male[1] / 1e6
    
    # Scenario 3: Scenario 2 PLUS closing gender gaps (stacked)
    # Additional women needed to reach parity with men at their maximized level
    s3_additional_women <- (s2_men_final - s2_women_final) * data$adult_pop_female[1] / 1e6
    s3_total_women_gain <- s2_women_gain + s3_additional_women
    s3_men_gain <- s2_men_gain  # Men gain stays same as scenario 2
    
    list(
      scenario1 = tibble(
        scenario = "Coverage Gaps Only",
        total_gain = s1_men_gain + s1_women_gain,
        men_gain = s1_men_gain,
        women_gain = s1_women_gain
      ),
      scenario2 = tibble(
        scenario = "Usage Gaps (Gender-Agnostic)",
        total_gain = s2_men_gain + s2_women_gain,
        men_gain = s2_men_gain,
        women_gain = s2_women_gain,
        women_offline = s2_women_offline,
        men_offline = s2_men_offline,
        women_final_pct = s2_women_final * 100,
        men_final_pct = s2_men_final * 100,
        gender_ratio = gender_ratio
      ),
      scenario3 = tibble(
        scenario = "Usage Gaps + Gender Parity",
        total_gain = s3_men_gain + s3_total_women_gain,
        men_gain = s3_men_gain,
        women_gain_base = s2_women_gain,
        women_gain_additional = s3_additional_women,
        women_gain = s3_total_women_gain
      )
    )
  })
  
  # Scenarios comparison plot with new colors
  output$scenarios_comparison <- renderPlotly({
    req(scenarios_results())
    results <- scenarios_results()
    
    # Prepare data for plotting
    plot_data <- data.frame(
      scenario = c("Coverage Gaps Only", "Usage Gaps (Gender-Agnostic)", "Usage Gaps + Gender Parity"),
      men_gain = c(results$scenario1$men_gain, results$scenario2$men_gain, results$scenario3$men_gain),
      women_gain_base = c(results$scenario1$women_gain, results$scenario2$women_gain, results$scenario3$women_gain_base),
      women_gain_additional = c(0, 0, results$scenario3$women_gain_additional)
    )
    
    # Create the plot
    p <- plot_ly(plot_data, x = ~scenario, y = ~men_gain, type = 'bar', 
                 name = 'Men', marker = list(color = colors$blue),
                 hovertemplate = 'Men: %{y:.1f}M<extra></extra>') %>%
      add_trace(y = ~women_gain_base, name = 'Women (Base)', 
                marker = list(color = colors$teal),
                hovertemplate = 'Women (Base): %{y:.1f}M<extra></extra>') %>%
      add_trace(y = ~women_gain_additional, name = 'Women (Additional for Parity)', 
                marker = list(color = colors$yellow, 
                              pattern = list(shape = "/")),
                hovertemplate = 'Women (Additional): %{y:.1f}M<extra></extra>')
    
    # Calculate totals for labels
    totals <- c(
      results$scenario1$total_gain,
      results$scenario2$total_gain,
      results$scenario3$total_gain
    )
    
    p %>% layout(
      yaxis = list(title = 'Millions of New Users'),
      xaxis = list(title = ''),
      barmode = 'stack',
      hovermode = 'x unified',
      annotations = list(
        list(x = 0, y = totals[1] + 2, text = paste0("Total: ", round(totals[1], 1), "M"),
             showarrow = FALSE, font = list(size = 12, color = colors$navy)),
        list(x = 1, y = totals[2] + 2, text = paste0("Total: ", round(totals[2], 1), "M"),
             showarrow = FALSE, font = list(size = 12, color = colors$navy)),
        list(x = 2, y = totals[3] + 2, text = paste0("Total: ", round(totals[3], 1), "M"),
             showarrow = FALSE, font = list(size = 12, color = colors$navy))
      ),
      plot_bgcolor = colors$light_grey,
      paper_bgcolor = 'white',
      font = list(color = colors$navy),
      legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")
    )
  })
  
  
  # Key Insights Box with new colors
  output$key_insights_box <- renderUI({
    req(scenarios_results())
    results <- scenarios_results()
    data <- country_data()
    
    total_population <- round(data$total_population[1]/1e6, 1)
    adult_population <- round(data$adult_population[1]/1e6, 1)
    total_offline <- round(data$internet_usage_gap_all_millions[1], 1)
    coverage_gap_millions <- round(data$adults_no_dominant_millions[1], 1)
    coverage_gap_pct <- round((coverage_gap_millions / total_offline) * 100, 1)
    usage_gap_pct <- round(100 - coverage_gap_pct, 1)
    gender_gap_pp <- round(data$internet_usage_male_pct[1] - data$internet_usage_female_pct[1], 1)
    women_offline <- round(data$internet_usage_gap_female_millions[1], 1)
    men_offline <- round(data$internet_usage_gap_male_millions[1], 1)
    
    s1_men <- round(results$scenario1$men_gain, 1)
    s1_women <- round(results$scenario1$women_gain, 1)
    
    # Calculate final numbers for scenario 2
    s2_women_online_final <- round(data$internet_usage_female_millions[1] + results$scenario2$women_gain, 1)
    s2_men_online_final <- round(data$internet_usage_male_millions[1] + results$scenario2$men_gain, 1)
    s2_women_final_pct <- round(results$scenario2$women_final_pct, 1)
    s2_men_final_pct <- round(results$scenario2$men_final_pct, 1)
    
    div(style = paste0("background: ", colors$light_blue, "; border: 2px solid ", colors$blue, 
                       "; border-radius: 10px; padding: 20px; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"),
        
        h4(icon("chart-line"), paste(input$country, "Digital Divide Analysis"), 
           style = paste0("color: ", colors$navy, "; margin-bottom: 15px;")),
        
        p(style = paste0("font-size: 14px; line-height: 1.8; color: ", colors$grey, ";"),
          strong(paste0(input$country, " has ", total_offline, " million people offline out of a total population of ", 
                        total_population, " million (", adult_population, " million adults)."),
                 style = paste0("color: ", colors$navy, ";")),
          br(),
          paste0("Of these, ", coverage_gap_millions, " million (", coverage_gap_pct, 
                 "%) are offline due to no broadband network coverage, and ", 
                 round(total_offline - coverage_gap_millions, 1), " million (", usage_gap_pct, 
                 "%) are offline as they do not use the internet despite having access to broadband network coverage."),
          br(),
          paste0("Gender gaps in internet use are ", gender_gap_pp, " percentage points, leaving ", 
                 women_offline, " million women offline, relative to ", men_offline, " million men.")
        ),
        
        hr(style = paste0("border-color: ", colors$blue, "; margin: 20px 0;")),
        
        h5("Scenario 1: If we extend network coverage to all unserved areas but keep the same usage patterns (including gender gaps), what happens?", 
           style = paste0("color: ", colors$navy, "; font-weight: bold;")),
        p(style = paste0("font-size: 14px; line-height: 1.6; color: ", colors$grey, ";"),
          
          "In broadband network covered areas, men are ", round(data$internet_usage_male_pct[1], 1), "% users, while women are ",
          round(data$internet_usage_female_pct[1], 1), "% users. If we extend coverage to everyone, ",
          "the same ", gender_gap_pp, " percentage point gap persists in newly covered areas.",
          br(), br(),
          strong(paste0("In ", input$country, ", closing coverage gaps while maintaining usage and gender gaps fixed will bring online ",
                        "a maximum of ", s1_men, " million men and ", s1_women, " million women."),
                 style = paste0("color: ", colors$navy, ";"))
        ),
        
        hr(style = paste0("border-color: ", colors$blue, "; margin: 20px 0;")),
        
        h5("Scenario 2: What if we maximize internet adoption for everyone with coverage but don't specifically target gender gaps?", 
           style = paste0("color: ", colors$navy, "; font-weight: bold;")),
        p(style = paste0("font-size: 14px; line-height: 1.6; color: ", colors$grey, ";"),
          
          "As women use the internet at the rate of ", round(results$scenario2$gender_ratio * 100, 1), 
          "% as much as men, when men's use reaches 100%, women's use would reach ",
          round(results$scenario2$gender_ratio * 100, 1), "%.",
          br(), br(),
          "If we target people within coverage areas without specific gender interventions, adoption rates reach ",
          s2_men_final_pct, "% for men and ", s2_women_final_pct, "% for women ",
          "(maintaining the current female to male ratio of internet use).",
          br(), br(),
          strong(paste0("Using gender-agnostic approaches, ", input$country, " would have ", 
                        s2_women_online_final, " million women online versus ", s2_men_online_final, " million men, ",
                        "leaving ", round(results$scenario2$women_offline, 1), " million women still offline."),
                 style = paste0("color: ", colors$navy, ";"))
        ),
        
        hr(style = paste0("border-color: ", colors$blue, "; margin: 20px 0;")),
        
        h5("Scenario 3: How many additional women can we bring online with gender-targeted interventions?", 
           style = paste0("color: ", colors$navy, "; font-weight: bold;")),
        p(style = paste0("font-size: 14px; line-height: 1.6; color: ", colors$grey, ";"),
          
          "This scenario combines maximizing usage (Scenario 2) with targeted interventions to close gender gaps, ",
          "bringing women's usage rates to match men's within covered areas.",
          br(), br(),
          strong(paste0("This would bring an additional ", round(results$scenario3$women_gain_additional, 1), 
                        " million women online beyond the gender-agnostic approach, for a total of ", 
                        round(results$scenario3$women_gain, 1), 
                        " million new women users, achieving full gender parity among those with coverage."),
                 style = paste0("color: ", colors$navy, ";"))
        )
    )
  })
  
  # Comparative adoption plot with new colors
  output$compare_adoption <- renderPlotly({
    req(input$comparison_countries)
    data <- comparison_data()
    
    # Create text labels for bars
    internet_text <- paste0(round(data$internet_usage_all_pct, 1), "%")
    smartphone_text <- paste0(round(data$smartphone_ownership_all_pct, 1), "%")
    phone_text <- paste0(round(data$phone_ownership_all_pct, 1), "%")
    
    plot_ly(data, x = ~country_name, y = ~internet_usage_all_pct,
            type = 'bar', name = 'Internet Usage',
            marker = list(color = colors$blue),
            text = internet_text, textposition = 'outside',
            textfont = list(size = 10, color = colors$navy)) %>%
      add_trace(y = ~smartphone_ownership_all_pct, name = 'Smartphone',
                marker = list(color = colors$teal),
                text = smartphone_text, textposition = 'outside',
                textfont = list(size = 10, color = colors$navy)) %>%
      add_trace(y = ~phone_ownership_all_pct, name = 'Phone',
                marker = list(color = colors$yellow),
                text = phone_text, textposition = 'outside',
                textfont = list(size = 10, color = colors$navy)) %>%
      layout(
        yaxis = list(title = 'Percentage (%)', range = c(0, 110)),
        xaxis = list(title = '', side = 'top'),
        barmode = 'group',
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy),
        margin = list(t = 80)
      )
  })
  
  # Comparison table
  output$compare_table <- DT::renderDataTable({
    req(input$comparison_countries)
    data <- comparison_data()
    
    summary_table <- data %>%
      select(country_name, regionwb24_hi, incomegroupwb24,
             internet_usage_all_pct, smartphone_ownership_all_pct,
             coverage_dominant_pct, gap_dominant_pct) %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      rename(
        Country = country_name,
        Region = regionwb24_hi,
        `Income Group` = incomegroupwb24,
        `Internet (%)` = internet_usage_all_pct,
        `Smartphone (%)` = smartphone_ownership_all_pct,
        `Coverage (%)` = coverage_dominant_pct,
        `Coverage Gap (%)` = gap_dominant_pct
      )
    
    DT::datatable(summary_table, 
                  options = list(pageLength = 10, dom = 'tip'),
                  rownames = FALSE)
  })
  
  # Compare gender gaps with new colors
  output$compare_gender <- renderPlotly({
    req(input$comparison_countries)
    data <- comparison_data()
    
    gender_gap_data <- data %>%
      mutate(internet_gap = internet_usage_male_pct - internet_usage_female_pct,
             phone_gap = phone_ownership_male_pct - phone_ownership_female_pct,
             smartphone_gap = smartphone_ownership_male_pct - smartphone_ownership_female_pct)
    
    plot_ly(gender_gap_data, x = ~country_name, y = ~internet_gap,
            type = 'bar', name = 'Internet Gap',
            marker = list(color = colors$yellow)) %>%
      add_trace(y = ~phone_gap, name = 'Phone Gap',
                marker = list(color = colors$teal)) %>%
      add_trace(y = ~smartphone_gap, name = 'Smartphone Gap',
                marker = list(color = colors$blue)) %>%
      layout(
        yaxis = list(title = 'Gender Gap (percentage points)'),
        xaxis = list(title = ''),
        barmode = 'group',
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Regional benchmarking with new colors
  output$regional_benchmark <- renderPlotly({
    data <- adoption_data %>%
      filter(regionwb24_hi == country_data()$regionwb24_hi[1])
    
    plot_ly(data, x = ~internet_usage_all_pct, y = ~country_name,
            type = 'bar', orientation = 'h',
            marker = list(color = ifelse(data$country_name == input$country, 
                                         colors$blue, colors$grey))) %>%
      layout(
        xaxis = list(title = 'Internet Usage (%)'),
        yaxis = list(title = ''),
        title = paste("Regional Comparison -", country_data()$regionwb24_hi[1]),
        plot_bgcolor = colors$light_grey,
        paper_bgcolor = 'white',
        font = list(color = colors$navy)
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$country), "_digital_adoption_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(country_data(), file)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)
