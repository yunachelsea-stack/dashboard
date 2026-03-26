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

# Home page summary stats (global, in millions)
format_pop_millions <- function(x) {
  if (is.na(x)) return("N/A")
  if (x >= 1000) {
    paste0(round(x / 1000, 1), "B")
  } else if (x >= 10) {
    paste0(comma(round(x, 0)), "M")
  } else if (x >= 1) {
    paste0(round(x, 1), "M")
  } else if (x > 0) {
    paste0(comma(round(x * 1000, 0)), "K")
  } else {
    "0"
  }
}

format_pop_detail_millions <- function(x) {
  if (is.na(x)) return("N/A")
  if (x >= 1000) {
    paste0(round(x / 1000, 1), "B")
  } else if (x >= 10) {
    paste0(round(x, 1), "M")
  } else if (x >= 1) {
    paste0(round(x, 2), "M")
  } else if (x > 0) {
    paste0(comma(round(x * 1000, 0)), "K")
  } else {
    "0"
  }
}

format_pop_transition <- function(from_x, to_x) {
  if (is.na(from_x) || is.na(to_x)) return("N/A")

  rounded_from <- round(from_x, 2)
  rounded_to <- round(to_x, 2)
  diff_k <- abs(to_x - from_x) * 1000

  if (rounded_from == rounded_to && diff_k > 0 && diff_k < 10) {
    paste0(
      format_pop_detail_millions(from_x),
      " (",
      round(from_x * 1000, 1),
      "K) → ",
      format_pop_detail_millions(to_x),
      " (",
      round(to_x * 1000, 1),
      "K)"
    )
  } else {
    paste0(format_pop_detail_millions(from_x), " → ", format_pop_detail_millions(to_x))
  }
}

sanitize_diagnostic_usage_gap_pct <- function(country_name, value) {
  ifelse(country_name %in% c("Mauritania", "West Bank and Gaza"), pmax(value, 0), value)
}
home_stats <- adoption_data %>%
  summarise(
    offline_m = sum(internet_usage_gap_all_millions, na.rm = TRUE),
    coverage_gap_m = sum(adults_no_dominant_millions, na.rm = TRUE),
    usage_gap_m = sum(internet_usage_gap_all_millions - adults_no_dominant_millions, na.rm = TRUE),
    women_offline_m = sum(internet_usage_gap_female_millions, na.rm = TRUE)
  ) %>%
  mutate(
    coverage_gap_pct = ifelse(offline_m > 0, 100 * coverage_gap_m / offline_m, NA_real_),
    usage_gap_pct = ifelse(offline_m > 0, 100 * usage_gap_m / offline_m, NA_real_),
    women_share_pct = ifelse(offline_m > 0, 100 * women_offline_m / offline_m, NA_real_)
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
    background: linear-gradient(180deg, #f7fafc 0%, #eef4f7 100%);
    color: ", colors$navy, ";
  }
  
  .navbar-default {
    background-color: ", colors$navy, " !important;
    border: none !important;
    box-shadow: 0 10px 24px rgba(0, 43, 56, 0.18);
  }
  
  .navbar-default .navbar-brand,
  .navbar-default .navbar-nav > li > a {
    color: white !important;
    font-weight: 500;
    letter-spacing: 0.01em;
    padding-top: 20px;
    padding-bottom: 20px;
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
    border: 1px solid #dde7ee;
    border-radius: 18px;
    box-shadow: 0 14px 38px rgba(10, 55, 76, 0.08);
  }
  
  .btn-primary {
    background-color: ", colors$blue, ";
    border-color: ", colors$blue, ";
    border-radius: 999px;
    font-weight: 600;
    box-shadow: 0 10px 22px rgba(25, 132, 162, 0.22);
  }
  
  .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
    background-color: ", colors$navy, " !important;
    border-color: ", colors$navy, " !important;
  }
  
  .btn-success {
    background-color: ", colors$teal, ";
    border-color: ", colors$teal, ";
    border-radius: 999px;
    font-weight: 600;
  }
  
  .btn-info {
    background-color: ", colors$yellow, ";
    border-color: ", colors$yellow, ";
    color: ", colors$navy, ";
    border-radius: 999px;
    font-weight: 600;
  }
  
  .metric-box {
    background: white;
    border-radius: 12px;
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
    border-radius: 12px;
    padding: 15px;
    margin: 10px 0;
    border-left: 4px solid ", colors$teal, ";
    box-shadow: 0 6px 18px rgba(0,0,0,0.05);
  }
  
  .warning-box {
    background: ", colors$light_yellow, ";
    border-radius: 12px;
    padding: 15px;
    margin: 10px 0;
    border-left: 4px solid ", colors$yellow, ";
    box-shadow: 0 6px 18px rgba(0,0,0,0.05);
  }
  
  .nav-tabs > li.active > a {
    background-color: ", colors$blue, " !important;
    color: white !important;
    border-color: ", colors$blue, " !important;
  }
  
  .nav-tabs > li > a {
    color: ", colors$grey, ";
    background-color: ", colors$light_grey, ";
    margin-right: 4px;
    border-top-left-radius: 10px;
    border-top-right-radius: 10px;
  }
  
  .nav-tabs > li > a:hover {
    background-color: ", colors$light_blue, ";
    color: ", colors$navy, ";
  }
  
  input[type='radio']:checked + span {
    color: ", colors$blue, ";
    font-weight: bold;
  }
  
  .selectize-input {
    border-color: #c8d8e0 !important;
    border-radius: 12px !important;
    min-height: 46px;
    padding-top: 10px;
    padding-bottom: 10px;
    box-shadow: none !important;
  }
  
  .selectize-input.focus {
    border-color: ", colors$blue, " !important;
    box-shadow: 0 0 0 0.2rem rgba(25, 132, 162, 0.16) !important;
  }
  
  table.dataTable thead th {
    background-color: ", colors$navy, " !important;
    color: white !important;
  }
  
  table.dataTable tbody tr:hover {
    background-color: ", colors$light_blue, " !important;
  }
  
  .container-fluid > h2 {
    background: linear-gradient(135deg, ", colors$navy, ", ", colors$blue, ");
    color: white;
    padding: 24px;
    margin: -15px -15px 26px -15px;
    border-radius: 0;
    text-align: center;
    box-shadow: 0 16px 34px rgba(0,0,0,0.12);
  }
  
  .metric-box[title] {
    cursor: help;
  }
  
  .home-gap-title {
    color: ", colors$navy, ";
    margin-top: 6px;
    margin-bottom: 0;
    font-size: clamp(19px, 1.45vw, 22px);
    font-weight: 650;
    line-height: 1.2;
  }
  
  .home-gap-text {
    margin-top: 15px;
    color: ", colors$navy, ";
    font-size: clamp(15px, 1.1vw, 17px);
    line-height: 1.5;
    text-align: center;
  }
  
  .home-gap-stat {
    font-size: clamp(34px, 2.8vw, 44px);
    font-weight: 700;
    line-height: 1.1;
  }
  
  .home-section-shell {
    background: white;
    border: 1px solid #dbe7ef;
    border-radius: 20px;
    padding: 24px 24px 22px 24px;
    margin-bottom: 22px;
    box-shadow: 0 16px 40px rgba(7, 54, 73, 0.08);
  }
  
  .diagnostic-shell {
    background: white;
    border: 1px solid #dbe7ef;
    border-radius: 20px;
    padding: 24px;
    margin-bottom: 22px;
    box-shadow: 0 16px 40px rgba(7, 54, 73, 0.08);
  }
  
  .diagnostic-gap-card {
    border-radius: 18px;
    padding: 20px;
    min-height: 210px;
    box-shadow: 0 14px 32px rgba(7, 54, 73, 0.07);
    position: relative;
    transition: transform 0.18s ease, box-shadow 0.18s ease;
  }
  
  .diagnostic-gap-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 18px 38px rgba(7, 54, 73, 0.1);
  }
  
  .diagnostic-pill {
    display: inline-block;
    padding: 6px 12px;
    border-radius: 999px;
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.06em;
    text-transform: uppercase;
  }
  
  .home-hero {
    border-radius: 0 0 22px 22px;
    overflow: hidden;
    margin-left: -15px;
    margin-right: -15px;
    margin-bottom: 28px;
    min-height: 420px;
    box-shadow: 0 22px 50px rgba(7, 54, 73, 0.16);
    position: relative;
  }
  
  .home-hero-panel {
    max-width: 760px;
    padding: 36px 34px 32px 34px;
  }
  
  .home-hero-card {
    background: rgba(255,255,255,0.74);
    backdrop-filter: blur(4px);
    border: 1px solid rgba(255,255,255,0.55);
    border-radius: 18px;
    padding: 28px 28px 22px 28px;
    box-shadow: 0 14px 36px rgba(7, 54, 73, 0.12);
  }
  
  .home-gap-card {
    border-radius: 18px;
    box-shadow: 0 14px 32px rgba(7, 54, 73, 0.08);
    min-height: 250px;
    position: relative;
    overflow: hidden;
    transition: transform 0.18s ease, box-shadow 0.18s ease;
  }
  
  .home-gap-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 18px 40px rgba(7, 54, 73, 0.11);
  }
  
  .home-policy-frame {
    background: linear-gradient(180deg, #ffffff 0%, #f8fbfd 100%);
    border: 1px solid #dbe5ef;
    border-radius: 18px;
    padding: 16px 16px 10px 16px;
  }
  
  .home-explore-card {
    border-radius: 18px;
    padding: 20px;
    min-height: 340px;
    box-shadow: 0 14px 32px rgba(7, 54, 73, 0.07);
    transition: transform 0.18s ease, box-shadow 0.18s ease;
  }
  
  .home-explore-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 18px 38px rgba(7, 54, 73, 0.1);
  }
  
  .home-explore-card.global-card {
    background: linear-gradient(180deg, #fcfeff 0%, #f1f7fb 100%);
    border: 1px solid #d6e4ef;
  }
  
  .home-explore-card.country-card {
    background: linear-gradient(180deg, #fffdfa 0%, #fff5e8 100%);
    border: 1px solid #f0dfc0;
  }
  
  .home-explore-card.policy-card {
    background: linear-gradient(180deg, #fcfaff 0%, #f3ecff 100%);
    border: 1px solid #e4d8f5;
  }
  
  .home-explore-list {
    margin-top: 18px;
    margin-bottom: 22px;
  }
  
  .home-explore-list p {
    font-size: 15px;
    margin-bottom: 10px;
  }
  
  .home-card-button {
    border-radius: 999px !important;
    font-size: 17px !important;
    padding: 11px 15px !important;
    font-weight: 600;
    box-shadow: 0 12px 24px rgba(7, 54, 73, 0.12);
  }
  
  .diagnostic-top-shell {
    background: linear-gradient(135deg, rgba(255,255,255,0.98) 0%, rgba(240,247,250,0.98) 100%);
    border-top: 5px solid ", colors$blue, ";
  }
  
  .diagnostic-heading-copy {
    font-size: clamp(16px, 1.1vw, 18px);
    line-height: 1.6;
    margin-bottom: 0;
  }
  
  .diagnostic-shell h3 {
    font-size: clamp(24px, 1.9vw, 28px);
    line-height: 1.18;
  }
  
  .diagnostic-footnote {
    color: ", colors$navy, ";
    font-size: 13px;
    line-height: 1.6;
    margin-top: 18px;
    margin-bottom: 0;
  }
  
  @media (max-width: 1199px) {
    .home-hero-panel {
      padding: 28px 24px 24px 24px;
    }

    .home-hero-card {
      padding: 24px 22px 20px 22px;
    }

    .home-section-shell,
    .diagnostic-shell {
      padding: 20px;
    }

    .home-gap-card,
    .home-explore-card,
    .diagnostic-gap-card {
      min-height: auto;
    }
  }
  
  .plotly .modebar {
    opacity: 0.35;
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
             tabPanel("Home",
                     fluidRow(
                        column(12,
                               div(class = "home-hero", style = paste0(
                                 "background-image: linear-gradient(90deg, rgba(255,255,255,0.16) 0%, rgba(255,255,255,0.08) 40%, rgba(255,255,255,0.04) 70%, rgba(255,255,255,0.02) 100%), ",
                                 "url('", if (file.exists("www/hero.png")) "hero.png" else "app_coverage_usage_gap/www/hero.png", "'); ",
                                 "background-size: cover; background-position: center right; background-repeat: no-repeat; ",
                                 "position: relative;"
                               ),
                                   div(class = "home-hero-panel",
                                       div(class = "home-hero-card",
                                           h2("The Global Digital Divide",
                                             style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 14px; font-size: clamp(36px, 3.2vw, 46px); line-height: 1.08;")),
                                           tags$p(
                                             tags$strong("Today, 1.4 billion people across low- and middle-income countries remain offline."),
                                             " Digital connectivity has become a powerful driver of economic growth, poverty reduction, and job creation, making digital exclusion an increasingly urgent development challenge.",
                                             style = paste0("font-size: clamp(17px, 1.35vw, 19px); line-height: 1.6; color: ", colors$navy, "; margin-bottom: 12px;")
                                           ),
                                          tags$p(
                                            "This interactive dashboard draws on the latest data from the Global Findex 2025 Digital Connectivity Tracker and the International Telecommunication Union (ITU). Explore global, regional and gender patterns in internet access and usage, and dive deeper into country-level trends and policy insights.",
                                            style = paste0("font-size: clamp(16px, 1.2vw, 18px); line-height: 1.6; color: ", colors$navy, "; margin-bottom: 0;")
                                          )
                                      )
                                  )
                               )
                        )
                     ),
                      fluidRow(
                        column(12,
                               div(class = "home-section-shell",
                                   h3("Understand the Digital Divide through 3 Gaps", style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 10px;")),
                                   div(style = "margin-top: 8px;",
                                       fluidRow(
                                         column(4,
                                                div(class = "home-gap-card", style = paste0(
                                                  "background: ", colors$light_blue, "; border: 1px solid #d7e5ee; border-radius: 10px; ",
                                                  "box-shadow: 0 2px 4px rgba(0,0,0,0.06); min-height: 250px; position: relative;"
                                                ),
                                                    div(style = "padding: 20px;",
                                                        div(style = "display: flex; align-items: center; justify-content: center; gap: 14px;",
                                                            div(style = paste0(
                                                              "width: 44px; height: 44px; border-radius: 50%; background: ", colors$blue, "; ",
                                                              "display: inline-flex; align-items: center; justify-content: center; color: white; font-size: 18px;"
                                                            ),
                                                                icon("broadcast-tower")
                                                            ),
                                                            h3("Coverage Gap", class = "home-gap-title", style = "margin-top: 0;")
                                                        ),
                                                        p(
                                                          span("171 million", style = paste0("font-weight: 700; font-size: 1.4em; color: ", colors$blue, ";")),
                                                          " offline people live outside broadband reach.",
                                                          class = "home-gap-text"
                                                        )
                                                    ),
                                                    div(style = paste0(
                                                      "position: absolute; bottom: 0; width: 100%; text-align: center; ",
                                                      "padding: 10px 8px; border-top: 1px solid #d7e5ee; background: white; ",
                                                      "border-radius: 0 0 10px 10px; color: ", colors$blue, ";"
                                                    ),
                                                        span("12%", class = "home-gap-stat")
                                                    )
                                                )
                                         ),
                                         column(4,
                                                div(class = "home-gap-card", style = paste0(
                                                  "background: ", colors$light_yellow, "; border: 1px solid #f2e0c3; border-radius: 10px; ",
                                                  "box-shadow: 0 2px 4px rgba(0,0,0,0.06); min-height: 250px; position: relative;"
                                                ),
                                                    div(style = "padding: 20px;",
                                                        div(style = "display: flex; align-items: center; justify-content: center; gap: 14px;",
                                                            div(style = paste0(
                                                              "width: 44px; height: 44px; border-radius: 50%; background: ", colors$yellow, "; ",
                                                              "display: inline-flex; align-items: center; justify-content: center; color: ", colors$navy, "; font-size: 18px;"
                                                            ),
                                                                icon("mobile-alt")
                                                            ),
                                                            h3("Usage Gap", class = "home-gap-title", style = "margin-top: 0;")
                                                        ),
                                                        p(
                                                          span("1.28 billion", style = "font-weight: 700; font-size: 1.4em; color: #a9791f;"),
                                                          " offline people have broadband coverage but do not use the internet.",
                                                          class = "home-gap-text"
                                                        )
                                                    ),
                                                    div(style = paste0(
                                                      "position: absolute; bottom: 0; width: 100%; text-align: center; ",
                                                      "padding: 10px 8px; border-top: 1px solid #f2e0c3; background: white; ",
                                                      "border-radius: 0 0 10px 10px; color: #a9791f;"
                                                    ),
                                                        span("88%", class = "home-gap-stat")
                                                    )
                                                )
                                         ),
                                         column(4,
                                                div(class = "home-gap-card", style = paste0(
                                                  "background: ", colors$light_teal, "; border: 1px solid #cfe8e4; border-radius: 10px; ",
                                                  "box-shadow: 0 2px 4px rgba(0,0,0,0.06); min-height: 250px; position: relative;"
                                                ),
                                                    div(style = "padding: 20px;",
                                                        div(style = "display: flex; align-items: center; justify-content: center; gap: 14px;",
                                                            div(style = paste0(
                                                              "width: 44px; height: 44px; border-radius: 50%; background: ", colors$teal, "; ",
                                                              "display: inline-flex; align-items: center; justify-content: center; color: white; font-size: 18px;"
                                                            ),
                                                                icon("venus")
                                                            ),
                                                            h3("Gender Gap", class = "home-gap-title", style = "margin-top: 0;")
                                                        ),
                                                        p(
                                                          span("800 million", style = paste0("font-weight: 700; font-size: 1.4em; color: ", colors$teal, ";")),
                                                          " offline people are women.",
                                                          class = "home-gap-text"
                                                        )
                                                    ),
                                                    div(style = paste0(
                                                      "position: absolute; bottom: 0; width: 100%; text-align: center; ",
                                                      "padding: 10px 8px; border-top: 1px solid #cfe8e4; background: white; ",
                                                      "border-radius: 0 0 10px 10px; color: ", colors$teal, ";"
                                                    ),
                                                        span("58% women", class = "home-gap-stat")
                                                    )
                                                )
                                         )
                                       )
                                   ),
                                   div(style = paste0("margin-top: 18px; padding-top: 14px; border-top: 1px solid ", colors$light_grey, ";"),
                                       p(
                                         "The ",
                                         span("coverage gap", style = "font-weight: 700;"),
                                         " is the share of population living outside the reach of broadband networks, typically measured as those without access to mobile broadband networks. The ",
                                         span("usage gap", style = "font-weight: 700;"),
                                         " refers to the share of population who live within broadband coverage, but do not use the internet or online services. Evidence suggests that affordability, limited digital skills, and the lack of relevant content online can all contribute to the usage gap. The ",
                                         span("gender gap", style = "font-weight: 700;"),
                                         " highlights disparities between men and women in terms of their internet use. Even when infrastructure is available and affordable, women in some countries are less likely to own smartphones, make data purchases, or use digital services for economic and social activities. This gap varies across countries, and is driven by many factors, including unequal access to financial resources, lower digital literacy, concerns about safety and privacy, and gendered social norms that may limit women from benefiting from digital technologies.",
                                         style = paste0("color: ", colors$navy, "; font-size: 17px; line-height: 1.6; margin-bottom: 0;")
                                       )
                                   )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(class = "home-section-shell",
                                 h3("Policy Implications",
                                    style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 8px;")),
                                 p(
                                   "Different connectivity gaps require different policy responses. While closing all gaps requires comprehensive action, ",
                                   span("we model the increase in internet users from closing each gap individually", style = "font-weight: 700;"),
                                   " to help identify policy priorities (see methodology",
                                   tags$sup("*"),
                                   "). On average, closing the usage gap would bring the largest gains, though country contexts vary significantly. ",
                                   tags$a(
                                     "Explore country diagnostics",
                                     href = "#",
                                     style = paste0("color: ", colors$blue, "; text-decoration: underline;"),
                                     onclick = "$('.navbar-nav a').filter(function(){return $(this).text().trim()==='Country Diagnostic Prototype';}).tab('show'); return false;"
                                   ),
                                   " to see which gap is most critical in each country, how it compares with regional peers, and the potential policy implications.",
                                   style = paste0("color: ", colors$navy, "; font-size: 18px; margin-bottom: 15px;")
                                 ),
                                 div(class = "home-policy-frame",
                                     h3(
                                       tagList(
                                         "Number of new internet users under the different policy scenarios"
                                       ),
                                       style = paste0("text-align: center; color: ", colors$navy, "; margin-top: 6px; margin-bottom: 0;")
                                     ),
                                     plotlyOutput("home_global_scenarios", height = "390px"),
                                     div(style = "text-align: center; margin-top: 2px; margin-bottom: 2px;",
                                        tags$a(
                                          "Run a Country Diagnostic",
                                          href = "#",
                                          class = "btn btn-warning home-card-button",
                                          style = "padding: 10px 24px;",
                                          onclick = "$('.navbar-nav a').filter(function(){return $(this).text().trim()==='Country Diagnostic Prototype';}).tab('show'); return false;"
                                        )
                                     )
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(class = "home-section-shell",
                                 h3("Explore the Dashboard", style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 15px;")),
                                 fluidRow(
                                   column(4,
                                          div(class = "home-explore-card global-card",
                                              div(style = "text-align: center;",
                                                  icon("globe", style = paste0("font-size: 44px; color: ", colors$blue, ";"))
                                              ),
                                              h3("Global Analysis", style = paste0("text-align: center; color: ", colors$navy, "; margin-top: 12px;")),
                                              div(class = "home-explore-list",
                                                  p(icon("check"), " Gap Distribution by Region and Country", style = paste0("color: ", colors$navy, "; margin-top: 14px;")),
                                                  p(icon("check"), " Ranking by Region and Country", style = paste0("color: ", colors$navy, ";")),
                                                  p(icon("check"), " Regional Statistics", style = paste0("color: ", colors$navy, "; margin-bottom: 0;"))
                                              ),
                                              tags$a(
                                                "Explore Global Data",
                                                href = "#",
                                                class = "btn btn-primary btn-block home-card-button",
                                                onclick = "$('.navbar-nav a').filter(function(){return $(this).text().trim()==='Global Analysis';}).tab('show'); return false;"
                                              )
                                          )
                                   ),
                                   column(4,
                                          div(class = "home-explore-card country-card",
                                              div(style = "text-align: center;",
                                                  icon("chart-bar", style = "font-size: 44px; color: #d2951f;")
                                              ),
                                              h3("Country Analysis", style = paste0("text-align: center; color: ", colors$navy, "; margin-top: 12px;")),
                                              div(class = "home-explore-list",
                                                  p(icon("check"), " Gap Statistics", style = paste0("color: ", colors$navy, "; margin-top: 14px;")),
                                                  p(icon("check"), " Regional Benchmarking", style = paste0("color: ", colors$navy, ";")),
                                                  p(icon("check"), " Select Countries to Compare", style = paste0("color: ", colors$navy, "; margin-bottom: 0;"))
                                              ),
                                              tags$a(
                                                "Explore Country Data",
                                                href = "#",
                                                class = "btn btn-warning btn-block home-card-button",
                                                onclick = "$('.navbar-nav a').filter(function(){return $(this).text().trim()==='Country Analysis';}).tab('show'); setTimeout(function(){ $('a[data-value=\"Overview\"]').tab('show'); }, 100); return false;"
                                              )
                                          )
                                   ),
                                   column(4,
                                          div(class = "home-explore-card policy-card",
                                              div(style = "text-align: center;",
                                                  icon("female", style = "font-size: 44px; color: #7b52c2;")
                                              ),
                                              h3("Policy Scenarios", style = paste0("text-align: center; color: ", colors$navy, "; margin-top: 12px;")),
                                              div(class = "home-explore-list",
                                                  p(icon("check"), " Scenario 1: Expand Infrastructure", style = paste0("color: ", colors$navy, "; margin-top: 14px;")),
                                                  p(icon("check"), " Scenario 2: Boost Adoption", style = paste0("color: ", colors$navy, ";")),
                                                  p(icon("check"), " Scenario 3: Close Gender Gap", style = paste0("color: ", colors$navy, "; margin-bottom: 0;"))
                                              ),
                                              tags$a(
                                                "Run Scenarios",
                                                href = "#",
                                                class = "btn btn-block home-card-button",
                                                style = "background: #7b52c2; color: white;",
                                                onclick = "$('.navbar-nav a').filter(function(){return $(this).text().trim()==='Country Analysis';}).tab('show'); setTimeout(function(){ $('a[data-value=\"Policy Scenarios\"]').tab('show'); }, 100); return false;"
                                              )
                                          )
                                   )
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               p(
                                 tags$sup("*"),
                                 " Methodology note: these scenario estimates are directional and rely on simplified assumptions about coverage expansion, adoption responses, and gender-gap closure. They are intended for policy prioritization in the prototype rather than as precise forecasts.",
                                 style = paste0("color: ", colors$navy, "; font-size: 13px; line-height: 1.5; margin-top: 4px; margin-bottom: 10px;")
                               )
                        )
                      )
             ),
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
             
            tabPanel("Country Diagnostic Prototype",
                     fluidRow(
                       column(12,
                              div(class = "diagnostic-shell diagnostic-top-shell",
                                   fluidRow(
                                     column(4,
                                            selectInput("diagnostic_country", "Select Country",
                                                        choices = NULL, selected = NULL,
                                                        width = "100%")
                                     ),
                                     column(8,
                                            h2("Country Diagnostic Prototype",
                                               style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 6px;")),
                                            p("A narrative that highlights the main digital gaps, regional context, and the gains from closing them.",
                                              class = "diagnostic-heading-copy")
                                     )
                                   )
                              )
                       )
                     ),
                     fluidRow(
                        column(12,
                               div(class = "diagnostic-shell",
                                   uiOutput("diagnostic_headline")
                               )
                        )
                     ),
                     fluidRow(
                        column(12,
                               div(class = "diagnostic-shell",
                                  h3(
                                    tagList(
                                      "1. What Is the Main Gap?",
                                      tags$sup("*")
                                    ),
                                    style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 8px;")
                                  ),
                                  uiOutput("diagnostic_main_gap_summary"),
                                  uiOutput("diagnostic_gap_cards"),
                                  div(style = "height: 10px;"),
                                  uiOutput("diagnostic_significance_text"),
                                  uiOutput("diagnostic_benchmark_plots"),
                                   p(
                                     tags$sup("*"),
                                     " We flag gaps as significant when they are above simple thresholds: coverage gap >= 10%, usage gap >= 20%, and gender gap >= 5 percentage points where women are disadvantaged. For countries where none of these gaps are above the thresholds, the largest gap in magnitude is considered the main gap.",
                                     class = "diagnostic-footnote"
                                   )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(class = "diagnostic-shell",
                                   h3("2. How Many People Could Come Online?", style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 8px;")),
                                   p("We modeled three hypothetical scenarios of possible approaches to closing coverage, usage, and gender gaps to illustrate the potential impact of different policy choices.",
                                     style = paste0("color: ", colors$navy, "; font-size: clamp(17px, 1.2vw, 19px); line-height: 1.5; margin-bottom: 16px;")),
                                   uiOutput("diagnostic_impact_cards"),
                                   div(style = paste0("margin-top: 18px; padding-top: 14px; border-top: 1px solid ", colors$light_grey, ";"),
                                       p(
                                         tags$strong("In Scenario 1"),
                                         ", mobile coverage is extended to all unserved areas while current usage patterns remain unchanged. This means that men's and women's internet use in newly covered areas continues to reflect current imbalances, if any.",
                                         style = paste0("color: ", colors$navy, "; font-size: 16px; line-height: 1.6; margin-bottom: 12px;")
                                       ),
                                       p(
                                         tags$strong("Scenario 2"),
                                         " models fully closing the usage gap, while leaving all other parameters constant. This scenario assumes universal adoption among all individuals with network access, while maintaining the existing female-to-male internet use ratio. Men's usage increases to match coverage levels (up to 100%), and women's usage rises proportionally based on the current gender gap. This represents an optimistic ceiling where infrastructure barriers are eliminated and all demand-side barriers except gender-specific ones are addressed, showing the maximum potential reach under current gender dynamics.",
                                         style = paste0("color: ", colors$navy, "; font-size: 16px; line-height: 1.6; margin-bottom: 12px;")
                                       ),
                                       p(
                                         tags$strong("Scenario 3"),
                                         " models the effects of closing gender gaps when men's use exceeds women's. This means bringing women's internet usage up to men's current levels, without any infrastructure expansion or increases in male use. While hypothetical, this scenario models potential gains from gender-focused interventions alone, revealing how many additional women would come online if they adopted the Internet at the same rate as men.",
                                         style = paste0("color: ", colors$navy, "; font-size: 16px; line-height: 1.6; margin-bottom: 0;")
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
    updateSelectInput(session, "diagnostic_country",
                      choices = countries,
                      selected = countries[1])
    updateSelectizeInput(session, "comparison_countries",
                         choices = countries)
  })
  
  # Home: global scenario chart (values provided for prototype)
  output$home_global_scenarios <- renderPlotly({
    scenario_levels <- c("s1", "s2", "s3")
    scenario_labels <- c(
      "Expand Coverage to All",
      "Maximize Adoption Rate<br>(without addressing gender disparity in adoption)",
      "Increase Adoption Rate of Women<br>to Current Level of Men's"
    )
    
    scenario_data <- tibble(
      scenario = factor(
        c("s1", "s1", "s2", "s2", "s3"),
        levels = scenario_levels
      ),
      group = c("Men", "Women", "Men", "Women", "Women"),
      value = c(53, 35, 519, 350, 208)
    )
    
    plot_ly(
      scenario_data,
      x = ~scenario,
      y = ~value,
      color = ~group,
      colors = c("Men" = colors$blue, "Women" = "#5a9cab"),
      type = "bar",
      text = ~value,
      textposition = "outside",
      textfont = list(size = 14, color = colors$navy),
      hoverinfo = "skip"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(
          title = "",
          tickvals = scenario_levels,
          ticktext = scenario_labels,
          tickfont = list(size = 13),
          automargin = TRUE
        ),
        yaxis = list(
          title = "",
          range = c(0, 620),
          ticksuffix = "M",
          dtick = 100,
          titlefont = list(size = 16),
          tickfont = list(size = 12),
          automargin = TRUE
        ),
        legend = list(orientation = "h", y = 1.08, x = 0.5, xanchor = "center", font = list(size = 12)),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        font = list(color = colors$navy),
        margin = list(l = 90, r = 30, t = 30, b = 90),
        hovermode = FALSE
      )
  })
  
  # Reactive: filtered data
  country_data <- reactive({
    req(input$country)
    adoption_data %>% filter(country_name == input$country)
  })
  
  # Prototype: selected country for the narrative diagnostic view
  diagnostic_country_data <- reactive({
    req(input$diagnostic_country)
    adoption_data %>% filter(country_name == input$diagnostic_country)
  })
  
  diagnostic_gap_table <- reactive({
    data <- diagnostic_country_data()
    req(nrow(data) > 0)
    
    region_name <- data$regionwb24_hi[1]
    region_data <- adoption_data %>% filter(regionwb24_hi == region_name)
    country_usage_gap <- sanitize_diagnostic_usage_gap_pct(data$country_name[1], data$internet_usage_gap_all_pct[1])
    region_usage_gap <- sanitize_diagnostic_usage_gap_pct(region_data$country_name, region_data$internet_usage_gap_all_pct)
    
    gender_country_signed <- data$internet_usage_male_pct[1] - data$internet_usage_female_pct[1]
    gender_region_signed <- region_data$internet_usage_male_pct - region_data$internet_usage_female_pct
    gender_country_gap <- max(gender_country_signed, 0)
    gender_region_gap <- pmax(gender_region_signed, 0)
    women_disadvantaged <- gender_country_signed > 0
    reverse_gender_gap <- gender_country_signed < 0
    significance_thresholds <- c(coverage = 10, usage = 20, gender = 5)
    
    gaps <- tibble(
      gap = c("Coverage Gap", "Usage Gap", "Gender Gap"),
      gap_key = c("coverage", "usage", "gender"),
      value = c(data$gap_dominant_pct[1], country_usage_gap, gender_country_gap),
      unit = c("% of adults", "% of adults", "pp"),
      affected_m = c(
        data$adults_no_dominant_millions[1],
        data$internet_usage_gap_all_millions[1],
        ifelse(data$internet_usage_gap_all_millions[1] > 0,
               100 * data$internet_usage_gap_female_millions[1] / data$internet_usage_gap_all_millions[1],
               NA_real_)
      ),
      affected_text = c(
        "outside broadband reach",
        "offline despite coverage",
        "of the offline are women"
      ),
      region_avg = c(
        mean(region_data$gap_dominant_pct, na.rm = TRUE),
        mean(region_usage_gap, na.rm = TRUE),
        mean(gender_region_gap, na.rm = TRUE)
      ),
      direction_text = c(
        "",
        "",
        ifelse(
          women_disadvantaged,
          "Men use the internet more than women",
          ifelse(
            reverse_gender_gap,
            "Women use the internet more than men",
            "No gender gap issue under this definition"
          )
        )
      ),
      headline_text = c(
        paste0(round(data$gap_dominant_pct[1], 1), "%"),
        paste0(round(country_usage_gap, 1), "%"),
        ifelse(
          women_disadvantaged,
          paste0(round(gender_country_gap, 1), "pp"),
          ifelse(
            reverse_gender_gap,
            paste0("Reversed Gender Gap: ", abs(round(gender_country_signed, 1)), "pp"),
            "No gap"
          )
        )
      ),
      status_label = c(
        ifelse(data$gap_dominant_pct[1] >= significance_thresholds["coverage"], "Significant", "Watch"),
        ifelse(country_usage_gap >= significance_thresholds["usage"], "Significant", "Watch"),
        ifelse(
          women_disadvantaged && gender_country_gap >= significance_thresholds["gender"],
          "Significant",
          "N/A"
        )
      ),
      signed_value = c(data$gap_dominant_pct[1], country_usage_gap, gender_country_gap),
      threshold = c(significance_thresholds["coverage"], significance_thresholds["usage"], significance_thresholds["gender"]),
      accent = c(colors$yellow, colors$teal, colors$blue),
      background = c(colors$light_yellow, colors$light_teal, colors$light_blue)
    ) %>%
      mutate(
        diff_vs_region = value - region_avg,
        significant = case_when(
          gap_key == "coverage" ~ value >= threshold,
          gap_key == "usage" ~ value >= threshold,
          gap_key == "gender" ~ women_disadvantaged & value >= threshold,
          TRUE ~ FALSE
        )
      )

    gaps
  })
  
  compute_scenarios <- function(data) {
    internet_male <- data$internet_usage_male_pct[1] / 100
    internet_female <- data$internet_usage_female_pct[1] / 100
    coverage_pct <- data$coverage_dominant_pct[1] / 100
    gap_pct <- data$gap_dominant_pct[1] / 100
    
    s1_male_usage <- min(internet_male + (gap_pct * internet_male / coverage_pct), 1)
    s1_female_usage <- min(internet_female + (gap_pct * internet_female / coverage_pct), 1)
    s1_men_gain <- (s1_male_usage - internet_male) * data$adult_pop_male[1] / 1e6
    s1_women_gain <- (s1_female_usage - internet_female) * data$adult_pop_female[1] / 1e6
    
    gender_ratio <- internet_female / internet_male
    s2_men_final <- min(coverage_pct, 1)
    s2_women_final <- min(s2_men_final * gender_ratio, 1)
    s2_men_gain <- (s2_men_final - internet_male) * data$adult_pop_male[1] / 1e6
    s2_women_gain <- (s2_women_final - internet_female) * data$adult_pop_female[1] / 1e6
    
    if(s2_men_final >= s2_women_final) {
      s3_gender_gain <- (s2_men_final - s2_women_final) * data$adult_pop_female[1] / 1e6
      s3_target_group <- "women"
    } else {
      s3_gender_gain <- 0
      s3_target_group <- "women"
    }
    
    list(
      coverage_gain = s1_men_gain + s1_women_gain,
      usage_gain = s2_men_gain + s2_women_gain,
      gender_gain = s3_gender_gain,
      gender_target_group = s3_target_group,
      s1_total_users_m = data$internet_usage_all_millions[1] + s1_men_gain + s1_women_gain,
      s1_total_rate = ((data$internet_usage_all_millions[1] + s1_men_gain + s1_women_gain) * 1e6 / data$adult_population[1]) * 100,
      s2_total_users_m = data$internet_usage_all_millions[1] + s2_men_gain + s2_women_gain,
      s2_total_rate = ((data$internet_usage_all_millions[1] + s2_men_gain + s2_women_gain) * 1e6 / data$adult_population[1]) * 100,
      s3_women_users_m = data$internet_usage_female_millions[1] + s3_gender_gain,
      s3_women_rate = ((data$internet_usage_female_millions[1] + s3_gender_gain) * 1e6 / data$adult_pop_female[1]) * 100
    )
  }
  
  diagnostic_impact_table <- reactive({
    data <- diagnostic_country_data()
    req(nrow(data) > 0)
    scenario_results <- compute_scenarios(data)
    
    tibble(
      gap_key = c("coverage", "usage", "gender"),
      title = c("If Coverage Gaps Were Closed", "If Usage Gaps Were Closed", "If Gender Gaps Were Closed"),
      impact_m = c(
        scenario_results$coverage_gain,
        scenario_results$usage_gain,
        scenario_results$gender_gain
      ),
      metric_1_label = c(
        "Total Internet Users",
        "Total Internet Users",
        "Women Internet Users"
      ),
      metric_1_before = c(
        data$internet_usage_all_millions[1],
        data$internet_usage_all_millions[1],
        data$internet_usage_female_millions[1]
      ),
      metric_1_after = c(
        scenario_results$s1_total_users_m,
        scenario_results$s2_total_users_m,
        scenario_results$s3_women_users_m
      ),
      metric_1_before_label = c(
        format_pop_detail_millions(data$internet_usage_all_millions[1]),
        format_pop_detail_millions(data$internet_usage_all_millions[1]),
        format_pop_detail_millions(data$internet_usage_female_millions[1])
      ),
      metric_1_after_label = c(
        format_pop_detail_millions(scenario_results$s1_total_users_m),
        format_pop_detail_millions(scenario_results$s2_total_users_m),
        format_pop_detail_millions(scenario_results$s3_women_users_m)
      ),
      metric_2_label = c(
        "Internet Usage Rate",
        "Internet Usage Rate",
        "Women's Usage Rate"
      ),
      metric_2_before = c(
        data$internet_usage_all_pct[1],
        data$internet_usage_all_pct[1],
        data$internet_usage_female_pct[1]
      ),
      metric_2_after = c(
        scenario_results$s1_total_rate,
        scenario_results$s2_total_rate,
        scenario_results$s3_women_rate
      ),
      metric_2_before_label = c(
        paste0(round(data$internet_usage_all_pct[1], 1), "%"),
        paste0(round(data$internet_usage_all_pct[1], 1), "%"),
        paste0(round(data$internet_usage_female_pct[1], 1), "%")
      ),
      metric_2_after_label = c(
        paste0(round(scenario_results$s1_total_rate, 1), "%"),
        paste0(round(scenario_results$s2_total_rate, 1), "%"),
        paste0(round(scenario_results$s3_women_rate, 1), "%")
      ),
      accent = c(colors$yellow, colors$teal, colors$blue),
      background = c(colors$light_yellow, colors$light_teal, colors$light_blue)
    ) %>%
      mutate(
        metric_1_show = !is.na(metric_1_before) & !is.na(metric_1_after),
        metric_2_show = !is.na(metric_2_before) & !is.na(metric_2_after)
      )
  })
  diagnostic_benchmark_gaps <- reactive({
    gaps <- diagnostic_gap_table()
    significant_gaps <- gaps %>% filter(significant)
    
    if(nrow(significant_gaps) == 0) {
      gaps %>% arrange(desc(value)) %>% slice(1)
    } else {
      significant_gaps
    }
  })
  
  output$diagnostic_headline <- renderUI({
    data <- diagnostic_country_data()
    gaps <- diagnostic_gap_table()
    region_data <- adoption_data %>%
      filter(regionwb24_hi == data$regionwb24_hi[1]) %>%
      arrange(desc(internet_usage_all_pct)) %>%
      mutate(rank = row_number())
    lmic_data <- adoption_data %>%
      filter(!is.na(internet_usage_all_pct)) %>%
      arrange(desc(internet_usage_all_pct)) %>%
      mutate(rank = row_number())
    
    main_gap <- gaps %>% arrange(desc(value)) %>% slice(1)
    region_label <- gsub("\\s*\\(.*", "", data$regionwb24_hi[1])
    region_rank <- region_data %>%
      filter(country_name == data$country_name[1]) %>%
      pull(rank)
    lmic_rank <- lmic_data %>%
      filter(country_name == data$country_name[1]) %>%
      pull(rank)
    lmic_total <- nrow(lmic_data)
    non_main_gaps <- gaps %>% filter(gap != main_gap$gap)
    gap_status_text <- paste(
      vapply(seq_len(nrow(non_main_gaps)), function(i) {
        gap_row <- non_main_gaps[i, ]
        status_text <- if(gap_row$gap_key == "gender" && grepl("^Reversed Gender Gap", gap_row$headline_text)) {
          "reversed"
        } else if(gap_row$gap_key == "gender" && gap_row$status_label == "N/A") {
          "not a current issue"
        } else if(gap_row$significant) {
          "significant"
        } else {
          "insignificant"
        }
        paste0(gap_row$gap, " is ", status_text)
      }, character(1)),
      collapse = " and "
    )
    div(style = "margin-bottom: 0;",
        h2(data$country_name[1], style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 8px;")),
        tags$p(
          "Among ",
          data$country_name[1],
          "'s adult population of ",
          format_pop_millions(data$adult_population[1] / 1e6),
          ", ",
          tags$strong(paste0(round(data$internet_usage_all_pct[1], 1), "%")),
          " (",
          format_pop_millions(data$internet_usage_all_millions[1]),
          ") are internet users. This internet usage rate ",
          tags$strong(
            paste0(
              "ranks ",
              ordinal(region_rank),
              " in the ",
              region_label,
              " region"
            )
          ),
          " and ",
          tags$strong(
            paste0(
              ordinal(lmic_rank),
              " among ",
              lmic_total,
              " LMICs"
            )
          ),
          " with available data",
          ".",
          style = paste0("color: ", colors$navy, "; font-size: clamp(18px, 1.35vw, 20px); line-height: 1.5; margin-bottom: 0;")
        )
    )
  })

  output$diagnostic_main_gap_summary <- renderUI({
    data <- diagnostic_country_data()
    gaps <- diagnostic_gap_table()
    main_gap <- gaps %>% arrange(desc(value)) %>% slice(1)
    significant_gaps <- gaps %>% filter(significant) %>% pull(gap)
    other_significant_gaps <- setdiff(significant_gaps, main_gap$gap)
    gender_reversed <- gaps %>% filter(gap_key == "gender") %>% pull(headline_text) %>% grepl("^Reversed Gender Gap", .)

    summary_text <- if(length(other_significant_gaps) > 0) {
      other_text <- if(length(other_significant_gaps) == 1) {
        other_significant_gaps
      } else {
        paste(paste(other_significant_gaps[-length(other_significant_gaps)], collapse = ", "), "and", other_significant_gaps[length(other_significant_gaps)])
      }
      paste0(main_gap$gap, " appears to be the most pronounced constraint in ", data$country_name[1], " based on the current country profile, and ", other_text, " is also significant.")
    } else if(gender_reversed) {
      other_gap <- gaps %>% filter(gap != main_gap$gap, gap_key != "gender") %>% slice(1)
      other_text <- if(other_gap$significant) "significant" else "insignificant"
      paste0(main_gap$gap, " appears to be the most pronounced constraint in ", data$country_name[1], " based on the current country profile, while ", other_gap$gap, " is ", other_text, " and Gender Gap is reversed.")
    } else {
      non_main_gaps <- gaps %>% filter(gap != main_gap$gap)
      gap_status_text <- paste(
        vapply(seq_len(nrow(non_main_gaps)), function(i) {
          gap_row <- non_main_gaps[i, ]
          status_text <- if(gap_row$gap_key == "gender" && gap_row$status_label == "N/A") {
            "not a current issue"
          } else if(gap_row$significant) {
            "significant"
          } else {
            "insignificant"
          }
          paste0(gap_row$gap, " is ", status_text)
        }, character(1)),
        collapse = " and "
      )
      paste0(main_gap$gap, " appears to be the most pronounced constraint in ", data$country_name[1], " based on the current country profile, while ", gap_status_text, ".")
    }
    
    p(
      summary_text,
      style = paste0("color: ", colors$navy, "; font-size: clamp(17px, 1.2vw, 19px); line-height: 1.5; margin-bottom: 16px;")
    )
  })
  
  output$diagnostic_gap_cards <- renderUI({
    gaps <- diagnostic_gap_table()
    
    fluidRow(
      lapply(seq_len(nrow(gaps)), function(i) {
        gap_row <- gaps[i, ]
        icon_name <- switch(
          gap_row$gap_key,
          "coverage" = "broadcast-tower",
          "usage" = "mobile-alt",
          "gender" = "venus",
          "circle"
        )
        
        badge_style <- if(gap_row$status_label == "Significant") {
          paste0("background: ", gap_row$accent, "; color: white;")
        } else if(gap_row$status_label == "N/A") {
          paste0("background: white; color: ", colors$navy, "; border: 1px solid ", colors$grey, ";")
        } else {
          paste0("background: white; color: ", colors$grey, "; border: 1px solid ", colors$grey, ";")
        }
        headline_style <- if(grepl("^Reversed Gender Gap", gap_row$headline_text)) {
          paste0("font-size: clamp(22px, 1.8vw, 29px); font-weight: 700; color: ", colors$navy, "; line-height: 1.08; margin-bottom: 10px;")
        } else {
          paste0("font-size: clamp(27px, 2.2vw, 34px); font-weight: 700; color: ", colors$navy, "; line-height: 1.05; margin-bottom: 10px;")
        }
        
        column(4,
               div(class = "diagnostic-gap-card",
                   style = paste0("background: ", gap_row$background, "; border: 1px solid ", gap_row$accent, "33;"),
                   div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 14px;",
                       div(style = "display: flex; align-items: center; gap: 12px;",
                           div(style = paste0(
                             "width: 42px; height: 42px; border-radius: 50%; background: ", gap_row$accent, "; ",
                             "display: inline-flex; align-items: center; justify-content: center; color: white; font-size: 16px;"
                           ),
                               icon(icon_name)
                           ),
                           h4(gap_row$gap, style = paste0("color: ", colors$navy, "; margin: 0;"))
                       ),
                       span(gap_row$status_label, class = "diagnostic-pill", style = badge_style)
                   ),
                  div(style = headline_style,
                      gap_row$headline_text
                  ),
                   p(
                     if(gap_row$gap_key == "gender") {
                       paste0(round(gap_row$affected_m, 0), "% ", gap_row$affected_text)
                     } else {
                       paste0(format_pop_millions(gap_row$affected_m), " ", gap_row$affected_text)
                     },
                     style = paste0("color: ", colors$navy, "; font-size: 18px; margin-bottom: 8px;")
                   ),
                   if(nzchar(gap_row$direction_text)) {
                     p(
                       gap_row$direction_text,
                       style = paste0("color: ", colors$navy, "; font-size: 15px; margin-bottom: 8px;")
                     )
                   },
                   {
                     comparison_text <- if(gap_row$gap_key == "gender" && grepl("^Reversed Gender Gap", gap_row$headline_text)) {
                       ""
                     } else if(gap_row$gap_key == "gender" && gap_row$status_label == "N/A") {
                       ""
                     } else if(gap_row$diff_vs_region >= 0) {
                       if(gap_row$gap_key == "gender") {
                         paste0(round(gap_row$diff_vs_region, 1), "pp above the regional average")
                       } else {
                         paste0(round(gap_row$diff_vs_region, 1), " percentage points above the regional average")
                       }
                     } else {
                       if(gap_row$gap_key == "gender") {
                         paste0(abs(round(gap_row$diff_vs_region, 1)), "pp below the regional average")
                       } else {
                         paste0(abs(round(gap_row$diff_vs_region, 1)), " percentage points below the regional average")
                       }
                     }
                     
                     if(nzchar(comparison_text)) {
                       p(
                         comparison_text,
                         style = paste0("color: ", colors$navy, "; font-size: 15px; margin-bottom: 0;")
                       )
                     }
                   }
               )
        )
      })
    )
  })
  
  output$diagnostic_significance_text <- renderUI({
    div(
      h4("Regional Benchmarking of Significant Gaps", style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 12px;"))
    )
  })

  output$diagnostic_benchmark_plots <- renderUI({
    data <- diagnostic_country_data()
    benchmark_gaps <- diagnostic_benchmark_gaps()
    region_n <- adoption_data %>%
      filter(regionwb24_hi == data$regionwb24_hi[1]) %>%
      nrow()
    plot_height <- max(360, min(920, 190 + (region_n + 1) * 30))
    
    tagList(
      lapply(seq_len(nrow(benchmark_gaps)), function(i) {
        plot_id <- paste0("diagnostic_benchmark_plot_", benchmark_gaps$gap_key[i])
        div(
          style = if(i < nrow(benchmark_gaps)) "margin-bottom: 20px;" else "",
          div(
            style = "overflow-x: auto; overflow-y: hidden; display: flex; justify-content: center;",
            div(
              style = "width: 100%; min-width: 980px; max-width: 1400px;",
              plotOutput(plot_id, height = paste0(plot_height, "px"), width = "100%")
            )
          )
        )
      })
    )
  })

  observe({
    data <- diagnostic_country_data()
    benchmark_gaps <- diagnostic_benchmark_gaps()
    region_data <- adoption_data %>% filter(regionwb24_hi == data$regionwb24_hi[1])
    
    lapply(seq_len(nrow(benchmark_gaps)), function(i) {
      gap_row <- benchmark_gaps[i, ]
      plot_id <- paste0("diagnostic_benchmark_plot_", gap_row$gap_key)
      
      peer_data <- switch(
        gap_row$gap_key,
        "coverage" = region_data %>%
          transmute(label = country_name, value = gap_dominant_pct),
        "usage" = region_data %>%
          transmute(label = country_name, value = sanitize_diagnostic_usage_gap_pct(country_name, internet_usage_gap_all_pct)),
        "gender" = region_data %>%
          transmute(label = country_name, value = internet_usage_male_pct - internet_usage_female_pct)
      ) %>%
        filter(!is.na(label), nzchar(trimws(label)), !is.na(value)) %>%
        mutate(series = ifelse(label == data$country_name[1], "Selected Country", "Peer Country"))
      
      benchmark_data <- bind_rows(
        peer_data,
        tibble(
          label = "Regional Average",
          value = mean(peer_data$value, na.rm = TRUE),
          series = "Regional Average"
        )
      ) %>%
        arrange(desc(value)) %>%
        mutate(
          label_factor = factor(label, levels = rev(label)),
          text_label = if(gap_row$gap_key == "gender") {
            paste0(round(value, 1), "pp")
          } else {
            paste0(round(value, 1), "%")
          }
        )
      x_min <- if(gap_row$gap_key == "gender") min(0, min(benchmark_data$value, na.rm = TRUE)) else 0
      x_max <- max(benchmark_data$value, na.rm = TRUE)
      x_pad <- max((x_max - x_min) * 0.03, 0.8)
      neg_label_min <- if(any(benchmark_data$value < 0, na.rm = TRUE)) {
        min(benchmark_data$value[benchmark_data$value < 0], na.rm = TRUE) - x_pad * 1.1
      } else {
        x_min
      }
      plot_x_min <- min(x_min, neg_label_min)
      x_breaks <- pretty(c(0, x_max), n = 5)
      x_breaks <- x_breaks[x_breaks >= 0]
      
      output[[plot_id]] <- renderPlot({
        ggplot(benchmark_data, aes(x = value, y = label_factor, fill = series)) +
          geom_col(width = 0.74) +
          geom_text(
            data = benchmark_data %>% filter(value >= 0),
            aes(x = value + x_pad, label = text_label),
            hjust = 0,
            size = 4.8,
            color = colors$navy
          ) +
          geom_text(
            data = benchmark_data %>% filter(value < 0),
            aes(x = value - x_pad * 0.35, label = text_label),
            hjust = 1,
            size = 4.8,
            color = colors$navy
          ) +
          scale_fill_manual(
            values = c("Peer Country" = colors$grey, "Regional Average" = colors$teal, "Selected Country" = colors$blue),
            breaks = c("Peer Country", "Regional Average", "Selected Country")
          ) +
          scale_x_continuous(
            expand = c(0, 0),
            breaks = x_breaks,
            name = if(gap_row$gap_key == "gender") "Gender Gap (pp)" else "Gap Size (%)"
          ) +
          scale_y_discrete(expand = expansion(mult = c(0.06, 0.08))) +
          labs(
            title = paste0(gap_row$gap, " Across ", gsub("\\s*\\(.*", "", data$regionwb24_hi[1])),
            y = NULL,
            fill = NULL
          ) +
          coord_cartesian(xlim = c(plot_x_min, x_max + x_pad * 3), clip = "off") +
          theme_minimal(base_size = 13) +
          theme(
            plot.title = element_text(size = 16, color = colors$navy, hjust = 0.5, margin = margin(b = 12)),
            legend.position = "top",
            legend.justification = "center",
            legend.text = element_text(size = 11, color = colors$navy),
            legend.margin = margin(t = 0, b = 2),
            legend.box.margin = margin(t = 0, b = 4),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "#d9dfe4", linewidth = 0.4),
            axis.text.y = element_text(color = colors$navy, size = 11),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(color = colors$navy, size = 11),
            axis.title.x = element_text(color = colors$navy, size = 12, margin = margin(t = 12)),
            axis.title.y = element_blank(),
            plot.margin = margin(t = 10, r = 28, b = 10, l = 20)
          )
      })
    })
  })
  
  output$diagnostic_impact_cards <- renderUI({
    impacts <- diagnostic_impact_table()
    card_width <- 4
    user_scale_max <- max(c(impacts$metric_1_before[impacts$metric_1_show], impacts$metric_1_after[impacts$metric_1_show]), na.rm = TRUE)
    if(!is.finite(user_scale_max) || user_scale_max <= 0) user_scale_max <- 1
    
    mini_metric_bar <- function(label, before, after, before_label, after_label, accent, scale_max) {
      before_width <- max(0, min(100, 100 * before / scale_max))
      after_width <- max(0, min(100, 100 * after / scale_max))
      gain_width <- max(0, after_width - before_width)
      divider_left <- max(0, min(100, before_width))
      div(
        style = "margin-top: 14px;",
        div(
          style = paste0("display: flex; justify-content: space-between; align-items: baseline; gap: 12px; margin-bottom: 6px; color: ", colors$navy, ";"),
          span(label, style = "font-size: 15px; font-weight: 600; line-height: 1.3;"),
          span(HTML(paste0(before_label, " &rarr; ", after_label)), style = "font-size: 14px; white-space: nowrap; text-align: right;")
        ),
        div(
          style = paste0("position: relative; height: 14px; border-radius: 999px; background: rgba(255,255,255,0.72); overflow: hidden; box-shadow: inset 0 0 0 1px ", accent, "22;"),
          div(style = paste0("position: absolute; inset: 0 auto 0 0; width: ", sprintf("%.1f", after_width), "%; background: ", accent, "; opacity: 0.18;")),
          if(gain_width > 0.2) {
            div(style = paste0("position: absolute; inset: 1px auto 1px ", sprintf("%.1f", before_width), "%; width: ", sprintf("%.1f", gain_width), "%; background: ", accent, "; opacity: 0.32;"))
          },
          div(style = paste0("position: absolute; inset: 2px auto 2px 2px; width: max(0px, calc(", sprintf("%.1f", before_width), "% - 4px)); border-radius: 999px; background: ", accent, "; opacity: 0.9;")),
          if(gain_width > 0.8) {
            div(style = paste0("position: absolute; top: 1px; bottom: 1px; left: calc(", sprintf("%.1f", divider_left), "% - 1px); width: 2px; background: rgba(255,255,255,0.92); border-radius: 999px;"))
          }
        )
      )
    }
    
    fluidRow(
      lapply(seq_len(nrow(impacts)), function(i) {
        impact_row <- impacts[i, ]
        column(card_width,
          div(
            class = "diagnostic-gap-card",
            style = paste0("background: ", impact_row$background, "; border: 1px solid ", impact_row$accent, "33; min-height: 265px;"),
            h4(impact_row$title, style = paste0("color: ", colors$navy, "; margin-top: 0; margin-bottom: 12px; font-size: clamp(16px, 1.25vw, 18px); line-height: 1.25;")),
            div(
              style = paste0("font-size: ", if(impact_row$gap_key == "gender") "clamp(24px, 2.1vw, 32px)" else "clamp(28px, 2.3vw, 36px)", "; font-weight: 700; color: ", impact_row$accent, "; line-height: 1.08; margin-bottom: 14px; white-space: nowrap; letter-spacing: -0.02em;"),
              if(!is.na(impact_row$impact_m) && impact_row$impact_m > 0) {
                if(impact_row$gap_key == "gender") {
                  HTML(paste0(format_pop_millions(impact_row$impact_m), "&nbsp;women&nbsp;come&nbsp;online"))
                } else {
                  HTML(paste0(format_pop_millions(impact_row$impact_m), "&nbsp;come&nbsp;online"))
                }
              } else {
                "No Gap to Close"
              }
            ),
            if(isTRUE(impact_row$metric_1_show)) {
              mini_metric_bar(
                label = impact_row$metric_1_label,
                before = impact_row$metric_1_before,
                after = impact_row$metric_1_after,
                before_label = impact_row$metric_1_before_label,
                after_label = impact_row$metric_1_after_label,
                accent = impact_row$accent,
                scale_max = user_scale_max
              )
            },
            if(isTRUE(impact_row$metric_2_show)) {
              mini_metric_bar(
                label = impact_row$metric_2_label,
                before = impact_row$metric_2_before,
                after = impact_row$metric_2_after,
                before_label = impact_row$metric_2_before_label,
                after_label = impact_row$metric_2_after_label,
                accent = impact_row$accent,
                scale_max = 100
              )
            }
          )
        )
      })
    )
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


















