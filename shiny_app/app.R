library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(plotly)
library(DT)

# ── Load model and data ────────────────────────────────────
rf_model <- readRDS("rf_model.rds")
tv_data  <- read_csv("tv_shows_clean.csv")

# ── Reference table ────────────────────────────────────────
ref_shows <- tv_data |>
  select(title, imdb_score, imdb_votes, runtime, seasons, outcome) |>
  arrange(desc(imdb_score)) |>
  head(30)

# ── Feature importance ─────────────────────────────────────
imp_df <- as.data.frame(importance(rf_model)) |>
  rownames_to_column("Feature") |>
  arrange(MeanDecreaseGini) |>
  mutate(Feature = case_when(
    Feature == "imdb_votes"   ~ "IMDb Votes",
    Feature == "runtime"      ~ "Episode Runtime",
    Feature == "imdb_score"   ~ "IMDb Score",
    Feature == "release_year" ~ "Release Year",
    TRUE ~ Feature
  ))

# ══════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span("ShowSense", style = "color:#e8a020; font-weight:bold;")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Renewal Predictor", tabName = "predict",
               icon = icon("film")),
      menuItem("Shows Comparison", tabName = "compare",
               icon = icon("table"))
    ),
    hr(),
    p("WQD7004 — Programming for Data Science",
      style = "color:#888; font-size:11px; padding:0 15px;"),
    p("MD Raffaul Islam | S2104232",
      style = "color:#666; font-size:11px; padding:0 15px;")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body { background-color: #0d0d1a; }
      .skin-black .main-header .logo {
        background-color: #1a1a2e;
        font-size: 18px;
      }
      .skin-black .main-header .navbar { background-color: #1a1a2e; }
      .skin-black .main-sidebar { background-color: #12121f; }
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #2a2a4e;
        border-left: 3px solid #e8a020;
      }
      .content-wrapper, .right-side { background-color: #0d0d1a; }
      .box {
        background-color: #1a1a2e !important;
        border-top: 3px solid #534ab7;
        color: #fff;
      }
      .box-header { color: #fff !important; background: #1a1a2e; }
      .box-title { color: #e8a020 !important; font-size:15px; }
      label { color: #ccc !important; }
      .irs-single, .irs-from, .irs-to { background: #534ab7; }
      .irs-bar { background: #534ab7; border-top: 1px solid #534ab7;
                 border-bottom: 1px solid #534ab7; }
      .irs-bar-edge { background: #534ab7; border: 1px solid #534ab7; }
      .renewal-badge {
        font-size: 26px; font-weight: bold;
        padding: 18px; border-radius: 8px;
        text-align: center; margin: 8px 0;
      }
      .badge-renewed   { background:#0a3d2e; color:#4ade80; 
                         border: 1px solid #1d9e75; }
      .badge-cancelled { background:#3d0a0a; color:#f87171;
                         border: 1px solid #c0392b; }
      .badge-pending   { background:#2a2a3e; color:#aaa;
                         border: 1px solid #444; }
      .prob-wrap { background:#2a2a3e; border-radius:6px;
                   height:22px; width:100%; margin:6px 0; }
      .prob-fill { height:22px; border-radius:6px; }
      .stat-mini { background:#12121f; border-radius:8px;
                   padding:12px; text-align:center; margin:4px; }
      .stat-mini .num { font-size:22px; font-weight:bold;
                        color:#e8a020; }
      .stat-mini .lbl { font-size:11px; color:#888; margin-top:4px; }
      .small-note { font-size:11px; color:#666; margin-top:6px; }
      table.dataTable { background:#1a1a2e !important; color:#ccc; }
      table.dataTable thead th { background:#12121f; color:#e8a020; }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color:#ccc !important; }
      .dataTables_wrapper { color:#ccc; }
    "))),
    
    tabItems(
      
      # ── TAB 1: Predictor ──────────────────────────────────
      tabItem(tabName = "predict",
              fluidRow(
                # Input panel
                box(title = "Enter Show Details", width = 4,
                    sliderInput("imdb_score", "IMDb Score (1-10)",
                                min=1, max=10, value=7.5, step=0.1),
                    sliderInput("imdb_votes", "IMDb Votes",
                                min=100, max=300000, value=50000, step=500),
                    sliderInput("runtime", "Episode Runtime (mins)",
                                min=10, max=120, value=45, step=5),
                    sliderInput("release_year", "Release Year",
                                min=2000, max=2024, value=2020, step=1),
                    actionButton("predict_btn", "Run Prediction",
                                 icon = icon("play"),
                                 style = "background:#534ab7; color:#fff;
                                  border:none; width:100%;
                                  padding:10px; margin-top:10px;
                                  border-radius:6px; font-size:14px;")
                ),
                
                # Result panel
                box(title = "Prediction Result", width = 4,
                    uiOutput("result_badge"),
                    br(),
                    p("Renewal probability:", 
                      style="color:#aaa; font-size:12px; margin-bottom:4px;"),
                    uiOutput("prob_bar"),
                    uiOutput("prob_pct"),
                    br(),
                    p("Risk assessment:", 
                      style="color:#aaa; font-size:12px; margin-bottom:4px;"),
                    uiOutput("risk_text"),
                    br(),
                    fluidRow(
                      column(6, div(class="stat-mini",
                                    div(class="num", textOutput("stat_score")),
                                    div(class="lbl", "IMDb Score"))),
                      column(6, div(class="stat-mini",
                                    div(class="num", textOutput("stat_votes")),
                                    div(class="lbl", "IMDb Votes")))
                    )
                ),
                
                # Feature importance
                box(title = "Feature Importance (Random Forest)",
                    width = 4,
                    plotlyOutput("importance_plot", height = "300px"),
                    p("Based on MeanDecreaseGini from trained RF model",
                      class = "small-note")
                )
              ),
              
              # Model info row
              fluidRow(
                box(title = "Model Information", width = 12,
                    fluidRow(
                      column(3, div(class="stat-mini",
                                    div(class="num", "70.6%"),
                                    div(class="lbl", "Model accuracy"))),
                      column(3, div(class="stat-mini",
                                    div(class="num", "1,414"),
                                    div(class="lbl", "Shows trained on"))),
                      column(3, div(class="stat-mini",
                                    div(class="num", "100"),
                                    div(class="lbl", "Decision trees"))),
                      column(3, div(class="stat-mini",
                                    div(class="num", "80/20"),
                                    div(class="lbl", "Train/test split")))
                    )
                )
              )
      ),
      
      # ── TAB 2: Comparison Table ───────────────────────────
      tabItem(tabName = "compare",
              fluidRow(
                box(title = "Top 30 Shows — Renewal Outcomes",
                    width = 12,
                    DTOutput("shows_table")
                )
              ),
              fluidRow(
                box(title = "IMDb Score Distribution by Outcome",
                    width = 6,
                    plotlyOutput("score_dist", height = "280px")),
                box(title = "IMDb Votes Distribution by Outcome",
                    width = 6,
                    plotlyOutput("votes_dist", height = "280px"))
              )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # Reactive prediction
  result <- eventReactive(input$predict_btn, {
    new_data <- data.frame(
      imdb_score   = input$imdb_score,
      imdb_votes   = input$imdb_votes,
      runtime      = input$runtime,
      release_year = input$release_year
    )
    prob <- predict(rf_model, new_data, type = "prob")
    pred <- predict(rf_model, new_data)
    list(
      label        = as.character(pred),
      prob_renewed = round(prob[, "Renewed"] * 100),
      prob_cancel  = round(prob[, "Cancelled"] * 100)
    )
  })
  
  # Badge
  output$result_badge <- renderUI({
    if (!input$predict_btn) {
      return(div(class="renewal-badge badge-pending",
                 "Awaiting prediction..."))
    }
    r <- result()
    if (r$label == "Renewed") {
      div(class="renewal-badge badge-renewed", "✔ Likely Renewed")
    } else {
      div(class="renewal-badge badge-cancelled", "✘ Likely Cancelled")
    }
  })
  
  # Probability bar
  output$prob_bar <- renderUI({
    req(result())
    pct   <- result()$prob_renewed
    color <- ifelse(pct >= 60, "#1d9e75",
                    ifelse(pct >= 40, "#ba7517", "#c0392b"))
    div(class = "prob-wrap",
        div(class = "prob-fill",
            style = paste0("width:", pct, "%; background:", color, ";")))
  })
  
  output$prob_pct <- renderUI({
    req(result())
    p(paste0(result()$prob_renewed, "% renewal probability"),
      style = "color:#fff; font-size:18px; font-weight:bold; margin:4px 0;")
  })
  
  # Risk text
  output$risk_text <- renderUI({
    req(result())
    pct <- result()$prob_renewed
    txt <- if (pct >= 70) "Low risk — strong indicators of renewal"
    else if (pct >= 50) "Moderate risk — borderline signals"
    else if (pct >= 30) "High risk — weak engagement metrics"
    else "Critical risk — cancellation very likely"
    col <- if (pct >= 70) "#4ade80"
    else if (pct >= 50) "#facc15"
    else if (pct >= 30) "#fb923c"
    else "#f87171"
    p(txt, style = paste0("color:", col, "; font-size:13px;"))
  })
  
  # Stat displays
  output$stat_score <- renderText({ input$imdb_score })
  output$stat_votes <- renderText({
    formatC(input$imdb_votes, format="d", big.mark=",")
  })
  
  # Feature importance plot
  output$importance_plot <- renderPlotly({
    plot_ly(imp_df,
            x = ~MeanDecreaseGini,
            y = ~reorder(Feature, MeanDecreaseGini),
            type  = "bar",
            orientation = "h",
            marker = list(color = c("#534ab7","#534ab7",
                                    "#e8a020","#e8a020"))) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(color = "#ccc", size = 12),
        xaxis  = list(title = "Importance score", color = "#888",
                      gridcolor = "#2a2a3e"),
        yaxis  = list(title = "", color = "#ccc"),
        margin = list(l = 10, r = 10, t = 10, b = 40)
      )
  })
  
  # Shows table
  output$shows_table <- renderDT({
    ref_shows |>
      rename(
        Title       = title,
        "IMDb Score" = imdb_score,
        "IMDb Votes" = imdb_votes,
        "Runtime (min)" = runtime,
        Seasons     = seasons,
        Outcome     = outcome
      ) |>
      datatable(
        options = list(pageLength = 10, dom = "ftp",
                       scrollX = TRUE),
        rownames = FALSE
      ) |>
      formatStyle("Outcome",
                  color = styleEqual(
                    c("Renewed", "Cancelled"),
                    c("#4ade80", "#f87171")
                  )
      )
  })
  
  # Score distribution plot
  output$score_dist <- renderPlotly({
    plot_ly(tv_data, x = ~imdb_score, color = ~outcome,
            type = "histogram", opacity = 0.7,
            colors = c("Cancelled" = "#c0392b",
                       "Renewed"   = "#1d9e75")) |>
      layout(
        barmode      = "overlay",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(color = "#ccc"),
        xaxis = list(title = "IMDb Score", color = "#888",
                     gridcolor = "#2a2a3e"),
        yaxis = list(title = "Count", color = "#888",
                     gridcolor = "#2a2a3e"),
        legend = list(font = list(color = "#ccc"))
      )
  })
  
  # Votes distribution plot
  output$votes_dist <- renderPlotly({
    plot_ly(tv_data, x = ~imdb_votes, color = ~outcome,
            type = "histogram", opacity = 0.7,
            colors = c("Cancelled" = "#c0392b",
                       "Renewed"   = "#1d9e75")) |>
      layout(
        barmode      = "overlay",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(color = "#ccc"),
        xaxis = list(title = "IMDb Votes", color = "#888",
                     gridcolor = "#2a2a3e"),
        yaxis = list(title = "Count", color = "#888",
                     gridcolor = "#2a2a3e"),
        legend = list(font = list(color = "#ccc"))
      )
  })
}

shinyApp(ui, server)