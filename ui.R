# ========================= ui.R (Modifié pour Plotly + Onglets DT) =========================
# D.E.E.R. — Differential Expression & Enrichment in R (multi-select interactive version)

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly) # Ajout de la bibliothèque plotly

# ---- COLORS ----
primary_color   <- "#fdcb6e"
secondary_color <- "#ffeaa7"
accent_color    <- "#2d3436"

ui <- dashboardPage(
  skin = "yellow",
  
  # ---- HEADER ----
  dashboardHeader(
    title = span("🦌 D.E.E.R Shiny", style = paste0("color:", accent_color, "; font-weight:bold;"))
  ),
  
  # ---- SIDEBAR ----
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil", tabName = "home_tab", icon = icon("home")),
      fileInput("file", "Téléverser un fichier CSV :", accept = ".csv"),
      uiOutput("dataset_status_sidebar"),
      menuItem("Analyse", tabName = "volcano_tab", icon = icon("fire")),
      selectInput(
        inputId = "organism_sidebar",
        label = "Sélectionner l'organisme :",
        choices = c("Homo sapiens", "Mus musculus", "Danio rerio", "Saccharomyces cerevisiae"),
        selected = "Homo sapiens"
      ),
      menuItem("À propos", tabName = "about_tab", icon = icon("info-circle"))
    )
  ),
  
  # ---- BODY ----
  dashboardBody(
    tabItems(
      # ===== HOME =====
      tabItem(tabName = "home_tab",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Bienvenue",
                    div(style = "display:flex; align-items:center; gap:32px;",
                        img(src = "DEER_logo.svg", width = "120px"),
                        div(
                          h2("D.E.E.R. - Differential Expression and Enrichment in R",
                             style = paste0("color:", accent_color, "; font-weight:700;")),
                          p("Importez un CSV pour lancer l'analyse. Accédez ensuite à l'onglet Analyse."),
                          tags$ul(
                            tags$li("Volcano Plot interactif avec info-bulles et sélection multiple"),
                            tags$li("Lignes de seuils ajustables (log2FC et -log10(padj))"),
                            tags$li("Téléchargement du graphique avec gènes annotés")
                          )
                        )
                    )
                )
              )
      ),
      
      # ===== VOLCANO TAB =====
      tabItem(tabName = "volcano_tab",
              fluidRow(infoBoxOutput("count_box", width = 12)),
              
              # --- Volcano Plot and Controls ---
              fluidRow(
                box(
                  title = "Volcano Plot interactif (ratio carré, multi-sélection)",
                  width = 8,
                  status = "primary",
                  div(
                    style = "display:flex; align-items:center; justify-content:center;",
                    # Modification : Remplacement de plotOutput par plotlyOutput
                    plotlyOutput("volcano_plot", height = "600px", width = "600px") %>%
                      withSpinner(type = 8, color = primary_color)
                  )
                ),
                
                box(
                  title = "Paramètres", width = 4, status = "info",
                  sliderInput("fc_thresh", "Seuil log2 Fold Change", min = 0, max = 3, value = 1, step = 0.1),
                  sliderInput("pval_thresh", "Seuil -log10(P-adj)", min = 0, max = 10, value = 1.3, step = 0.1),
                  textInput("plot_title", "Titre du graphique :", value = "Volcano Plot interactif"),
                  selectInput(
                    "download_format", "Format du graphique :",
                    choices = c("PNG", "PDF", "SVG"), selected = "PNG"
                  ),
                  downloadButton("downloadVolcano", "Télécharger le graphique annoté")
                )
              ),
              
              # --- Table ---
              # Modification : Remplacement de box() par tabBox() avec 4 onglets
              fluidRow(
                tabBox(
                  title = "Table associée (Sélectionnez dans l'onglet 'Tous')",
                  width = 12,
                  id = "table_tabs",
                  tabPanel("Tous", 
                           withSpinner(DTOutput("table_volcano"), type = 8, color = primary_color)
                  ),
                  tabPanel("Surexprimés", 
                           withSpinner(DTOutput("table_over"), type = 8, color = primary_color)
                  ),
                  tabPanel("Sousexprimés", 
                           withSpinner(DTOutput("table_under"), type = 8, color = primary_color)
                  ),
                  tabPanel("Sélectionnés", 
                           withSpinner(DTOutput("table_selected"), type = 8, color = primary_color)
                  )
                )
              )
      ),
      
      # ===== ABOUT =====
      tabItem(tabName = "about_tab",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "À propos de D.E.E.R.",
                    p("Application Shiny pour l'analyse et la visualisation interactive des résultats d'expression différentielle."),
                    tags$ul(
                      tags$li(span("Auteur : ", style = "font-weight:bold;"), "Philippe Stocker"),
                      tags$li(span("Affiliation : ", style = "font-weight:bold;"), "Université de Rouen Normandie"),
                      tags$li(span("Contact : ", style = "font-weight:bold;"), "philippe.stocker@univ-rouen.fr")
                    ),
                    tags$a(
                      href = "https://github.com/PhilRTFM/DEER_Shiny-Project", target = "_blank",
                      class = "btn btn-default",
                      style = paste0("background-color:", primary_color, "; color:", accent_color, "; border:none; font-weight:600;"),
                      icon("github"), "  GitHub du projet"
                    )
                )
              )
      )
    )
  )
)