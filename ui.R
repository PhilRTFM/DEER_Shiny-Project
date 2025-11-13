# ===================================================================
# ui.R (Corrigé Rendu 2)
#
# Auteur: Philippe Stocker
# Projet: D.E.E.R. Shiny
#
# Description:
# UI corrigée pour le Rendu 2.
# Intègre shinyjs et un conteneur de plot responsive (ratio 1:1).
# Supprime le code mort (uiOutput).
# ===================================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(plotly) 
library(shinyjs) # <-- AJOUT: Pour la robustesse (ex: reset fileInput)

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
      fileInput("file", "Téléverser un fichier CSV :", 
                accept = c(".csv", "text/csv", "text/plain")), # Accepte plus de types
      # uiOutput("dataset_status_sidebar"), # <-- SUPPRIMÉ (Code mort)
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
    # --- AJOUT: Initialisation de shinyjs ---
    useShinyjs(),
    
    # --- AJOUT: CSS pour le plot responsive 1:1 ---
    tags$head(
      tags$style(HTML("
        .responsive-plot-container {
          position: relative;
          padding-bottom: 100%; /* Ratio 1:1 */
          height: 0;
          overflow: hidden;
        }
        .responsive-plot-container .shiny-plot-output,
        .responsive-plot-container .plotly {
          position: absolute;
          top: 0;
          left: 0;
          width: 100% !important;
          height: 100% !important;
        }
      "))
    ),
    
    tabItems(
      # ===== HOME =====
      tabItem(tabName = "home_tab",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Bienvenue",
                    div(style = "display:flex; align-items:center; gap:32px;",
                        img(src = "DEER_logo.svg", width = "120px"), # Assurez-vous d'avoir 'www/DEER_logo.svg'
                        div(
                          h2("D.E.E.R. - Differential Expression and Enrichment in R",
                             style = paste0("color:", accent_color, "; font-weight:700;")),
                          p("Importez un CSV (séparateur virgule ou point-virgule) pour lancer l'analyse."),
                          tags$ul(
                            tags$li("Volcano Plot interactif avec sélection par clic et zoom."),
                            tags$li("Seuils de significativité dynamiques."),
                            tags$li("Synchronisation entre le graphique et la table 'Tous'."),
                            tags$li("Téléchargement du graphique (PNG, PDF, SVG) avec gènes annotés.")
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
                  title = "Volcano Plot Interactif", # Titre simplifié
                  width = 8,
                  status = "primary",
                  solidHeader = TRUE, # Ajout pour la cohérence
                  
                  # --- MODIFIÉ: Remplacement du div fixe par le conteneur responsive ---
                  div(
                    class = "responsive-plot-container",
                    plotlyOutput("volcano_plot") %>%
                      withSpinner(type = 8, color = primary_color)
                  )
                ),
                
                box(
                  title = "Paramètres", width = 4, status = "info", solidHeader = TRUE, # Ajout
                  # Sliders qui seront mis à jour par le serveur
                  sliderInput("fc_thresh", "Seuil log2 Fold Change", min = 0, max = 3, value = 1, step = 0.1),
                  sliderInput("pval_thresh", "Seuil -log10(P-adj)", min = 0, max = 10, value = 1.3, step = 0.1),
                  
                  hr(), # Séparateur
                  
                  textInput("plot_title", "Titre du graphique :", value = "Volcano Plot"),
                  selectInput(
                    "download_format", "Format du graphique :",
                    choices = c("PNG", "PDF", "SVG"), selected = "PNG"
                  ),
                  downloadButton("downloadVolcano", "Télécharger le graphique annoté")
                )
              ),
              
              # --- Table ---
              fluidRow(
                tabBox(
                  title = "Explorateur de Gènes", # Titre simplifié
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