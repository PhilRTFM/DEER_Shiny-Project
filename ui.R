# ============================================================
# D.E.E.R. - Differential Expression and Enrichment Analysis in R
# Project: D.E.E.R. - Shiny app for Volcano plots (Fold Change focused)
#
# Description : Projet de Master 2, Développement d'un outil R Shiny pour l'analyse de données transcriptomiques
#
# Contact: philippe.stocker@univ-rouen.fr
# Affiliation: Université de Rouen Normandie
# ============================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

# ============================================================
# COULEURS GLOBALES
# ============================================================
primary_color   <- "#fdcb6e"   # Jaune foncé
secondary_color <- "#ffeaa7"   # Jaune clair
accent_color    <- "#2d3436"   # Gris anthracite

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "yellow",
  
  # HEADER ----------------------------------------------------
  dashboardHeader(
    title = span("🦌 D.E.E.R Shiny",
                 style = paste0("color:", accent_color, "; font-weight:bold;"))
  ),
  
  # SIDEBAR ---------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil", tabName = "home_tab", icon = icon("home")),
      fileInput("file", "Téléverser un fichier CSV :", accept = ".csv"),
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
  
  # BODY ------------------------------------------------------
  dashboardBody(
    use_theme("www/deer_theme.css"),  # Application du thème CSS exporté
    
    # Splash Screen --------------------------------------------
    tags$head(
      tags$style(HTML(paste0("
        #loading-content {
          position: fixed;
          background-color: ", primary_color, ";
          opacity: 0.98;
          color: ", accent_color, ";
          top: 0; left: 0; right: 0; bottom: 0;
          z-index: 9999;
          text-align: center;
          padding-top: 200px;
          font-family: 'Roboto', sans-serif;
        }
        #loading-content img {
          width: 200px;
          animation: pulse 3s infinite;
        }
        @keyframes pulse {
          0% { opacity: 0.7; transform: scale(0.95); }
          50% { opacity: 1; transform: scale(1.05); }
          100% { opacity: 0.7; transform: scale(0.95); }
        }
      ")))
    ),
    
    div(id = "loading-content",
        img(src = "DEER_logo.svg", alt = "Chargement..."),
        h2("Chargement de D.E.E.R.", style = paste0("color:", accent_color, ";"))
    ),
    
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('#loading-content').fadeOut(800);
      });
    ")),
    
    # ---- TABS ----
    tabItems(
      
      # ---- 1. HOME ----
      tabItem(tabName = "home_tab",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  div(
                    style = "display:flex; align-items:center; justify-content:center; gap:40px;",
                    div(style = "flex:0 0 auto; text-align:center;",
                        img(src = "DEER_logo.svg", width = "120px")),
                    div(
                      style = "flex:1; text-align:left;",
                      h2("D.E.E.R. - Differential Expression and Enrichment in R",
                         style = paste0("color:", accent_color, "; font-weight:700;")),
                      p("D.E.E.R. (Differential Expression and Enrichment in R) est une application R Shiny développée dans le cadre d’un projet de Master 2. Son objectif est de fournir un outil ergonomique, interactif et modulaire pour :"),
                      tags$ul(
                        tags$li(HTML("Explorer les résultats d’<strong>analyse d’expression différentielle</strong> (RNA-seq) ;")),
                        tags$li(HTML("Visualiser les <strong>log2 Fold Change</strong> sous forme de <strong>Volcano Plot</strong> ;")),
                        tags$li(HTML("Intégrer à terme des modules d’<strong>enrichissement fonctionnel (GO, KEGG)</strong> ;")),
                        tags$li("Offrir une interface claire et harmonisée pour les biologistes.")
                      )
                    )
                    
                  )
                )
              ),
              fluidRow(
                box(title = "Bloc 1 (bientôt disponible)", width = 6, status = "info", height = "300px"),
                box(title = "Bloc 2 (bientôt disponible)", width = 6, status = "info", height = "300px")
              )
      ),
      
      # ---- 2. VOLCANO ----
      tabItem(tabName = "volcano_tab",
              fluidRow(infoBoxOutput("count_box", width = 12)),
              fluidRow(
                box(title = "Volcano Plot (à venir)", width = 8, status = "primary", height = "500px",
                    div(style = "text-align:center; margin-top:200px; font-style:italic;",
                        "Le graphique Volcano sera affiché ici.")),
                box(title = "Paramètres", width = 4, status = "info",
                    sliderInput("FCcutoff", "Seuil log2 Fold Change",
                                min = 0, max = 3, value = 1, step = 0.1),
                    sliderInput("P-cutoff", "Seuil P-valeur",
                                min = 0, max = 1, value = 0.05, step = 0.01),
                    textInput("title", "Titre du graphique :", value = "Volcano Plot (FC uniquement)"),
                    downloadButton("downloadVolcano", "Télécharger le graphique"))
              ),
              fluidRow(
                box(title = "Table associée au fichier importé", width = 12, status = "success",
                    withSpinner(dataTableOutput("table_volcano"), type = 8, color = primary_color))
              )
      ),
      
      # ---- 3. À PROPOS ----
      tabItem(tabName = "about_tab",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "À propos de D.E.E.R.",
                    div(style = "display:flex; align-items:center; justify-content:center; gap:40px;",
                        div(style = "flex:0 0 auto; text-align:center;",
                            img(src = "DEER_logo.svg", width = "250px")),
                        div(style = "flex:1; text-align:left;",
                            h3("D.E.E.R. - Differential Expression and Enrichment in R",
                               style = paste0("color:", accent_color, "; font-weight:700;")),
                            p("Application Shiny pour l’analyse et la visualisation des résultats d’expression différentielle 
                               et d’enrichissement fonctionnel en bioinformatique.",
                              style = paste0("color:", accent_color, "; font-size:16px;")),
                            br(),
                            tags$ul(
                              tags$li(span("Auteur : ", style = "font-weight:bold;"), "Philippe Stocker"),
                              tags$li(span("Affiliation : ", style = "font-weight:bold;"),
                                      "Université de Rouen Normandie"),
                              tags$li(span("Contact : ", style = "font-weight:bold;"),
                                      "philippe.stocker@univ-rouen.fr")
                            ),
                            br(),
                            tags$a(href = "https://github.com/PhilRTFM/DEER_Shiny-Project",
                                   target = "_blank",
                                   class = "btn btn-default",
                                   style = paste0("background-color:", primary_color,
                                                  "; color:", accent_color,
                                                  "; border:none; font-weight:600;"),
                                   icon("github"), "  GitHub du projet")
                        )
                    )
                )
              )
      )
    )
  )
)
