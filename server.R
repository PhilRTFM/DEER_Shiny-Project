# ============================================================
# D.E.E.R. - Differential Expression and Enrichment Analysis in R
# Project: D.E.E.R. - Shiny app for Volcano plots (Fold Change focused)
#
# Description : Projet de Master 2, Développement d'un outil R Shiny pour l'analyse de données transcriptomiques
#
# Contact: philippe.stocker@univ-rouen.fr
# Affiliation: Université de Rouen Normandie
# ============================================================

## Partie Serveur 

server <- function(input, output, session) {
  
  observe({ updateTabItems(session, "tabs", "home_tab") })
  
  expected_cols <- c("Gene", "log2FC", "p_value")
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (tolower(ext) != "csv") {
      showNotification("Erreur : veuillez importer un fichier CSV.", type = "error")
      return(NULL)
    }
    df <- read.csv(input$file$datapath, sep = ",", stringsAsFactors = FALSE)
    missing_cols <- setdiff(expected_cols, colnames(df))
    if (length(missing_cols) > 0) {
      showNotification(paste("Colonnes manquantes :", paste(missing_cols, collapse = ", ")), type = "error")
      return(NULL)
    }
    df
  })
  
  # ---- TABLE UNIQUEMENT ----
  output$table_volcano <- renderDataTable({
    req(data())
    datatable(data(),
              options = list(pageLength = 10,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#ffeaa7', 'color': '#000'});",
                               "}")))
  })
  
  # ---- INFOBOX ----
  output$count_box <- renderInfoBox({
    req(data())
    df <- data()
    infoBox(
      "Fichier chargé",
      paste(nrow(df), "lignes"),
      subtitle = "Visualisation active du tableau",
      color = "yellow",
      fill = TRUE,
      icon = icon("table")
    )
  })
}