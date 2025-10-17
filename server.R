# ============================================================
# D.E.E.R. - Differential Expression and Enrichment Analysis in R
# Project: D.E.E.R. - Shiny app for Differential Expression and Enrichement analysis
#
# Description : Projet de Master 2, Développement d'un outil R Shiny 
#pour l'analyse de données transcriptomiques
#
# Contact: philippe.stocker@univ-rouen.fr
# Affiliation: Université de Rouen Normandie
# ============================================================

## Partie Serveur - Logique de l'application Shiny
# Cette fonction définit la logique serveur qui gère :
# - Le chargement et la validation des fichiers CSV
# - L'affichage des données dans un tableau
# - La génération d'informations sur le dataset chargé

server <- function(input, output, session) {
  
  # ---- INITIALISATION ----
  # Redirection automatique vers l'onglet d'accueil au démarrage
  observe({ 
    updateTabItems(session, "tabs", "home_tab") 
  })
  
  # Définition des colonnes obligatoires pour les fichiers d'expression différentielle
  expected_cols <- c("Gene", "log2FC", "p_value")
  
  # ---- CHARGEMENT ET VALIDATION DES DONNÉES ----
  # Fonction réactive pour charger et valider le fichier CSV uploadé
  # Vérifie le format de fichier et la présence des colonnes obligatoires
  data <- reactive({
    # Vérification que l'utilisateur a sélectionné un fichier
    req(input$file)
    
    # Extraction de l'extension du fichier
    ext <- tools::file_ext(input$file$name)
    
    # Validation du format CSV
    if (tolower(ext) != "csv") {
      showNotification("Erreur : veuillez importer un fichier CSV.", 
                       type = "error")
      return(NULL)
    }
    
    # Lecture du fichier CSV avec les paramètres standards
    df <- read.csv(input$file$datapath, 
                   sep = ",", 
                   stringsAsFactors = FALSE)
    
    # Vérification de la présence des colonnes obligatoires
    missing_cols <- setdiff(expected_cols, colnames(df))
    if (length(missing_cols) > 0) {
      showNotification(paste("Colonnes manquantes :", 
                            paste(missing_cols, collapse = ", ")), 
                       type = "error")
      return(NULL)
    }
    
    # Retour du dataframe validé
    df
  })
  
  # ---- AFFICHAGE DU TABLEAU DE DONNÉES ----
  # Génération du tableau interactif pour visualiser les données d'expression
  output$table_volcano <- renderDataTable({
    # Vérification que les données sont disponibles
    req(data())
    
    # Création du tableau avec les options de style personnalisées
    datatable(data(),
              options = list(
                pageLength = 10,  # Nombre de lignes par page
                initComplete = JS(
                  # Script JavaScript pour personnaliser l'en-tête du tableau
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({",
                  "'background-color': '#ffeaa7', 'color': '#000'});",
                  "}"
                )
              ))
  })
  
  # ---- BOÎTE D'INFORMATION SUR LE DATASET ----
  # Affichage d'une infoBox avec le nombre de lignes du fichier chargé
  output$count_box <- renderInfoBox({
    # Vérification que les données sont disponibles
    req(data())
    df <- data()
    
    # Création de l'infoBox avec les statistiques du dataset
    infoBox(
      title = "Fichier chargé",
      value = paste(nrow(df), "lignes"),
      subtitle = "Visualisation active du tableau",
      color = "yellow",
      fill = TRUE,
      icon = icon("table")
    )
  })
}