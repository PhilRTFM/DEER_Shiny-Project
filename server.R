# ===================================================================
# server.R (Corrigé Rendu 2 - Fix hover_text)
#
# Auteur: Philippe Stocker
# Projet: D.E.E.R. Shiny
#
# Description:
# Logique serveur corrigée pour le Rendu 2.
# CORRECTION: L'objet hover_text est créé AVANT le subset df_selected.
# Intègre shinyjs, sliders dynamiques, et sélection unifiée.
# ===================================================================

# --- Bibliothèques requises ---
library(ggplot2)       
library(shiny)
library(DT)            
library(shinydashboard)
library(shinycssloaders) 
library(dplyr)         
library(ggrepel)       
library(plotly)        
library(svglite)       
library(shinyjs)       

# --- Définition des couleurs ---
primary_color   <- "#fdcb6e"
secondary_color <- "#ffeaa7"
accent_color    <- "#2d3436"

server <- function(input, output, session) {
  
  # ===================================================================
  # VALEURS RÉACTIVES (Reactive Values)
  # ===================================================================
  
  dataset_reactive <- reactiveVal(NULL)
  selected_genes <- reactiveVal(character(0)) 
  
  # ===================================================================
  # VALIDATION ET LECTURE DES DONNÉES
  # ===================================================================
  
  #' Vérifie l'intégrité du jeu de données téléversé
  #' (Gestion des séparateurs, colonnes manquantes, valeurs non-numériques)
  check_dataset_integrity <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
      return(list(valid = FALSE, message = "Fichier vide ou non chargé."))
    
    if (ncol(df) == 1 && grepl(";", df[1, 1]))
      df <- tryCatch(read.csv(input$file$datapath, sep = ";", stringsAsFactors = FALSE), error = function(e) NULL)
    
    required_cols <- c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0)
      return(list(valid = FALSE, message = paste0("Colonnes manquantes : ", paste(missing_cols, collapse = ", "))))
    
    df$log2FC <- suppressWarnings(as.numeric(gsub(",", ".", df$log2FC)))
    df$pval   <- suppressWarnings(as.numeric(gsub(",", ".", df$pval)))
    df$padj   <- suppressWarnings(as.numeric(gsub(",", ".", df$padj)))
    
    if (any(is.na(df$log2FC)) || any(is.na(df$pval)) || any(is.na(df$padj)))
      return(list(valid = FALSE, message = "Les colonnes log2FC, pval ou padj contiennent des valeurs non numériques."))
    
    list(valid = TRUE, message = "Jeu de données conforme.", df = df)
  }
  
  
  #' Observeur pour le téléversement du fichier
  observeEvent(input$file, {
    req(input$file)
    
    df <- tryCatch(
      read.csv(input$file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    check <- check_dataset_integrity(df)
    
    if (check$valid) {
      dataset_reactive(check$df)
      selected_genes(character(0)) 
      
    } else {
      dataset_reactive(NULL)
      selected_genes(character(0))
      
      showModal(modalDialog(
        title = "Erreur de Fichier",
        div(
          style = "text-align: center; padding: 20px;",
          shiny::icon("times-circle", style = "color: #d9534f; font-size: 64px; margin-bottom: 20px;")
        ),
        tags$p(check$message, 
               style = "font-weight:bold; text-align: center; font-size: 16px;"),
        easyClose = TRUE,
        footer = modalButton("Fermer"),
        size = "s" 
      ))
      
      # Réinitialiser le fileInput (Spécification Rendu 2)
      shinyjs::reset("file")
    }
  })
  
  
  # ===================================================================
  # MISE À JOUR DYNAMIQUE DES CURSEURS (SLIDERS)
  # ===================================================================
  
  #' Observeur pour mettre à jour les curseurs en fonction des données
  observeEvent(dataset_reactive(), {
    df <- dataset_reactive()
    
    if (is.null(df)) {
      # Réinitialisation
      updateSliderInput(session, "fc_thresh", max = 3, value = 1)
      updateSliderInput(session, "pval_thresh", max = 10, value = 1.3)
      
    } else {
      # Mise à jour dynamique
      max_fc <- ceiling(max(abs(df$log2FC), na.rm = TRUE))
      max_pval <- ceiling(max(-log10(df$padj), na.rm = TRUE))
      
      updateSliderInput(session, "fc_thresh", 
                        max = max(3, max_fc), 
                        value = 1)
      updateSliderInput(session, "pval_thresh", 
                        max = max(10, max_pval), 
                        value = 1.3)
    }
  })
  
  
  # ===================================================================
  # CALCULS RÉACTIFS (Cached)
  # ===================================================================
  
  #' Données filtrées et calcul de significativité
  filtered_data <- reactive({
    df <- dataset_reactive(); req(df)
    req(input$fc_thresh, input$pval_thresh) 
    
    df$overexpressed  <- df$log2FC >= input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    df$underexpressed <- df$log2FC <= -input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    
    df$significance <- ifelse(df$overexpressed, "Over", 
                              ifelse(df$underexpressed, "Under", "NS"))
    
    df
  })
  
  
  # ===================================================================
  # RENDU: GRAPHIQUE PLOTLY INTERACTIF
  # ===================================================================
  
  output$volcano_plot <- renderPlotly({
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    
    # --- CORRECTION DE L'ERREUR ---
    # 1. Créer hover_text D'ABORD
    df$hover_text <- paste0(
      "<b>", df$GeneName, "</b>",
      "<br>log2FC: ", round(df$log2FC, 2),
      "<br>-log10(padj): ", round(-log10(df$padj), 2),
      "<br>padj: ", signif(df$padj, 3),
      "<br>Significance: ", df$significance
    )
    
    # 2. Créer df_selected ENSUITE
    # Il héritera maintenant de la colonne hover_text
    df_selected <- df[df$GeneName %in% selected, , drop = FALSE]
    
    pal <- c("NS" = "grey70", "Over" = "#0072B2", "Under" = "#D55E00")
    
    # Calcul des axes dynamiques
    max_y <- max(c(0, -log10(df$padj), input$pval_thresh), na.rm = TRUE) * 1.05
    min_x <- min(c(0, df$log2FC, -input$fc_thresh), na.rm = TRUE) * 1.05
    max_x <- max(c(0, df$log2FC, input$fc_thresh), na.rm = TRUE) * 1.05
    
    if (is.infinite(max_y) || max_y == 0) max_y <- 1
    if (is.infinite(min_x) || min_x == 0) min_x <- -1
    if (is.infinite(max_x) || max_x == 0) max_x <- 1
    
    
    # --- Création du Plotly natif ---
    p <- plot_ly(
      data = df,
      x = ~log2FC,
      y = ~-log10(padj),
      type = 'scatter',
      mode = 'markers',
      color = ~significance,
      colors = pal,
      marker = list(size = 8, opacity = 0.7, line = list(color = 'black', width = 0.5)),
      text = ~hover_text, # <- Fonctionne
      hoverinfo = 'text',
      key = ~GeneName,         
      source = "volcano_plot" 
    )
    
    # --- Ajout de la couche de SURBRILLANCE ---
    if (nrow(df_selected) > 0) {
      p <- p %>% add_trace(
        data = df_selected,
        x = ~log2FC,
        y = ~-log10(padj),
        type = 'scatter',
        mode = 'markers',
        color = ~significance,
        colors = pal,
        marker = list(size = 12, opacity = 1, line = list(color = 'black', width = 1.5)),
        text = ~hover_text, # <- Fonctionne maintenant
        hoverinfo = 'text',
        key = ~GeneName,
        inherit = FALSE,
        showlegend = FALSE
      )
    }
    
    # --- Layout et Lignes de Seuil ---
    p <- p %>% layout(
      title = list(text = input$plot_title, x = 0.5, font = list(size = 14, family = "sans-serif", color = "black")),
      xaxis = list(title = "log2(Fold Change)", range = c(min_x, max_x), zeroline = FALSE),
      yaxis = list(title = "-log10(P-adj)", range = c(0, max_y), zeroline = FALSE),
      showlegend = TRUE,
      legend = list(title = list(text = '<b>Significance</b>')),
      shapes = list(
        list(type = 'line', x0 = input$fc_thresh, x1 = input$fc_thresh, y0 = 0, y1 = max_y,
             line = list(color = "#E64B35FF", dash = 'dash')),
        list(type = 'line', x0 = -input$fc_thresh, x1 = -input$fc_thresh, y0 = 0, y1 = max_y,
             line = list(color = "#E64B35FF", dash = 'dash')),
        list(type = 'line', x0 = min_x, x1 = max_x, y0 = input$pval_thresh, y1 = input$pval_thresh,
             line = list(color = "#4DBBD5FF", dash = 'dash'))
      )
    )
    
    p
  })
  
  
  # ===================================================================
  # RENDU: TABLES (DT)
  # ===================================================================
  
  common_dt_options <- list(scrollX = TRUE, pageLength = 10, stateSave = TRUE)
  
  # --- Table 1: Tous (SÉLECTIONNABLE) ---
  output$table_volcano <- renderDT({
    df <- filtered_data(); req(df)
    df_display <- df[, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    
    # SYNCHRONISATION: Trouve les index des lignes qui sont dans selected_genes()
    selected_row_indices <- which(df_display$GeneName %in% selected_genes())
    
    datatable(df_display, 
              selection = list(mode = "multiple", selected = selected_row_indices),
              extensions = 'Select',
              options = common_dt_options,
              rownames = FALSE
    )
  })
  
  # --- Table 2: Surexprimés (LECTURE SEULE) ---
  output$table_over <- renderDT({
    df <- filtered_data(); req(df)
    df_over <- df[df$significance == "Over", c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")]
    datatable(df_over, 
              selection = 'none', # Lecture seule
              options = common_dt_options,
              rownames = FALSE
    )
  })
  
  # --- Table 3: Sousexprimés (LECTURE SEULE) ---
  output$table_under <- renderDT({
    df <- filtered_data(); req(df)
    df_under <- df[df$significance == "Under", c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")]
    datatable(df_under, 
              selection = 'none', # Lecture seule
              options = common_dt_options,
              rownames = FALSE
    )
  })
  
  # --- Table 4: Sélectionnés (LECTURE SEULE) ---
  output$table_selected <- renderDT({
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    
    if (length(selected) == 0) {
      df_selected <- df[FALSE, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    } else {
      df_selected <- df[df$GeneName %in% selected, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    }
    
    datatable(df_selected, 
              selection = 'none', 
              options = common_dt_options, 
              rownames = FALSE)
  })
  
  
  # ===================================================================
  # LOGIQUE DE SÉLECTION (Observateurs)
  # ===================================================================
  
  # --- 1. Sélection depuis la TABLE "Tous" ---
  observeEvent(input$table_volcano_rows_selected, {
    df <- filtered_data(); req(df)
    selected_rows <- input$table_volcano_rows_selected
    
    genes_from_table <- if (length(selected_rows) > 0) df[selected_rows, "GeneName"] else character(0)
    
    # Anti-boucle: Ne met à jour la source de vérité que si nécessaire
    if (!identical(sort(genes_from_table), sort(selected_genes()))) {
      selected_genes(genes_from_table)
    }
  }, ignoreNULL = FALSE) 
  
  
  # --- 2. Sélection depuis le CLIC sur le GRAPHIQUE ---
  observeEvent(event_data("plotly_click", source = "volcano_plot"), {
    
    clicked_gene <- event_data("plotly_click", source = "volcano_plot")$key
    if (is.null(clicked_gene)) return()
    
    current_selection <- selected_genes()
    
    # Logique de BASCULE (Toggle):
    if (clicked_gene %in% current_selection) {
      new_selection <- setdiff(current_selection, clicked_gene) 
    } else {
      new_selection <- c(current_selection, clicked_gene)
    }
    
    selected_genes(new_selection)
  })
  
  
  # ===================================================================
  # RENDU: AUTRES (InfoBox, Export)
  # ===================================================================
  
  # --- InfoBox: Statistiques ---
  output$count_box <- renderInfoBox({
    df <- filtered_data(); req(df)
    
    n_over  <- sum(df$significance == "Over", na.rm = TRUE)
    n_under <- sum(df$significance == "Under", na.rm = TRUE)
    n_sig   <- n_over + n_under
    total   <- nrow(df)
    
    pct_total <- ifelse(total > 0, round(100 * n_sig / total, 1), 0)
    pct_over  <- ifelse(n_sig > 0, round(100 * n_over / n_sig, 1), 0)
    pct_under <- ifelse(n_sig > 0, round(100 * n_under / n_sig, 1), 0)
    
    infoBox(
      title = HTML(paste0(
        "<strong>Points significatifs</strong> : ", n_sig, " (", pct_total, "%)<br>",
        "<strong>Points sur-exprimés</strong> : ", n_over, " (", pct_over, "%)<br>",
        "<strong>Points sous-exprimés</strong> : ", n_under, " (", pct_under, "%)"
      )),
      value = NULL,
      icon = icon("chart-area"),
      color = if (n_sig > 0) "green" else "yellow",
      fill = TRUE
    )
  })
  
  # --- Fonction Helper pour créer le graphique d'EXPORT (ggplot) ---
  create_download_plot <- function() {
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    
    df_selected <- df[df$GeneName %in% selected, , drop = FALSE]
    
    p <- ggplot(df, aes(x = log2FC, y = -log10(padj))) +
      geom_point(aes(fill = factor(significance, levels = c("NS", "Over", "Under"))),
                 color = "black", shape = 21, alpha = 0.8, size = 3, stroke = 0.4) +
      geom_vline(xintercept = c(-input$fc_thresh, input$fc_thresh), color = "#E64B35FF", linetype = "dashed") +
      geom_hline(yintercept = input$pval_thresh, color = "#4DBBD5FF", linetype = "dashed") +
      scale_fill_manual(
        values = c("NS" = "grey70", "Over" = "#0072B2", "Under" = "#D55E00"),
        name = "Significance"
      )
    
    # Ajout des étiquettes ggrepel
    if (nrow(df_selected) > 0) {
      # Création du label_text (similaire à votre version)
      df_selected$label_text <- paste0(
        df_selected$GeneName,
        "\nlog2FC = ", round(df_selected$log2FC, 2)
      )
      
      p <- p + ggrepel::geom_label_repel(
        data = df_selected,
        aes(label = label_text), # Utilisation de label_text
        color = "black",
        fontface = "bold",
        size = 3.5,
        box.padding = 0.4,
        point.padding = 0.3,
        max.overlaps = 50,
        force = 1.5,
        segment.color = "grey50",
        label.size = 0.2
      )
    }
    
    p <- p +
      labs(title = input$plot_title, x = "log2(Fold Change)", y = "-log10(P-adj)") +
      coord_cartesian(xlim = c(-3, 3)) + 
      theme_bw(base_size = 13) +
      theme(
        aspect.ratio = 1, 
        legend.position = "right",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
      )
    
    return(p)
  }
  
  # --- Logique de Téléchargement du Graphique ---
  output$downloadVolcano <- downloadHandler(
    filename = function() {
      paste0("volcano_plot_", Sys.Date(), ".", tolower(input$download_format))
    },
    content = function(file) {
      p_static <- create_download_plot()
      
      plot_device <- switch(tolower(input$download_format),
                            "png" = "png",
                            "pdf" = "pdf",
                            "svg" = svglite::svglite,
                            "png") 
      
      ggsave(
        file,
        plot = p_static,
        device = plot_device,
        width = 8,
        height = 8, 
        units = "in",
        dpi = 300
      )
    }
  )
  
}