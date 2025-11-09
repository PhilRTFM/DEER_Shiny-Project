# ========================= server.R (Sélection multi-onglets + "Select All") =========================

library(ggplot2)
library(shiny)
library(DT) # Crucial pour 'Select' et 'dataTableProxy'
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(ggrepel) 
library(plotly) 
library(svglite) 

primary_color   <- "#fdcb6e"
secondary_color <- "#ffeaa7"
accent_color    <- "#2d3436"

server <- function(input, output, session) {
  dataset_reactive <- reactiveVal(NULL)
  selected_genes <- reactiveVal(character(0))
  
  check_dataset_integrity <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
      return(list(valid = FALSE, color = "red", message = "Fichier vide ou non chargé."))
    
    if (ncol(df) == 1 && grepl(";", df[1, 1]))
      df <- tryCatch(read.csv(input$file$datapath, sep = ";", stringsAsFactors = FALSE), error = function(e) NULL)
    
    required_cols <- c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0)
      return(list(valid = FALSE, color = "red", message = paste0("Colonnes manquantes : ", paste(missing_cols, collapse = ", "))))
    
    df$log2FC <- suppressWarnings(as.numeric(gsub(",", ".", df$log2FC)))
    df$pval   <- suppressWarnings(as.numeric(gsub(",", ".", df$pval)))
    df$padj   <- suppressWarnings(as.numeric(gsub(",", ".", df$padj)))
    
    if (any(is.na(df$log2FC)) || any(is.na(df$pval)) || any(is.na(df$padj)))
      return(list(valid = FALSE, color = "red", message = "Les colonnes log2FC, pval ou padj contiennent des valeurs non numériques."))
    
    list(valid = TRUE, color = "green", message = "Jeu de données conforme.", df = df)
  }
  
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read.csv(input$file$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    check <- check_dataset_integrity(df)
    
    output$dataset_status_sidebar <- renderUI({ NULL })
    
    if (check$valid) {
      dataset_reactive(check$df)
      
    } else {
      dataset_reactive(NULL)
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
    }
  })
  
  filtered_data <- function() {
    df <- dataset_reactive(); req(df)
    df$overexpressed  <- df$log2FC >= input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    df$underexpressed <- df$log2FC <= -input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    df$significance <- ifelse(df$overexpressed, "Over", ifelse(df$underexpressed, "Under", "NS"))
    df
  }
  
  # === PLOTLY INTERACTIF (POUR L'AFFICHAGE) ===
  output$volcano_plot <- renderPlotly({
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    
    df$selected_flag <- df$GeneName %in% selected
    df_selected <- df[df$selected_flag, , drop = FALSE]
    
    df$hover_text <- paste0(
      "<b>", df$GeneName, "</b>",
      "<br>log2FC: ", round(df$log2FC, 2),
      "<br>-log10(padj): ", round(-log10(df$padj), 2),
      "<br>padj: ", signif(df$padj, 3),
      "<br>Significance: ", df$significance
    )
    
    pal <- c("NS" = "grey70", "Over" = "#0072B2", "Under" = "#D55E00")
    
    max_y <- max(-log10(df$padj), input$pval_thresh, na.rm = TRUE) * 1.05
    
    p <- plot_ly(
      data = df,
      x = ~log2FC,
      y = ~-log10(padj),
      type = 'scatter',
      mode = 'markers',
      color = ~significance,
      colors = pal,
      marker = list(size = 8, opacity = 0.85, line = list(color = 'black', width = 0.5)),
      text = ~hover_text,
      hoverinfo = 'text',
      key = ~GeneName
    )
    
    if (nrow(df_selected) > 0) {
      df_selected$label_text <- paste0(
        df_selected$GeneName,
        "\nlog2FC = ", round(df_selected$log2FC, 2)
      )
      
      p <- p %>% add_annotations(
        data = df_selected,
        x = ~log2FC,
        y = ~-log10(padj),
        text = ~label_text,
        showarrow = TRUE,
        arrowhead = 0,
        ax = 20, 
        ay = -30,
        font = list(color = 'black', size = 9),
        bgcolor = 'rgba(255, 255, 255, 0.8)',
        bordercolor = 'black',
        borderwidth = 1
      )
    }
    
    p <- p %>% layout(
      title = list(text = input$plot_title, x = 0.5, font = list(size = 14, family = "sans-serif", color = "black")),
      xaxis = list(title = "log2(Fold Change)", range = c(-3, 3), zeroline = FALSE),
      yaxis = list(title = "-log10(P-adj)", 
                   range = c(0, max_y), 
                   zeroline = FALSE),
      showlegend = TRUE,
      legend = list(title = list(text = 'Significance')),
      shapes = list(
        list(type = 'line', x0 = input$fc_thresh, x1 = input$fc_thresh, y0 = 0, y1 = max_y,
             line = list(color = "#E64B35FF", dash = 'dash')),
        list(type = 'line', x0 = -input$fc_thresh, x1 = -input$fc_thresh, y0 = 0, y1 = max_y,
             line = list(color = "#E64B35FF", dash = 'dash')),
        list(type = 'line', x0 = -3, x1 = 3, y0 = input$pval_thresh, y1 = input$pval_thresh,
             line = list(color = "#4DBBD5FF", dash = 'dash'))
      )
    )
    
    p
  })
  
  # === GESTION DES TABLES ===
  
  # Fonction pour générer les options DT (pour éviter la duplication)
  dt_options <- function() {
    list(
      selection = list(mode = "multiple"),
      extensions = 'Select', # Ajoute la colonne checkbox + boutons "Select All/None"
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE
    )
  }
  
  # --- Onglet "Tous" ---
  output$table_volcano <- renderDT({
    df <- filtered_data(); req(df)
    df <- df[, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    datatable(df, dt_options())
  })
  
  # --- Onglet "Surexprimés" ---
  output$table_over <- renderDT({
    df <- filtered_data(); req(df)
    df_over <- df[df$significance == "Over", c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")]
    datatable(df_over, dt_options())
  })
  
  # --- Onglet "Sousexprimés" ---
  output$table_under <- renderDT({
    df <- filtered_data(); req(df)
    df_under <- df[df$significance == "Under", c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")]
    datatable(df_under, dt_options())
  })
  
  # --- Onglet "Sélectionnés" (pas de sélection ici) ---
  output$table_selected <- renderDT({
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    if (length(selected) == 0) {
      df_selected <- df[FALSE, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    } else {
      df_selected <- df[df$GeneName %in% selected, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    }
    datatable(df_selected, selection = 'none', options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # === OBSERVATEURS DE SÉLECTION ===
  # Ils sont "destructifs" : la dernière sélection effectuée remplace la précédente.
  
  observeEvent(input$table_volcano_rows_selected, {
    df <- filtered_data(); req(df)
    selected_rows <- input$table_volcano_rows_selected
    if (length(selected_rows) > 0) {
      selected_genes(df[selected_rows, "GeneName"])
    } else {
      selected_genes(character(0))
    }
  })
  
  observeEvent(input$table_over_rows_selected, {
    df <- filtered_data(); req(df)
    df_over <- df[df$significance == "Over", ]
    selected_rows <- input$table_over_rows_selected
    if (length(selected_rows) > 0) {
      selected_genes(df_over[selected_rows, "GeneName"])
    } else {
      selected_genes(character(0))
    }
  })
  
  observeEvent(input$table_under_rows_selected, {
    df <- filtered_data(); req(df)
    df_under <- df[df$significance == "Under", ]
    selected_rows <- input$table_under_rows_selected
    if (length(selected_rows) > 0) {
      selected_genes(df_under[selected_rows, "GeneName"])
    } else {
      selected_genes(character(0))
    }
  })
  
  # === Reste du code (inchangé) ===
  
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
  
  create_download_plot <- function() {
    df <- filtered_data(); req(df)
    selected <- selected_genes()
    
    df$selected_flag <- df$GeneName %in% selected
    df_selected <- df[df$selected_flag, , drop = FALSE]
    
    if (nrow(df_selected) > 0) {
      df_selected$label_color <- dplyr::case_when(
        df_selected$significance == "Over" ~ "#cce5ff",
        df_selected$significance == "Under" ~ "#f8d7da",
        TRUE ~ "#e9ecef"
      )
      
      df_selected$label_text <- paste0(
        df_selected$GeneName,
        "\nlog2FC = ", round(df_selected$log2FC, 2)
      )
    }
    
    p <- ggplot(df, aes(x = log2FC, y = -log10(padj))) +
      geom_point(aes(fill = factor(significance, levels = c("NS", "Over", "Under"))),
                 color = "black", shape = 21, alpha = 0.85, size = 3, stroke = 0.4, show.legend = TRUE) +
      scale_fill_manual(
        values = c("NS" = "grey70", "Over" = "#0072B2", "Under" = "#D55E00"),
        name = "Significance",
        breaks = c("NS", "Over", "Under"),
        labels = c("NS", "Over", "Under")
      ) +
      geom_vline(xintercept = c(-input$fc_thresh, input$fc_thresh), color = "#E64B35FF", linetype = "dashed") +
      geom_hline(yintercept = input$pval_thresh, color = "#4DBBD5FF", linetype = "dashed")
    
    if (nrow(df_selected) > 0) {
      p <- p + geom_label_repel(
        data = df_selected,
        aes(x = log2FC, y = -log10(padj), label = label_text),
        fill = df_selected$label_color,
        color = "black",
        fontface = "bold",
        size = 3.5,
        box.padding = 0.4,
        point.padding = 0.3,
        max.overlaps = 50,
        force = 1.5,
        segment.color = "grey50",
        label.size = 0.2,
        show.legend = FALSE
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
  
  output$downloadVolcano <- downloadHandler(
    filename = function() {
      paste0("volcano_plot_", Sys.Date(), ".", tolower(input$download_format))
    },
    content = function(file) {
      p <- create_download_plot()
      
      plot_device <- switch(tolower(input$download_format),
                            "png" = "png",
                            "pdf" = "pdf",
                            "svg" = svglite::svglite,
                            "png") 
      
      ggsave(
        file,
        plot = p,
        device = plot_device,
        width = 8,
        height = 8, 
        units = "in",
        dpi = 300
      )
    }
  )
  
}