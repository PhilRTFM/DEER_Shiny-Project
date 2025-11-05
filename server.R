# ========================= server.R (Fixed geom_label_repel) =========================
# Adds explicit x/y mapping for geom_label_repel and removes duplicate fill scale warnings

library(ggplot2)
library(shiny)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(ggrepel)

primary_color   <- "#fdcb6e"
secondary_color <- "#ffeaa7"
accent_color    <- "#2d3436"

server <- function(input, output, session) {
  dataset_reactive <- reactiveVal(NULL)
  selected_genes <- reactiveVal(character(0))
  
  check_dataset_integrity <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
      return(list(valid = FALSE, color = "red", message = "❌ Fichier vide ou non chargé."))
    
    if (ncol(df) == 1 && grepl(";", df[1, 1]))
      df <- tryCatch(read.csv(input$file$datapath, sep = ";", stringsAsFactors = FALSE), error = function(e) NULL)
    
    required_cols <- c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0)
      return(list(valid = FALSE, color = "red", message = paste0("❌ Colonnes manquantes : ", paste(missing_cols, collapse = ", "))))
    
    df$log2FC <- suppressWarnings(as.numeric(gsub(",", ".", df$log2FC)))
    df$pval   <- suppressWarnings(as.numeric(gsub(",", ".", df$pval)))
    df$padj   <- suppressWarnings(as.numeric(gsub(",", ".", df$padj)))
    
    if (any(is.na(df$log2FC)) || any(is.na(df$pval)) || any(is.na(df$padj)))
      return(list(valid = FALSE, color = "red", message = "❌ Les colonnes log2FC, pval ou padj contiennent des valeurs non numériques."))
    
    list(valid = TRUE, color = "green", message = "✅ Jeu de données conforme.", df = df)
  }
  
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read.csv(input$file$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    check <- check_dataset_integrity(df)
    
    output$dataset_status_sidebar <- renderUI({
      tags$p(check$message, style = paste0("color:", check$color, "; font-weight:bold;"))
    })
    
    if (check$valid) dataset_reactive(check$df) else dataset_reactive(NULL)
  })
  
  filtered_data <- reactive({
    df <- dataset_reactive(); req(df)
    df$overexpressed  <- df$log2FC >= input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    df$underexpressed <- df$log2FC <= -input$fc_thresh & -log10(df$padj) >= input$pval_thresh
    df$significance <- ifelse(df$overexpressed, "Over", ifelse(df$underexpressed, "Under", "NS"))
    df
  })
  
  output$volcano_plot <- renderPlot({
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
        "\nlog2FC = ", round(df_selected$log2FC, 2),
        "; padj = ", signif(df_selected$padj, 3)
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
    
    p +
      labs(title = input$plot_title, x = "log2(Fold Change)", y = "-log10(P-adj)") +
      coord_cartesian(xlim = c(-3, 3)) +
      theme_bw(base_size = 13) +
      theme(
        aspect.ratio = 1,
        legend.position = "right",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
      )
  })
  
  output$table_volcano <- renderDT({
    df <- filtered_data(); req(df)
    df <- df[, c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj", "significance")]
    datatable(df, selection = list(mode = "multiple"), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  observeEvent(input$table_volcano_rows_selected, {
    df <- filtered_data(); req(df)
    selected_rows <- input$table_volcano_rows_selected
    if (length(selected_rows) > 0) {
      selected_genes(df[selected_rows, "GeneName"])
    } else {
      selected_genes(character(0))
    }
  })
  
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
        "<strong>Points significatifs</strong><br>", n_sig, " (", pct_total, "%)<br><br>",
        "<strong>Points sur-exprimés</strong><br>", n_over, " (", pct_over, "%)<br><br>",
        "<strong>Points sous-exprimés</strong><br>", n_under, " (", pct_under, "%)"
      )),
      value = NULL,
      icon = icon("chart-area"),
      color = if (n_sig > 0) "green" else "yellow",
      fill = TRUE
    )
  })
}
