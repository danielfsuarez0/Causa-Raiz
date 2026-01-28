# app.R
library(shiny)
library(DT)
library(bslib)

# (Opcional) Para leer logo PNG en exportaciones base-R (si no está, no falla)
# install.packages("png")
# install.packages("grid")
# install.packages("base64enc")
# pero no es obligatorio; el HTML sí captura el logo desde www/logo.png
suppressWarnings({
  has_png <- requireNamespace("png", quietly = TRUE)
})

# ==========================
#  CONFIGURACIÓN DEL TEMA
# ==========================
tema_app <- bs_theme(
  version  = 5,
  bootswatch = "flatly",
  primary = "#F57C00",
  secondary = "#FFB74D",
  base_font = "Arial"
)

# ==========================
#  FUNCIONES AUXILIARES
# ==========================
`%||%` <- function(x, y) if (is.null(x)) y else x

# --- helpers texto ---
wrap_lines <- function(x, width = 80) {
  x <- x %||% ""
  x <- trimws(x)
  if (x == "") return(character(0))
  unlist(strwrap(x, width = width))
}

# --- logo raster (para plots base R) ---
get_logo_raster <- function(path = "www/logo.png") {
  if (!has_png) return(NULL)
  if (!file.exists(path)) return(NULL)
  img <- tryCatch(png::readPNG(path), error = function(e) NULL)
  img
}

draw_header_meta <- function(title, fecha, responsables, lugar = "", logo_img = NULL) {
  # Encabezado superior dentro del plot (base R)
  op <- par(xpd = NA)
  on.exit(par(op), add = TRUE)
  
  # Logo (si existe)
  if (!is.null(logo_img)) {
    # ubicar esquina superior izquierda
    rasterImage(logo_img, xleft = 0.02, ybottom = 0.90, xright = 0.14, ytop = 0.985)
  }
  
  # Título
  text(0.5, 0.97, labels = title, cex = 1.05, font = 2, col = "#F57C00")
  
  # Meta
  meta <- paste0(
    "Fecha: ", ifelse(fecha == "", "[Sin fecha]", fecha),
    "  |  Responsables: ", ifelse(responsables == "", "[Sin responsables]", responsables),
    ifelse(lugar == "", "", paste0("  |  Lugar/Acta: ", lugar))
  )
  meta_lines <- wrap_lines(meta, width = 120)
  text(0.5, 0.915, labels = paste(meta_lines, collapse = "\n"), cex = 0.65, col = "#5D4037")
}

# ==========================
#   PLOT: 5 PORQUÉS (profesional, sin sobreponer causa raíz)
# ==========================
plot_whys <- function(df, problem, root_text,
                      fecha = "", responsables = "", lugar = "",
                      logo_img = NULL) {
  par(family = "Arial")
  
  # Preparar texto envuelto
  prob_lines <- wrap_lines(problem, width = 85)
  root_lines <- wrap_lines(root_text, width = 95)
  
  n <- if (is.null(df) || nrow(df) == 0) 0 else nrow(df)
  
  # Espacios dinámicos
  header_space <- 5
  problem_space <- max(2, length(prob_lines)) + 1
  whys_space <- max(1, n) * 2.2
  root_space <- if (length(root_lines) > 0) (length(root_lines) + 2.5) else 0
  
  total_y <- header_space + problem_space + whys_space + root_space + 2
  
  par(mar = c(4, 1, 4, 1))
  plot(c(0, 1), c(0, total_y), type = "n", axes = FALSE, xlab = "", ylab = "")
  
  # Normalizar para dibujar header arriba con coordenadas 0-1
  # (se dibuja con base en coordenadas del plot, no device)
  # Usamos y en escala del plot:
  # Encabezado en zona superior
  usr <- par("usr")
  # Truco: mapear 0-1 a usr
  to_x <- function(u) usr[1] + u * (usr[2] - usr[1])
  to_y <- function(u) usr[3] + u * (usr[4] - usr[3])
  
  # Header
  op <- par(xpd = NA)
  on.exit(par(op), add = TRUE)
  
  if (!is.null(logo_img)) {
    rasterImage(logo_img, to_x(0.02), to_y(0.90), to_x(0.14), to_y(0.985))
  }
  text(to_x(0.5), to_y(0.97), "Análisis de 5 Porqués", cex = 1.05, font = 2, col = "#F57C00")
  
  meta <- paste0(
    "Fecha: ", ifelse(fecha == "", "[Sin fecha]", fecha),
    "  |  Responsables: ", ifelse(responsables == "", "[Sin responsables]", responsables),
    ifelse(lugar == "", "", paste0("  |  Lugar/Acta: ", lugar))
  )
  meta_lines <- wrap_lines(meta, width = 120)
  text(to_x(0.5), to_y(0.915), paste(meta_lines, collapse = "\n"), cex = 0.65, col = "#5D4037")
  
  # Bloque problema
  y_cursor <- total_y - (header_space + 1.5)
  text(0.06, y_cursor, "Desviación / problema:", adj = c(0, 0.5), cex = 0.75, font = 2, col = "#E65100")
  y_cursor <- y_cursor - 1.0
  if (length(prob_lines) == 0) prob_lines <- c("Desviación no especificada.")
  text(0.06, y_cursor, paste(prob_lines, collapse = "\n"), adj = c(0, 1), cex = 0.7, col = "#5D4037")
  y_cursor <- y_cursor - (length(prob_lines) + 1.2)
  
  # Porqués
  if (n == 0) {
    rect(0.06, y_cursor - 1.4, 0.94, y_cursor + 0.6, border = "#FFB74D")
    text(0.5, y_cursor - 0.4, "Sin información de 5 Porqués.", cex = 0.8, col = "grey40")
    y_cursor <- y_cursor - 2.0
  } else {
    for (i in seq_len(n)) {
      etiqueta <- paste0("¿Por qué ", i, "?")
      resp <- df$Respuesta[i] %||% ""
      resp_lines <- wrap_lines(ifelse(trimws(resp) == "", "[Sin respuesta]", resp), width = 90)
      
      # Caja
      box_h <- max(1.8, 0.75 + 0.55 * length(resp_lines))
      rect(0.22, y_cursor - box_h, 0.94, y_cursor, border = "#F57C00")
      text(0.06, y_cursor - box_h/2, etiqueta, adj = c(0, 0.5), cex = 0.75, font = 2, col = "#F57C00")
      text(0.24, y_cursor - 0.35, paste(resp_lines, collapse = "\n"), adj = c(0, 1), cex = 0.68, col = "#5D4037")
      
      y_cursor <- y_cursor - (box_h + 0.8)
    }
  }
  
  # Causa raíz final (abajo, con margen garantizado)
  if (length(root_lines) > 0) {
    rect(0.06, max(0.6, y_cursor - (length(root_lines) * 0.65 + 1.2)), 0.94, y_cursor, border = "#FFB74D")
    text(0.08, y_cursor - 0.35, "Causa raíz final:", adj = c(0, 1), cex = 0.75, font = 2, col = "#E65100")
    text(0.08, y_cursor - 0.95, paste(root_lines, collapse = "\n"), adj = c(0, 1), cex = 0.68, col = "#5D4037")
  }
}

# ==========================
#   PLOT: LLUVIA DE IDEAS (reporte tipo acta con wrap)
# ==========================
plot_brainstorm_report <- function(df, problem, root_text,
                                   fecha = "", responsables = "", lugar = "",
                                   logo_img = NULL) {
  par(family = "Arial")
  
  prob_lines <- wrap_lines(problem, width = 95)
  root_lines <- wrap_lines(root_text, width = 95)
  
  if (is.null(df) || nrow(df) == 0) {
    par(mar = c(4, 1, 4, 1))
    plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    # Header simple
    usr <- par("usr")
    to_x <- function(u) usr[1] + u * (usr[2] - usr[1])
    to_y <- function(u) usr[3] + u * (usr[4] - usr[3])
    if (!is.null(logo_img)) rasterImage(logo_img, to_x(0.02), to_y(0.80), to_x(0.14), to_y(0.97))
    text(to_x(0.5), to_y(0.95), "Lluvia de Ideas - Reporte", cex = 1.05, font = 2, col = "#F57C00")
    meta <- paste0("Fecha: ", ifelse(fecha == "", "[Sin fecha]", fecha),
                   "  |  Responsables: ", ifelse(responsables == "", "[Sin responsables]", responsables),
                   ifelse(lugar == "", "", paste0("  |  Lugar/Acta: ", lugar)))
    text(to_x(0.5), to_y(0.85), meta, cex = 0.65, col = "#5D4037")
    text(0.5, 0.5, "Sin ideas registradas.", col = "grey40", cex = 0.9)
    return(invisible(NULL))
  }
  
  # Ordenar por impacto desc, luego tipo
  df <- df[order(df$Impacto, decreasing = TRUE), , drop = FALSE]
  
  # Preparar filas con wrap (para altura dinámica)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    idea_lines <- wrap_lines(df$Idea[i], width = 70)
    list(
      tipo = df$Tipo[i],
      impacto = df$Impacto[i],
      idea_lines = if (length(idea_lines) == 0) c("[Sin idea]") else idea_lines
    )
  })
  
  # Calcular altura total aproximada
  header_space <- 5
  problem_space <- max(2, length(prob_lines)) + 1.5
  table_header_space <- 1.5
  rows_space <- sum(sapply(rows, function(r) max(1.6, 0.65 * length(r$idea_lines) + 1.0))) + 1
  root_space <- if (length(root_lines) > 0) (length(root_lines) * 0.7 + 2.0) else 0
  
  total_y <- header_space + problem_space + table_header_space + rows_space + root_space + 2
  
  par(mar = c(4, 1, 4, 1))
  plot(c(0, 1), c(0, total_y), type = "n", axes = FALSE, xlab = "", ylab = "")
  
  usr <- par("usr")
  to_x <- function(u) usr[1] + u * (usr[2] - usr[1])
  to_y <- function(u) usr[3] + u * (usr[4] - usr[3])
  
  # Header
  if (!is.null(logo_img)) rasterImage(logo_img, to_x(0.02), to_y(0.90), to_x(0.14), to_y(0.985))
  text(to_x(0.5), to_y(0.97), "Lluvia de Ideas - Reporte de Priorización", cex = 1.05, font = 2, col = "#F57C00")
  meta <- paste0("Fecha: ", ifelse(fecha == "", "[Sin fecha]", fecha),
                 "  |  Responsables: ", ifelse(responsables == "", "[Sin responsables]", responsables),
                 ifelse(lugar == "", "", paste0("  |  Lugar/Acta: ", lugar)))
  meta_lines <- wrap_lines(meta, width = 120)
  text(to_x(0.5), to_y(0.915), paste(meta_lines, collapse = "\n"), cex = 0.65, col = "#5D4037")
  
  # Problema
  y_cursor <- total_y - (header_space + 1.5)
  text(0.06, y_cursor, "Desviación / problema:", adj = c(0, 0.5), cex = 0.75, font = 2, col = "#E65100")
  y_cursor <- y_cursor - 1.0
  if (length(prob_lines) == 0) prob_lines <- c("Desviación no especificada.")
  text(0.06, y_cursor, paste(prob_lines, collapse = "\n"), adj = c(0, 1), cex = 0.7, col = "#5D4037")
  y_cursor <- y_cursor - (length(prob_lines) + 1.2)
  
  # Encabezado de tabla
  rect(0.06, y_cursor - 1.2, 0.94, y_cursor, border = "#F57C00")
  text(0.08, y_cursor - 0.35, "Tipo", adj = c(0, 1), cex = 0.72, font = 2, col = "#F57C00")
  text(0.22, y_cursor - 0.35, "Impacto", adj = c(0, 1), cex = 0.72, font = 2, col = "#F57C00")
  text(0.34, y_cursor - 0.35, "Idea / causa (texto ajustado)", adj = c(0, 1), cex = 0.72, font = 2, col = "#F57C00")
  y_cursor <- y_cursor - 1.8
  
  # Filas
  for (r in rows) {
    h <- max(1.6, 0.65 * length(r$idea_lines) + 1.0)
    rect(0.06, y_cursor - h, 0.94, y_cursor, border = "#FFB74D")
    text(0.08, y_cursor - 0.35, r$tipo, adj = c(0, 1), cex = 0.65, col = "#5D4037")
    text(0.22, y_cursor - 0.35, as.character(r$impacto), adj = c(0, 1), cex = 0.65, col = "#5D4037")
    text(0.34, y_cursor - 0.35, paste(r$idea_lines, collapse = "\n"), adj = c(0, 1), cex = 0.65, col = "#5D4037")
    y_cursor <- y_cursor - (h + 0.35)
  }
  
  # Causa raíz final
  if (length(root_lines) > 0) {
    rect(0.06, max(0.6, y_cursor - (length(root_lines) * 0.7 + 1.4)), 0.94, y_cursor, border = "#FFB74D")
    text(0.08, y_cursor - 0.35, "Causa raíz final:", adj = c(0, 1), cex = 0.75, font = 2, col = "#E65100")
    text(0.08, y_cursor - 0.95, paste(root_lines, collapse = "\n"), adj = c(0, 1), cex = 0.68, col = "#5D4037")
  }
}

# ==========================
#          UI
# ==========================
ui <- fluidPage(
  theme = tema_app,
  tags$head(
    tags$title("Análisis de Causa Raíz"),
    # html2canvas para screenshot del diagrama Ishikawa
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('capture-fishbone', function(message) {
        var target = document.getElementById('fishbone-container');
        if (!target) return;
        html2canvas(target).then(function(canvas) {
          var link = document.createElement('a');
          link.download = message.filename || 'diagrama_causa_efecto.png';
          link.href = canvas.toDataURL();
          link.click();
        });
      });
    ")),
    tags$style(HTML("
      body {
        background-color: #FFF7F0;
        font-family: Arial, sans-serif;
      }
      .app-header {
        padding: 20px 0 10px 0;
        border-bottom: 1px solid #FFECB3;
        margin-bottom: 10px;
      }
      .app-title {
        font-weight: 700;
        font-size: 26px;
        color: #F57C00;
      }
      .app-subtitle {
        color: #8D6E63;
        font-size: 14px;
      }
      .app-card {
        background: #ffffff;
        border-radius: 14px;
        padding: 18px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        margin-bottom: 18px;
        border: 1px solid #FFE0B2;
      }
      .section-title {
        font-weight: 600;
        font-size: 18px;
        margin-bottom: 8px;
        color: #5D4037;
      }
      .section-help {
        font-size: 13px;
        color: #8D6E63;
        margin-bottom: 10px;
      }
      .method-pill {
        display: inline-block;
        background: #FFE0B2;
        color: #E65100;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 10px;
      }
      .footer-note {
        font-size: 11px;
        color: #BCAAA4;
        margin-top: 10px;
        text-align: right;
      }
      .btn-primary {
        border-radius: 999px;
        padding-left: 18px;
        padding-right: 18px;
        background-color: #F57C00;
        border-color: #F57C00;
      }
      .btn-primary:hover {
        background-color: #E65100;
        border-color: #E65100;
      }

      /* ======= Bloque Datos de la sesión ======= */
      .session-grid {
        display: grid;
        grid-template-columns: 160px 1fr;
        gap: 10px 12px;
        align-items: center;
      }
      .session-label {
        font-size: 12px;
        font-weight: 700;
        color: #E65100;
      }
      .logo-row {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .logo-box {
        width: 140px;
        height: 44px;
        border: 1px dashed #FFCC80;
        border-radius: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: #FFF8E1;
        overflow: hidden;
      }
      .logo-box img {
        max-height: 40px;
        max-width: 130px;
      }
      .logo-hint {
        font-size: 12px;
        color: #8D6E63;
      }

      /* ======= ISHIKAWA HTML (lo que se captura) ======= */
      #fishbone-container {
        background: #ffffff;
        border-radius: 14px;
        padding: 16px 24px 20px;
        border: 1px solid #FFE0B2;
        min-height: 420px;
      }
      .fb-title {
        text-align: center;
        font-weight: 700;
        color: #F57C00;
        margin-bottom: 6px;
        font-size: 18px;
      }
      .fb-meta {
        display: flex;
        justify-content: space-between;
        gap: 12px;
        align-items: center;
        margin-bottom: 8px;
      }
      .fb-meta-left {
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .fb-logo {
        width: 120px;
        height: 38px;
        border: 1px dashed #FFCC80;
        border-radius: 10px;
        background: #FFF8E1;
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
      }
      .fb-logo img { max-height: 34px; max-width: 110px; }
      .fb-meta-text {
        font-size: 12px;
        color: #5D4037;
        white-space: pre-wrap;
      }

      .fb-main {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .fb-axis-head-row {
        display: grid;
        grid-template-columns: 1fr auto;
        align-items: center;
        column-gap: 8px;
        margin: 8px 40px 4px 20px;
        position: relative;
      }
      .fb-axis {
        height: 2px;
        background: #000000;
        position: relative;
        margin-right: 60px;
      }
      .fb-axis::after {
        content: '';
        position: absolute;
        right: -12px;
        top: -4px;
        border-left: 12px solid #000000;
        border-top: 6px solid transparent;
        border-bottom: 6px solid transparent;
      }
      .fb-head {
        border-radius: 10px;
        border: 1px solid #FFCC80;
        background: #FFF8E1;
        padding: 6px 10px;
        min-width: 210px;
        max-width: 320px;
      }
      .fb-problem-label {
        font-weight: 700;
        font-size: 12px;
        color: #E65100;
        margin-bottom: 2px;
      }
      .fb-problem-text {
        font-size: 12px;
        color: #5D4037;
        white-space: pre-wrap;
      }

      .fb-branches-top,
      .fb-branches-bottom {
        display: flex;
        justify-content: space-between;
        gap: 16px;
        margin: 0 80px;
      }
      .fb-branch {
        flex: 1;
        font-size: 12px;
        position: relative;
        min-height: 70px;
      }
      .fb-branch-line {
        position: absolute;
        width: 55px;
        height: 2px;
        background: #000000;
        left: 0;
      }
      .fb-branch-top .fb-branch-line {
        bottom: -2px;
        transform-origin: left center;
        transform: rotate(-25deg);
      }
      .fb-branch-bottom .fb-branch-line {
        top: -2px;
        transform-origin: left center;
        transform: rotate(25deg);
      }
      .fb-branch-box {
        margin-left: 24px;
        background: #fff8e1;
        border-radius: 10px;
        border: 1px dashed #FFCC80;
        padding: 6px 8px;
      }
      .fb-category {
        font-weight: 700;
        color: #F57C00;
        margin-bottom: 3px;
        font-size: 12px;
      }
      .fb-branch-box ul {
        padding-left: 16px;
        margin: 0;
      }
      .fb-branch-box li { margin-bottom: 2px; }

      .fb-root-title {
        margin-top: 10px;
        font-weight: 700;
        color: #E65100;
        font-size: 13px;
      }
      .fb-root-text {
        font-size: 13px;
        color: #5D4037;
        white-space: pre-wrap;
      }

      /* Árbol de decisión */
      .decision-tree {
        border-left: 3px solid #FFCC80;
        padding-left: 14px;
        margin-top: 4px;
        display: flex;
        flex-direction: column;
        gap: 6px;
      }
      .dt-node {
        background: #FFF8E1;
        border-radius: 12px;
        border: 1px solid #FFCC80;
        padding: 10px 12px;
        font-size: 12px;
      }
      .dt-step {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #E65100;
        font-weight: 700;
        margin-bottom: 4px;
      }
      .dt-question {
        font-size: 13px;
        font-weight: 600;
        color: #5D4037;
        margin-bottom: 6px;
      }
      .dt-answers { display: flex; flex-direction: column; gap: 4px; }
      .dt-answer { display: flex; gap: 6px; align-items: flex-start; font-size: 12px; color: #5D4037; }
      .dt-badge { border-radius: 999px; padding: 2px 8px; font-size: 11px; font-weight: 600; white-space: nowrap; }
      .dt-yes { background: #FFE0B2; color: #E65100; }
      .dt-no { background: #FFF3E0; color: #6D4C41; }
      .dt-arrow { font-size: 12px; color: #FFB74D; margin: 2px 0 4px 4px; }
      .dt-note { font-size: 11px; color: #8D6E63; margin-top: 2px; }
    "))
  ),
  
  # Encabezado
  div(
    class = "app-header",
    fluidRow(
      column(
        width = 8,
        span("Módulo de Análisis de Causa Raíz", class = "app-title"),
        br(),
        span("5 Porqués · Diagrama de Causa–Efecto · Lluvia de ideas",
             class = "app-subtitle")
      )
    )
  ),
  
  # Datos globales (aparecen en todas las pestañas)
  div(
    class = "app-card",
    span("Datos de la sesión", class = "section-title"),
    p(class = "section-help",
      "Estos datos se incluirán en los diagramas e imágenes descargadas."),

    br(),
    div(
      class = "session-grid",
      div(class = "session-label", "Fecha"),
      dateInput("session_date", label = NULL, value = Sys.Date(), format = "yyyy-mm-dd"),
      div(class = "session-label", "Responsables (nombres)"),
      textInput("session_responsables", label = NULL,
                placeholder = "..."),
      div(class = "session-label", "Lugar / Acta (opcional)"),
      textInput("session_lugar", label = NULL, placeholder = "Microsoft teams")
    )
  ),
  
  # Tabs
  tabsetPanel(
    id = "metodo",
    type = "tabs",
    
    # ---------------- INICIO ----------------
    tabPanel(
      "Inicio",
      br(),
      div(class = "app-card",
          span("Bienvenido/a", class = "section-title"),
          p(class = "section-help",
            "Esta aplicación permite estructurar análisis de causa raíz para trabajos no conformes, desviaciones, hallazgos de auditoría o incidentes, ",
            "utilizando tres metodologías: 5 Porqués, Diagrama de Causa–Efecto e ideación libre (lluvia de ideas).")
      ),
      div(
        class = "app-card",
        span("¿Qué metodología de análisis de causa raíz usar?", class = "section-title"),
        p(class = "section-help",
          "Use este árbol de decisión como guía rápida para elegir el método según el tipo de problema."),
        HTML('
          <div class="decision-tree">
            <div class="dt-node">
              <div class="dt-step">Paso 1</div>
              <div class="dt-question">¿El problema es puntual, acotado y está claramente definido?</div>
              <div class="dt-answers">
                <div class="dt-answer">
                  <span class="dt-badge dt-yes">Sí</span>
                  <span>Use <strong>5 Porqués</strong> para profundizar en una sola cadena causal y llegar a la causa raíz del evento puntual.</span>
                </div>
                <div class="dt-answer">
                  <span class="dt-badge dt-no">No</span>
                  <span>Continúe al paso 2.</span>
                </div>
              </div>
            </div>

            <div class="dt-arrow">▼</div>

            <div class="dt-node">
              <div class="dt-step">Paso 2</div>
              <div class="dt-question">¿Hay múltiples factores (método, personal, equipos, entorno, control, gestión)?</div>
              <div class="dt-answers">
                <div class="dt-answer">
                  <span class="dt-badge dt-yes">Sí</span>
                  <span>Use el <strong>Diagrama de Causa–Efecto (Ishikawa)</strong> para organizar las causas por categorías y visualizar el sistema completo.</span>
                </div>
                <div class="dt-answer">
                  <span class="dt-badge dt-no">No / No estoy seguro</span>
                  <span>Vaya al paso 3 para explorar ideas con el equipo antes de seleccionar el método principal.</span>
                </div>
              </div>
            </div>

            <div class="dt-arrow">▼</div>

            <div class="dt-node">
              <div class="dt-step">Paso 3</div>
              <div class="dt-question">¿Necesita explorar ideas con varias personas o el problema aún es difuso?</div>
              <div class="dt-answers">
                <div class="dt-answer">
                  <span class="dt-badge dt-yes">Sí</span>
                  <span>Comience con <strong>Lluvia de ideas</strong> para listar posibles causas y priorizarlas. Luego consolide con 5 Porqués o Ishikawa.</span>
                </div>
                <div class="dt-answer">
                  <span class="dt-badge dt-no">No</span>
                  <span>Elija: <strong>5 Porqués</strong> si es puntual, o <strong>Ishikawa</strong> si es sistémico y multifactorial.</span>
                </div>
              </div>
            </div>

            <div class="dt-note">
              Recomendación: puede combinar métodos. Por ejemplo: <strong>Lluvia de ideas</strong> → <strong>Ishikawa</strong> → <strong>5 Porqués</strong> sobre la causa crítica.
            </div>
          </div>
        ')
      ),
      div(class = "footer-note",
          "Tipografía Arial · Paleta anaranjada · Exportación con logo y metadatos")
    ),
    
    # ---------------- 5 PORQUÉS ----------------
    tabPanel(
      "5 Porqués",
      br(),
      fluidRow(
        column(
          width = 6,
          div(
            class = "app-card",
            span("5 Porqués", class = "method-pill"),
            span("Definición de la desviación", class = "section-title"),
            p(class = "section-help", "Describa claramente la desviación o problema a analizar."),
            textAreaInput("whys_problem", "Desviación / problema:",
                          placeholder = "Ej: Se cerró un trabajo no conforme sin evidencia del análisis de impacto sobre resultados previos...",
                          rows = 4),
            numericInput("n_whys", "Número de porqués", value = 5, min = 1, max = 10, step = 1),
            p(class = "section-help", "Para cada nivel, registre la respuesta a “¿Por qué ocurrió?”.")
          )
        ),
        column(
          width = 6,
          div(
            class = "app-card",
            span("Desglose", class = "section-title"),
            uiOutput("whys_inputs"),
            br(),
            span("Causa raíz final", class = "section-title"),
            p(class = "section-help", "Redacte la causa raíz definitiva (se incluirá en la imagen descargada)."),
            textAreaInput("whys_root", label = NULL,
                          placeholder = "Ej: La falta de análisis de impacto se debe a...",
                          rows = 4),
            br(),
            downloadButton("download_whys_img", "Descargar imagen (5 Porqués)")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(class = "app-card",
              span("Vista previa del reporte", class = "section-title"),
              plotOutput("whys_plot", height = "720px"))
        )
      )
    ),
    
    # ---------------- CAUSA–EFECTO ----------------
    tabPanel(
      "Causa–Efecto",
      br(),
      fluidRow(
        column(
          width = 4,
          div(
            class = "app-card",
            span("Diagrama de Causa–Efecto", class = "method-pill"),
            span("Definición de la desviación", class = "section-title"),
            textAreaInput("fish_problem", "Desviación / problema:",
                          placeholder = "Ej: No se realizó el análisis de riesgo para definir la frecuencia de participación en ensayos de aptitud...",
                          rows = 4),
            span("Causas por categoría", class = "section-title"),
            p(class = "section-help", "Escriba una causa por línea (se verá en viñetas)."),
            textAreaInput("fish_metodo", "Método", rows = 3,
                          placeholder = "Ej: Procedimiento desactualizado\nNo incluye análisis de riesgo 7.3.1"),
            textAreaInput("fish_personal", "Personal", rows = 3,
                          placeholder = "Ej: Desconocimiento del requisito\nFalta de apropiación del CEA-3.0-04"),
            textAreaInput("fish_equipos", "Equipos", rows = 3,
                          placeholder = "Ej: Falta de software para seguimiento"),
            textAreaInput("fish_materiales", "Materiales / Insumos", rows = 3,
                          placeholder = "Ej: No aplica / No afecta la desviación"),
            textAreaInput("fish_entorno", "Entorno / Ambiente", rows = 3,
                          placeholder = "Ej: Cambios normativos recientes\nAlta carga de auditorías"),
            textAreaInput("fish_medicion", "Medición / Control", rows = 3,
                          placeholder = "Ej: Falta de revisión anual de matrices de riesgo"),
            span("Causa raíz final", class = "section-title"),
            textAreaInput("fish_root_text", label = NULL, rows = 4,
                          placeholder = "Ej: La reincidencia se debe a..."),
            br(),
            actionButton("download_fish_img", "Descargar imagen del diagrama")
          )
        ),
        column(
          width = 8,
          div(
            class = "app-card",
            span("Vista del diagrama (captura)", class = "section-title"),
            p(class = "section-help",
              "Este bloque es el que se captura como imagen. Incluye logo + fecha + responsables + causa raíz."),
            uiOutput("fishbone_html")
          )
        )
      )
    ),
    
    # ---------------- LLUVIA DE IDEAS ----------------
    tabPanel(
      "Lluvia de ideas",
      br(),
      fluidRow(
        column(
          width = 4,
          div(
            class = "app-card",
            span("Lluvia de ideas", class = "method-pill"),
            span("Definición de la desviación", class = "section-title"),
            textAreaInput("brain_problem", "Desviación / problema:",
                          placeholder = "Ej: Alta reincidencia de no conformidades relacionadas con trabajos no conformes...",
                          rows = 4),
            hr(),
            span("Registro de ideas / causas", class = "section-title"),
            textInput("idea_text", "Idea / causa:",
                      placeholder = "Ej: Falta de entrenamiento específico en QA-P-07"),
            selectInput("idea_tipo", "Tipo de causa:",
                        choices = c("Procedimiento", "Personal", "Equipo", "Material / Insumo",
                                    "Entorno", "Gestión / Organización", "Otro"),
                        selected = "Procedimiento"),
            sliderInput("idea_impacto", "Impacto percibido (1-5):", min = 1, max = 5, value = 3, step = 1),
            actionButton("add_idea", "Agregar idea"),
            actionButton("clear_ideas", "Limpiar lista"),
            br(), br(),
            span("Causa raíz final", class = "section-title"),
            textAreaInput("brain_root_text", label = NULL, rows = 4,
                          placeholder = "Ej: La reincidencia se debe a..."),
            br(),
            downloadButton("download_brain_img", "Descargar reporte (Lluvia de ideas)")
          )
        ),
        column(
          width = 8,
          div(
            class = "app-card",
            span("Ideas registradas", class = "section-title"),
            p(class = "section-help",
              "Esta tabla sirve para depurar y priorizar. El reporte descargado quedará en formato acta (con ajuste de texto)."),
            DTOutput("ideas_table")
          ),
          br(),
          div(
            class = "app-card",
            span("Vista previa del reporte descargable", class = "section-title"),
            plotOutput("brain_plot", height = "760px")
          )
        )
      )
    )
  )
)

# ==========================
#        SERVER
# ==========================
server <- function(input, output, session) {
  
  # Logo (para plots)
  logo_img <- reactive({
    get_logo_raster("www/logo.png")
  })
  
  # ---------- 5 PORQUÉS ----------
  output$whys_inputs <- renderUI({
    n <- input$n_whys %||% 5
    if (n < 1) n <- 1
    lapply(seq_len(n), function(i) {
      textAreaInput(
        inputId = paste0("why_", i),
        label   = paste0("Respuesta al porqué ", i, ":"),
        placeholder = "Describa la causa identificada en este nivel...",
        rows = 2
      )
    })
  })
  
  whys_data <- reactive({
    n <- input$n_whys %||% 0
    if (n < 1) return(NULL)
    respuestas <- vapply(seq_len(n), function(i) {
      input[[paste0("why_", i)]] %||% ""
    }, character(1))
    data.frame(
      Nivel_Porque = seq_len(n),
      Respuesta    = respuestas,
      stringsAsFactors = FALSE
    )
  })
  
  output$whys_plot <- renderPlot({
    df <- whys_data()
    plot_whys(
      df = df,
      problem = input$whys_problem %||% "",
      root_text = input$whys_root %||% "",
      fecha = as.character(input$session_date %||% ""),
      responsables = input$session_responsables %||% "",
      lugar = input$session_lugar %||% "",
      logo_img = logo_img()
    )
  })
  
  output$download_whys_img <- downloadHandler(
    filename = function() paste0("analisis_5_porque_", Sys.Date(), ".png"),
    content = function(file) {
      df <- whys_data()
      png(file, width = 1600, height = 950, res = 140)
      plot_whys(
        df = df,
        problem = input$whys_problem %||% "",
        root_text = input$whys_root %||% "",
        fecha = as.character(input$session_date %||% ""),
        responsables = input$session_responsables %||% "",
        lugar = input$session_lugar %||% "",
        logo_img = logo_img()
      )
      dev.off()
    }
  )
  
  # ---------- ISHIKAWA (HTML) ----------
  fishbone_df <- reactive({
    categorias <- c("Método", "Personal", "Equipos",
                    "Materiales / Insumos", "Entorno / Ambiente", "Medición / Control")
    ids <- c("fish_metodo", "fish_personal", "fish_equipos",
             "fish_materiales", "fish_entorno", "fish_medicion")
    
    causas_list <- lapply(seq_along(ids), function(i) {
      txt <- input[[ids[i]]] %||% ""
      txt <- trimws(txt)
      if (txt == "") return(NULL)
      lineas <- unlist(strsplit(txt, "\n", fixed = TRUE))
      lineas <- trimws(lineas)
      lineas <- lineas[lineas != ""]
      if (length(lineas) == 0) return(NULL)
      data.frame(
        Categoria = categorias[i],
        Causa = lineas,
        stringsAsFactors = FALSE
      )
    })
    
    out <- do.call(rbind, causas_list)
    if (is.null(out)) out <- data.frame(Categoria = character(), Causa = character())
    out
  })
  
  output$fishbone_html <- renderUI({
    df <- fishbone_df()
    problem <- input$fish_problem %||% ""
    root <- input$fish_root_text %||% ""
    
    # Categorías alternadas arriba/abajo
    cats <- unique(df$Categoria)
    if (length(cats) == 0) {
      top_cats <- character(0)
      bottom_cats <- character(0)
    } else {
      top_cats <- cats[seq(1, length(cats), by = 2)]
      bottom_cats <- setdiff(cats, top_cats)
    }
    
    make_branch <- function(cat, posicion) {
      causas <- df$Causa[df$Categoria == cat]
      if (length(causas) == 0) return(NULL)
      clase_pos <- if (posicion == "top") "fb-branch fb-branch-top" else "fb-branch fb-branch-bottom"
      tags$div(
        class = clase_pos,
        tags$div(class = "fb-branch-line"),
        tags$div(
          class = "fb-branch-box",
          tags$div(class = "fb-category", cat),
          tags$ul(lapply(causas, function(ca) tags$li(ca)))
        )
      )
    }
    
    meta <- paste0(
      "Fecha: ", as.character(input$session_date %||% ""),
      "  |  Responsables: ", input$session_responsables %||% "",
      ifelse((input$session_lugar %||% "") == "", "", paste0("\nLugar/Acta: ", input$session_lugar %||% ""))
    )
    
    tags$div(
      id = "fishbone-container",
      tags$div(class = "fb-title", "Diagrama de Causa–Efecto (Ishikawa)"),
      tags$div(
        class = "fb-meta",
        tags$div(
          class = "fb-meta-left",
          tags$div(class = "fb-logo",
                   tags$img(src = "logo.png", alt = "Logo", onerror = "this.style.display='none';")
          ),
          tags$div(class = "fb-meta-text", meta)
        ),
        tags$div(class = "fb-meta-text", "")
      ),
      tags$div(
        class = "fb-main",
        tags$div(class = "fb-branches-top", lapply(top_cats, function(cat) make_branch(cat, "top"))),
        tags$div(
          class = "fb-axis-head-row",
          tags$div(class = "fb-axis"),
          tags$div(
            class = "fb-head",
            tags$div(class = "fb-problem-label", "Desviación / efecto"),
            tags$div(class = "fb-problem-text", ifelse(trimws(problem) == "", "Desviación no especificada.", problem))
          )
        ),
        tags$div(class = "fb-branches-bottom", lapply(bottom_cats, function(cat) make_branch(cat, "bottom")))
      ),
      tags$div(class = "fb-root-title", "Causa raíz"),
      tags$div(class = "fb-root-text", ifelse(trimws(root) == "", "Sin causa raíz registrada.", root))
    )
  })
  
  observeEvent(input$download_fish_img, {
    session$sendCustomMessage(
      "capture-fishbone",
      list(filename = paste0("diagrama_causa_efecto_", Sys.Date(), ".png"))
    )
  })
  
  # ---------- LLUVIA DE IDEAS ----------
  ideas <- reactiveVal(
    data.frame(
      Idea = character(),
      Tipo = character(),
      Impacto = integer(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$add_idea, {
    req(input$idea_text)
    df <- ideas()
    nueva <- data.frame(
      Idea = input$idea_text %||% "",
      Tipo = input$idea_tipo %||% "Otro",
      Impacto = as.integer(input$idea_impacto %||% 3),
      stringsAsFactors = FALSE
    )
    ideas(rbind(df, nueva))
    updateTextInput(session, "idea_text", value = "")
  })
  
  observeEvent(input$clear_ideas, {
    ideas(data.frame(Idea = character(), Tipo = character(), Impacto = integer(), stringsAsFactors = FALSE))
  })
  
  output$ideas_table <- renderDT({
    df <- ideas()
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        language = list(url = "https://cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json"),
        columnDefs = list(list(targets = 2, className = "dt-center"))
      )
    )
  })
  
  output$brain_plot <- renderPlot({
    plot_brainstorm_report(
      df = ideas(),
      problem = input$brain_problem %||% "",
      root_text = input$brain_root_text %||% "",
      fecha = as.character(input$session_date %||% ""),
      responsables = input$session_responsables %||% "",
      lugar = input$session_lugar %||% "",
      logo_img = logo_img()
    )
  })
  
  output$download_brain_img <- downloadHandler(
    filename = function() paste0("reporte_lluvia_ideas_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1700, height = 1100, res = 140)
      plot_brainstorm_report(
        df = ideas(),
        problem = input$brain_problem %||% "",
        root_text = input$brain_root_text %||% "",
        fecha = as.character(input$session_date %||% ""),
        responsables = input$session_responsables %||% "",
        lugar = input$session_lugar %||% "",
        logo_img = logo_img()
      )
      dev.off()
    }
  )
}

shinyApp(ui, server)

