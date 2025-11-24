# app.R
library(shiny)
library(DT)
library(bslib)
library(httr)
library(jsonlite)

# ==========================
#  CONFIGURACIÓN DEL TEMA
# ==========================
tema_app <- bs_theme(
  version  = 5,
  bootswatch = "flatly",
  primary = "#F57C00",     # naranja principal
  secondary = "#FFB74D",   # naranja claro
  base_font = "Arial"
)

# ==========================
#  FUNCIONES AUXILIARES
# ==========================
# Operador helper: x %||% y  ->  si x es NULL devuelve y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# --- Llamada a OpenAI / ChatGPT ---
generar_causa_ia <- function(texto_contexto, metodo) {
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (api_key == "") {
    return("⚠ No se encontró la variable de entorno OPENAI_API_KEY.\n\nConfigure su clave antes de usar la integración con IA.")
  }
  
  prompt_usuario <- paste0(
    "Eres un experto en sistemas de gestión de calidad (ISO 9001, ISO/IEC 17025 y similares) y análisis de causa raíz. ",
    "Con base en el siguiente análisis de causa raíz realizado con el método ",
    metodo, ", redacta AL MENOS 3 OPCIONES diferentes de redacción de causa raíz, ",
    "claras, técnicas y orientadas a sistemas de calidad en laboratorio.\n\n",
    "Texto del análisis:\n\n",
    texto_contexto,
    "\n\nResponde SOLO con las opciones de causa raíz numeradas o en viñetas."
  )
  
  body <- list(
    model = "gpt-4.1-mini",
    messages = list(
      list(
        role = "system",
        content = "Eres un asistente especializado en análisis de causa raíz y redacción técnica en sistemas de gestión de calidad."
      ),
      list(role = "user", content = prompt_usuario)
    )
  )
  
  resp <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(resp) || resp$status_code >= 300) {
    return("⚠ Ocurrió un error al conectarse con la API de OpenAI. Verifique su clave y la conexión a internet.")
  }
  
  parsed <- content(resp, as = "parsed", type = "application/json")
  texto <- parsed$choices[[1]]$message$content
  return(texto)
}

# --- Plot resumen 5 Porqués ---
plot_whys <- function(df) {
  par(mar = c(4, 1, 3, 1), family = "Arial")
  if (is.null(df) || nrow(df) == 0) {
    plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(0.5, 0.5, "Sin información de 5 Porqués", col = "grey40", cex = 0.9)
    return(invisible(NULL))
  }
  n <- nrow(df)
  
  plot(c(0, 1), c(0, n + 1), type = "n", axes = FALSE,
       xlab = "", ylab = "")
  mtext("Análisis de 5 Porqués", side = 3, line = 0.2,
        cex = 1, font = 2, col = "#F57C00")
  
  # problema (envuelto)
  problema <- ifelse(df$Desviacion[1] == "", "Desviación no especificada", df$Desviacion[1])
  problema_wrapped <- strwrap(problema, width = 60)
  text(0.5, n + 0.8,
       labels = paste(problema_wrapped, collapse = "\n"),
       cex = 0.7, font = 2)
  
  for (i in seq_len(n)) {
    y <- n + 0.5 - i
    text(0.1, y,
         labels = paste0("¿Por qué ", i, "?"),
         adj = c(0, 0.5), cex = 0.7, font = 2, col = "#F57C00")
    rect(0.25, y - 0.4, 0.95, y + 0.4,
         border = "#F57C00")
    resp <- ifelse(df$Respuesta[i] == "", "[Sin respuesta]", df$Respuesta[i])
    resp_wrapped <- strwrap(resp, width = 80)
    text(0.27, y,
         labels = paste(resp_wrapped, collapse = "\n"),
         adj = c(0, 0.5), cex = 0.65)
  }
}

# --- Plot resumen lluvia de ideas ---
plot_brainstorm <- function(df, problem) {
  par(mar = c(5, 10, 3, 2), family = "Arial")
  if (is.null(df) || nrow(df) == 0) {
    plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(0.5, 0.5, "Sin ideas registradas", col = "grey40", cex = 0.9)
    return(invisible(NULL))
  }
  
  # Ordenar por impacto
  df <- df[order(df$Impacto, decreasing = TRUE), ]
  
  barplot(df$Impacto,
          horiz = TRUE,
          names.arg = df$Idea,
          las = 1,
          cex.names = 0.6,
          col = "#FFCC80",
          border = "#F57C00",
          xlab = "Impacto (1-5)",
          xlim = c(0, 5.5))
  title(main = "Lluvia de Ideas - Priorización por impacto",
        col.main = "#F57C00", font.main = 2, cex.main = 1)
  mtext(ifelse(problem == "" | is.null(problem),
               "Desviación no especificada",
               problem),
        side = 3, line = 0.2, cex = 0.7)
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
      .fishbone-category {
        font-weight: 600;
        margin-bottom: 4px;
        color: #F57C00;
        text-transform: uppercase;
        font-size: 12px;
      }
      .fishbone-box {
        background: #fff8e1;
        border-radius: 12px;
        padding: 10px 12px;
        height: 100%;
        border: 1px dashed #FFB74D;
      }
      .fishbone-box ul {
        padding-left: 18px;
        margin-bottom: 0;
        font-size: 13px;
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
      .ai-box {
        background: #FFF3E0;
        border-radius: 10px;
        padding: 10px 12px;
        border: 1px solid #FFCC80;
        font-size: 13px;
      }
      .ai-title {
        font-weight: 600;
        color: #E65100;
        margin-bottom: 6px;
      }
      .ai-output {
        white-space: pre-wrap;
        font-family: Arial, sans-serif;
        font-size: 13px;
      }
      .shiny-output-error-validation {
        color: #E64A19;
        font-size: 12px;
      }

      /* === DIAGRAMA HTML DE ISHIKAWA (LO QUE SE CAPTURA) === */
      #fishbone-container {
        background: #ffffff;
        border-radius: 14px;
        padding: 20px 28px 24px;
        border: 1px solid #FFE0B2;
        min-height: 380px;
      }
      .fb-title {
        text-align: center;
        font-weight: 700;
        color: #F57C00;
        margin-bottom: 10px;
        font-size: 18px;
      }
      .fb-problem-label {
        font-weight: 600;
        font-size: 13px;
        color: #E65100;
        margin-bottom: 4px;
      }
      .fb-problem-text {
        font-size: 13px;
        color: #5D4037;
        margin-bottom: 14px;
        white-space: pre-wrap;
      }
      .fb-axis-wrapper {
        position: relative;
        margin: 40px 70px;
      }
      .fb-axis {
        height: 2px;
        background: #000000;
        position: relative;
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
      .fb-effect-label {
        position: absolute;
        right: -60px;
        top: -10px;
        font-weight: 700;
        color: #F57C00;
        font-size: 13px;
      }
      .fb-top-row,
      .fb-bottom-row {
        display: flex;
        justify-content: space-between;
        gap: 16px;
        margin: 0 70px;
      }
      .fb-top-row {
        margin-bottom: 18px;
      }
      .fb-bottom-row {
        margin-top: 18px;
      }
      .fb-branch {
        flex: 1;
        font-size: 12px;
      }
      .fb-category {
        font-weight: 700;
        color: #F57C00;
        margin-bottom: 4px;
      }
      .fb-branch ul {
        padding-left: 16px;
        margin: 0;
      }
      .fb-branch li {
        margin-bottom: 2px;
      }
      .fb-root-title {
        margin-top: 22px;
        font-weight: 600;
        color: #E65100;
        font-size: 13px;
      }
      .fb-root-text {
        font-size: 13px;
        color: #5D4037;
        white-space: pre-wrap;
      }
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
  
  # Navegación
  tabsetPanel(
    id = "metodo",
    type = "tabs",
    
    # ---------------- INICIO ----------------
    tabPanel("Inicio",
             br(),
             div(class = "app-card",
                 span("Bienvenido/a", class = "section-title"),
                 p(class = "section-help",
                   "Esta aplicación le permite estructurar el análisis de causa raíz ",
                   "para trabajos no conformes, desviaciones, hallazgos de auditoría o incidentes, ",
                   "utilizando tres metodologías: 5 Porqués, Diagrama de Causa–Efecto e ",
                   "ideación libre (lluvia de ideas). Además, puede apoyarse en ChatGPT ",
                   "para redactar la causa raíz con base en lo diligenciado.")
             ),
             fluidRow(
               column(
                 width = 4,
                 div(class = "app-card",
                     span("5 Porqués", class = "section-title"),
                     p(class = "section-help",
                       "Profundice en el origen del problema preguntando “¿por qué?” de forma ",
                       "iterativa hasta llegar a la causa raíz."),
                     tags$ul(
                       tags$li("Adecuado para desviaciones puntuales."),
                       tags$li("Fácil de explicar en comités y actas.")
                     )
                 )
               ),
               column(
                 width = 4,
                 div(class = "app-card",
                     span("Diagrama de Causa–Efecto", class = "section-title"),
                     p(class = "section-help",
                       "Organice causas por categorías (Método, Personal, Equipos, etc.) ",
                       "en un diagrama tipo espina de pescado (Ishikawa)."),
                     tags$ul(
                       tags$li("Útil cuando hay múltiples factores."),
                       tags$li("Visual e intuitivo para socializar causas.")
                     )
                 )
               ),
               column(
                 width = 4,
                 div(class = "app-card",
                     span("Lluvia de ideas", class = "section-title"),
                     p(class = "section-help",
                       "Registre libremente todas las posibles causas y valore su impacto."),
                     tags$ul(
                       tags$li("Ideal para sesiones grupales iniciales."),
                       tags$li("Permite priorizar causas antes de formalizar el análisis.")
                     )
                 )
               )
             ),
             div(class = "footer-note",
                 "Desarrollado en Shiny · Tipografía Arial · Paleta anaranjada")
    ),
    
    # ---------------- 5 PORQUÉS ----------------
    tabPanel("5 Porqués",
             br(),
             fluidRow(
               column(
                 width = 6,
                 div(class = "app-card",
                     span("5 Porqués", class = "method-pill"),
                     span("Definición de la desviación", class = "section-title"),
                     p(class = "section-help",
                       "Describa claramente la desviación o el problema que desea analizar."),
                     textAreaInput(
                       inputId = "whys_problem",
                       label   = "Desviación / problema:",
                       placeholder = "Ejemplo: Se cerró un trabajo no conforme sin evidencia del análisis de impacto sobre resultados previos...",
                       rows = 4
                     ),
                     numericInput(
                       inputId = "n_whys",
                       label   = "Número de veces que se preguntará “¿Por qué?”",
                       value   = 5,
                       min     = 1,
                       max     = 10,
                       step    = 1
                     ),
                     p(class = "section-help",
                       "Para cada nivel, registre la respuesta a la pregunta “¿Por qué ocurrió?”.")
                 )
               ),
               column(
                 width = 6,
                 div(class = "app-card",
                     span("Desglose de porqués", class = "section-title"),
                     uiOutput("whys_inputs"),
                     br(),
                     downloadButton("download_whys_img", "Descargar imagen del análisis")
                 )
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 div(class = "app-card",
                     span("Resumen del análisis de 5 Porqués", class = "section-title"),
                     plotOutput("whys_plot", height = "400px")
                 )
               ),
               column(
                 width = 6,
                 div(class = "app-card",
                     div(class = "ai-box",
                         span("Asistente IA - Redacción de causa raíz", class = "ai-title"),
                         p(
                           "Al presionar el botón, se enviará el contenido del análisis de 5 Porqués ",
                           "a ChatGPT para proponer varias opciones de redacción de la causa raíz."
                         ),
                         actionButton("whys_ai_btn", "Generar opciones con IA")
                     ),
                     br(),
                     div(class = "ai-output",
                         verbatimTextOutput("whys_ai_text")
                     )
                 )
               )
             )
    ),
    
    # ---------------- DIAGRAMA DE CAUSA–EFECTO ----------------
    tabPanel("Causa–Efecto",
             br(),
             fluidRow(
               column(
                 width = 4,
                 div(class = "app-card",
                     span("Diagrama de Causa–Efecto", class = "method-pill"),
                     span("Definición de la desviación", class = "section-title"),
                     p(class = "section-help",
                       "Describa el efecto o problema principal que se ubicaría en la ‘cabeza del pescado’."),
                     textAreaInput(
                       inputId = "fish_problem",
                       label   = "Desviación / problema:",
                       placeholder = "Ejemplo: No se realizó el análisis de riesgo para definir la frecuencia de participación en ensayos de aptitud...",
                       rows = 4
                     ),
                     span("Causas por categoría", class = "section-title"),
                     p(class = "section-help",
                       "Escriba una causa por línea. El texto se mostrará como viñetas en cada categoría."),
                     textAreaInput("fish_metodo",   "Método", rows = 3,
                                   placeholder = "Ej: Procedimiento desactualizado\nNo incluye análisis de riesgo 7.3.1"),
                     textAreaInput("fish_personal", "Personal", rows = 3,
                                   placeholder = "Ej: Desconocimiento del requisito\nFalta de apropiación del CEA-3.0-04"),
                     textAreaInput("fish_equipos",  "Equipos", rows = 3,
                                   placeholder = "Ej: No aplica / No afecta\nFalta de software para seguimiento"),
                     textAreaInput("fish_materiales", "Materiales / Insumos", rows = 3,
                                   placeholder = "Ej: No aplica / No afecta la desviación"),
                     textAreaInput("fish_entorno", "Entorno / Ambiente", rows = 3,
                                   placeholder = "Ej: Cambios normativos recientes\nAlta carga de auditorías"),
                     textAreaInput("fish_medicion", "Medición / Control", rows = 3,
                                   placeholder = "Ej: Falta de revisión anual de matrices de riesgo\nSeguimiento inefectivo de EA/CILD"),
                     span("Causa raíz final", class = "section-title"),
                     p(class = "section-help",
                       "Redacte aquí la causa raíz final. Puede copiar una de las opciones sugeridas ",
                       "por ChatGPT y ajustarla según su criterio."),
                     textAreaInput(
                       "fish_root_text",
                       label = NULL,
                       placeholder = "Ejemplo: La reincidencia en la no consideración de los requisitos del CEA-3.0-04 para EA/CILD se debe a la falta de actualización del procedimiento interno, ausencia de responsable definido para el análisis de riesgo y débil seguimiento a la planificación anual.",
                       rows = 4
                     ),
                     br(),
                     actionButton("download_fish_img", "Descargar imagen del diagrama")
                 )
               ),
               column(
                 width = 8,
                 div(class = "app-card",
                     span("Vista del diagrama tipo espina de pescado", class = "section-title"),
                     p(class = "section-help",
                       "Este bloque es el que se captura como imagen. El texto se ajusta automáticamente, sin sobreponerse."),
                     uiOutput("fishbone_html")
                 ),
                 br(),
                 div(class = "app-card",
                     div(class = "ai-box",
                         span("Asistente IA - Redacción de causa raíz", class = "ai-title"),
                         p(
                           "Se utilizará la información del diagrama de causa–efecto ",
                           "para proponer redacciones de causa raíz alineadas con sistemas de calidad."
                         ),
                         actionButton("fish_ai_btn", "Generar opciones con IA")
                     ),
                     br(),
                     div(class = "ai-output",
                         verbatimTextOutput("fish_ai_text")
                     )
                 )
               )
             )
    ),
    
    # ---------------- LLUVIA DE IDEAS ----------------
    tabPanel("Lluvia de ideas",
             br(),
             fluidRow(
               column(
                 width = 4,
                 div(class = "app-card",
                     span("Lluvia de ideas", class = "method-pill"),
                     span("Definición de la desviación", class = "section-title"),
                     p(class = "section-help",
                       "Defina la desviación o problema que servirá de foco para esta lluvia de ideas."),
                     textAreaInput(
                       inputId = "brain_problem",
                       label   = "Desviación / problema:",
                       placeholder = "Ejemplo: Alta reincidencia de no conformidades relacionadas con trabajos no conformes...",
                       rows = 4
                     ),
                     hr(),
                     span("Registro de ideas / causas", class = "section-title"),
                     textInput(
                       "idea_text",
                       "Idea / causa:",
                       placeholder = "Ej: Falta de entrenamiento específico en gestión de trabajos no conformes"
                     ),
                     selectInput(
                       "idea_tipo",
                       "Tipo de causa:",
                       choices = c("Procedimiento", "Personal", "Equipo", "Material / Insumo",
                                   "Entorno", "Gestión / Organización", "Otro"),
                       selected = "Procedimiento"
                     ),
                     sliderInput(
                       "idea_impacto",
                       "Impacto percibido sobre la desviación:",
                       min   = 1,
                       max   = 5,
                       value = 3,
                       step  = 1
                     ),
                     actionButton("add_idea", "Agregar idea"),
                     actionButton("clear_ideas", "Limpiar lista"),
                     br(), br(),
                     downloadButton("download_brain_img", "Descargar imagen de priorización")
                 )
               ),
               column(
                 width = 8,
                 div(class = "app-card",
                     span("Ideas registradas y priorización", class = "section-title"),
                     p(class = "section-help",
                       "Utilice esta tabla para priorizar y depurar las causas antes de formalizar el análisis."),
                     DTOutput("ideas_table")
                 ),
                 br(),
                 div(class = "app-card",
                     div(class = "ai-box",
                         span("Asistente IA - Redacción de causa raíz", class = "ai-title"),
                         p(
                           "Se tomará la desviación y las ideas registradas (con su impacto) ",
                           "para proponer varias redacciones de causa raíz consolidadas."
                         ),
                         actionButton("brain_ai_btn", "Generar opciones con IA")
                     ),
                     br(),
                     div(class = "ai-output",
                         verbatimTextOutput("brain_ai_text")
                     )
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
  
  # ---------- 5 PORQUÉS ----------
  output$whys_inputs <- renderUI({
    n <- input$n_whys
    if (is.null(n) || n < 1) n <- 1
    
    lapply(
      seq_len(n),
      function(i) {
        textAreaInput(
          inputId = paste0("why_", i),
          label   = paste0("Respuesta al porqué ", i, ":"),
          placeholder = "Describa la causa identificada en este nivel...",
          rows = 2
        )
      }
    )
  })
  
  whys_data <- reactive({
    n <- input$n_whys
    if (is.null(n) || n < 1) return(NULL)
    
    respuestas <- character(n)
    for (i in seq_len(n)) {
      id <- paste0("why_", i)
      respuestas[i] <- if (is.null(input[[id]])) "" else input[[id]]
    }
    
    data.frame(
      Nivel_Porque = seq_len(n),
      Desviacion   = if (is.null(input$whys_problem)) "" else input$whys_problem,
      Respuesta    = respuestas,
      stringsAsFactors = FALSE
    )
  })
  
  output$whys_plot <- renderPlot({
    df <- whys_data()
    plot_whys(df)
  })
  
  output$download_whys_img <- downloadHandler(
    filename = function() {
      paste0("analisis_5_porque_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- whys_data()
      png(file, width = 1200, height = 700, res = 130)
      plot_whys(df)
      dev.off()
    }
  )
  
  # IA - 5 Porqués
  observeEvent(input$whys_ai_btn, {
    df <- whys_data()
    if (is.null(df)) {
      output$whys_ai_text <- renderText("Debe diligenciar primero la desviación y las respuestas de los porqués.")
      return(NULL)
    }
    
    contexto <- paste0(
      "Desviación / problema: ", df$Desviacion[1], "\n\n",
      paste0("Nivel ", df$Nivel_Porque, ": ", df$Respuesta, collapse = "\n")
    )
    
    texto_ia <- generar_causa_ia(contexto, "5 Porqués")
    output$whys_ai_text <- renderText(texto_ia)
  })
  
  # ---------- DIAGRAMA DE CAUSA–EFECTO ----------
  fishbone_df <- reactive({
    categorias <- c("Método", "Personal", "Equipos",
                    "Materiales / Insumos", "Entorno / Ambiente", "Medición / Control")
    ids <- c("fish_metodo", "fish_personal", "fish_equipos",
             "fish_materiales", "fish_entorno", "fish_medicion")
    
    causas_list <- lapply(seq_along(ids), function(i) {
      txt <- input[[ids[i]]]
      if (is.null(txt) || txt == "") return(NULL)
      lineas <- unlist(strsplit(txt, "\n"))
      lineas <- trimws(lineas)
      lineas <- lineas[lineas != ""]
      if (length(lineas) == 0) return(NULL)
      data.frame(
        Categoria  = categorias[i],
        Causa      = lineas,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, causas_list)
  })
  
  output$fishbone_html <- renderUI({
    df <- fishbone_df()
    problem <- if (is.null(input$fish_problem)) "" else input$fish_problem
    root    <- if (is.null(input$fish_root_text)) "" else input$fish_root_text
    
    if (is.null(df)) {
      df <- data.frame(Categoria = character(), Causa = character())
    }
    
    cats <- unique(df$Categoria)
    
    # Si no hay categorías, no calculamos seq() para evitar el error
    if (length(cats) == 0) {
      top_cats    <- character(0)
      bottom_cats <- character(0)
    } else {
      # 1,3,5 arriba; 2,4,6 abajo
      top_cats    <- cats[seq(1, length(cats), by = 2)]
      bottom_cats <- setdiff(cats, top_cats)
    }
    
    make_branch <- function(cat) {
      causas <- df$Causa[df$Categoria == cat]
      if (length(causas) == 0) return(NULL)
      tags$div(
        class = "fb-branch",
        tags$div(class = "fb-category", cat),
        tags$ul(
          lapply(causas, function(ca) tags$li(ca))
        )
      )
    }
    
    tags$div(
      id = "fishbone-container",
      class = "fb-container",
      tags$div(class = "fb-title", "Diagrama de Causa–Efecto (Ishikawa)"),
      tags$div(class = "fb-problem-label", "Desviación / efecto"),
      tags$div(class = "fb-problem-text", problem),
      
      tags$div(
        class = "fb-axis-wrapper",
        tags$div(class = "fb-axis"),
        tags$div(class = "fb-effect-label", "Efecto")
      ),
      
      tags$div(
        class = "fb-top-row",
        lapply(top_cats, make_branch)
      ),
      tags$div(
        class = "fb-bottom-row",
        lapply(bottom_cats, make_branch)
      ),
      
      tags$div(class = "fb-root-title", "Causa raíz"),
      tags$div(
        class = "fb-root-text",
        if (root == "") "Sin causa raíz registrada." else root
      )
    )
  })
  
  
  observeEvent(input$download_fish_img, {
    session$sendCustomMessage(
      "capture-fishbone",
      list(filename = paste0("diagrama_causa_efecto_", Sys.Date(), ".png"))
    )
  })
  
  # IA - Causa–Efecto
  observeEvent(input$fish_ai_btn, {
    df <- fishbone_df()
    problem <- if (is.null(input$fish_problem)) "" else input$fish_problem
    if (is.null(df) || nrow(df) == 0) {
      output$fish_ai_text <- renderText("Debe diligenciar primero la desviación y al menos una causa en alguna categoría.")
      return(NULL)
    }
    
    resumen <- paste0(
      "Desviación / problema: ", ifelse(problem == "", "[No especificado]", problem), "\n\n",
      paste0(df$Categoria, ": ", df$Causa, collapse = "\n")
    )
    
    texto_ia <- generar_causa_ia(resumen, "Diagrama de Causa–Efecto (Ishikawa)")
    output$fish_ai_text <- renderText(texto_ia)
  })
  
  # ---------- LLUVIA DE IDEAS ----------
  ideas <- reactiveVal(
    data.frame(
      Desviacion = character(),
      Idea       = character(),
      Tipo       = character(),
      Impacto    = integer(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$add_idea, {
    req(input$idea_text)
    
    df <- ideas()
    nueva <- data.frame(
      Desviacion = if (is.null(input$brain_problem)) "" else input$brain_problem,
      Idea       = input$idea_text,
      Tipo       = input$idea_tipo,
      Impacto    = input$idea_impacto,
      stringsAsFactors = FALSE
    )
    df <- rbind(df, nueva)
    ideas(df)
    
    updateTextInput(session, "idea_text", value = "")
  })
  
  observeEvent(input$clear_ideas, {
    ideas(
      data.frame(
        Desviacion = character(),
        Idea       = character(),
        Tipo       = character(),
        Impacto    = integer(),
        stringsAsFactors = FALSE
      )
    )
  })
  
  output$ideas_table <- renderDT({
    df <- ideas()
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        language = list(
          url = "https://cdn.datatables.net/plug-ins/1.13.1/i18n/es-ES.json"
        ),
        columnDefs = list(
          list(targets = 3, className = "dt-center")
        )
      )
    )
  })
  
  output$download_brain_img <- downloadHandler(
    filename = function() {
      paste0("lluvia_ideas_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- ideas()
      problem <- if (is.null(input$brain_problem)) "" else input$brain_problem
      png(file, width = 1400, height = 800, res = 130)
      plot_brainstorm(df, problem)
      dev.off()
    }
  )
  
  # IA - Lluvia de ideas
  observeEvent(input$brain_ai_btn, {
    df <- ideas()
    problem <- if (is.null(input$brain_problem)) "" else input$brain_problem
    
    if (is.null(df) || nrow(df) == 0) {
      output$brain_ai_text <- renderText("Debe registrar primero al menos una idea/causa.")
      return(NULL)
    }
    
    contexto <- paste0(
      "Desviación / problema: ", ifelse(problem == "", "[No especificado]", problem), "\n\n",
      "Ideas y causas registradas:\n",
      paste0("- (Impacto ", df$Impacto, ") [", df$Tipo, "]: ", df$Idea, collapse = "\n")
    )
    
    texto_ia <- generar_causa_ia(contexto, "Lluvia de ideas")
    output$brain_ai_text <- renderText(texto_ia)
  })
}

shinyApp(ui, server)

