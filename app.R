library(shiny)
library(tidyverse)
library(plotly)
library(randomForest)
library(DT)

# ============================================================================
# CARGAR Y PREPARAR DATOS (OPTIMIZADO)
# ============================================================================

spotify_raw <- read_csv("dataset.csv", show_col_types = FALSE)

# REDUCIR TAMAÑO: Tomar una muestra representativa si el dataset es muy grande
if (nrow(spotify_raw) > 50000) {
  set.seed(123)
  spotify_raw <- spotify_raw %>% 
    sample_n(50000)  # Limitar a 50k filas para mejor rendimiento
}

# Limpieza básica
spotify <- spotify_raw %>%
  janitor::clean_names() %>%
  select(-matches("^unnamed_0$|^x1$")) %>%
  rename(
    artist_name = artists,
    genre = track_genre
  ) %>%
  drop_na(track_id, artist_name, track_name, popularity) %>%
  distinct(track_id, track_name, .keep_all = TRUE) %>%
  mutate(
    popularity = as.numeric(popularity),
    duration_min = duration_ms / 60000
  )

# Crear meta-géneros
genres_pop <- c("cantopop", "indie-pop", "j-pop", "k-pop", "mandopop", 
                "pop", "pop-film", "power-pop", "synth-pop")
genres_rock <- c("alt-rock", "hard-rock", "j-rock", "psych-rock",
                 "punk-rock", "rock", "rock-n-roll", "rockabilly")
genres_hiphop <- c("hip-hop")

spotify <- spotify %>%
  mutate(
    meta_genre = case_when(
      is.na(genre) ~ "Other",
      genre %in% genres_pop ~ "Pop",
      genre %in% genres_rock ~ "Rock",
      genre %in% genres_hiphop ~ "Hip-hop",
      TRUE ~ "Other"
    )
  )

# ============================================================================
# ENTRENAR MODELO PREDICTIVO (OPTIMIZADO)
# ============================================================================

vars_modelo <- c("popularity", "danceability", "energy", "loudness",
                 "acousticness", "instrumentalness", "valence",
                 "speechiness", "liveness", "tempo", "duration_min")

model_data <- spotify %>%
  select(all_of(vars_modelo)) %>%
  drop_na()

# Usar muestra más pequeña para entrenamiento (más rápido)
set.seed(123)
if (nrow(model_data) > 20000) {
  train_sample <- sample_n(model_data, 20000)
} else {
  train_sample <- model_data
}

train_idx <- sample(seq_len(nrow(train_sample)), size = 0.7 * nrow(train_sample))
train <- train_sample[train_idx, ]
test <- train_sample[-train_idx, ]

# Entrenar Random Forest con MENOS árboles (más rápido)
mod_rf <- randomForest(
  popularity ~ .,
  data = train,
  ntree = 100,        # Reducido de 200 a 100
  mtry = 3,
  importance = TRUE
)

# Limpiar memoria
rm(spotify_raw, model_data, train_sample)
gc()

# ============================================================================
# INTERFAZ DE USUARIO (UI)
# ============================================================================

ui <- fluidPage(
  titlePanel("🎵 Análisis de Popularidad en Spotify"),
  
  tabsetPanel(
    # ========== TAB 1: EXPLORACIÓN ==========
    tabPanel(
      "📊 Exploración",
      sidebarLayout(
        sidebarPanel(
          h4("Filtros de exploración"),
          selectInput(
            "meta_genre_filter",
            "Selecciona meta-género:",
            choices = c("Todos", unique(spotify$meta_genre)),
            selected = "Todos"
          ),
          sliderInput(
            "pop_range",
            "Rango de popularidad:",
            min = 0,
            max = 100,
            value = c(0, 100)
          ),
          hr(),
          p("Explora las distribuciones de características musicales según género y popularidad.")
        ),
        mainPanel(
          h3("Dispersión: Popularidad vs Danceability"),
          plotlyOutput("scatter_pop_dance", height = "400px"),
          hr(),
          fluidRow(
            column(6, 
                   h4("Histograma: Energía"),
                   plotlyOutput("hist_energy", height = "300px")
            ),
            column(6,
                   h4("Histograma: Tempo"),
                   plotlyOutput("hist_tempo", height = "300px")
            )
          )
        )
      )
    ),
    
    # ========== TAB 2: PREDICCIÓN ==========
    tabPanel(
      "🔮 Predicción",
      sidebarLayout(
        sidebarPanel(
          h4("Características de la canción"),
          sliderInput("pred_danceability", "Danceability:", 
                      min = 0, max = 1, value = 0.5, step = 0.01),
          sliderInput("pred_energy", "Energy:", 
                      min = 0, max = 1, value = 0.5, step = 0.01),
          sliderInput("pred_loudness", "Loudness (dB):", 
                      min = -60, max = 0, value = -5, step = 1),
          sliderInput("pred_acousticness", "Acousticness:", 
                      min = 0, max = 1, value = 0.3, step = 0.01),
          sliderInput("pred_instrumentalness", "Instrumentalness:", 
                      min = 0, max = 1, value = 0.1, step = 0.01),
          sliderInput("pred_valence", "Valence:", 
                      min = 0, max = 1, value = 0.5, step = 0.01),
          sliderInput("pred_speechiness", "Speechiness:", 
                      min = 0, max = 1, value = 0.05, step = 0.01),
          sliderInput("pred_liveness", "Liveness:", 
                      min = 0, max = 1, value = 0.1, step = 0.01),
          sliderInput("pred_tempo", "Tempo (BPM):", 
                      min = 50, max = 200, value = 120, step = 1),
          sliderInput("pred_duration_min", "Duración (min):", 
                      min = 1, max = 10, value = 3.5, step = 0.1),
          hr(),
          actionButton("predict_btn", "Predecir Popularidad", 
                       class = "btn-primary btn-lg")
        ),
        mainPanel(
          h3("Resultado de la predicción"),
          uiOutput("prediction_box"),
          hr(),
          h4("Importancia de variables en el modelo"),
          plotOutput("var_importance", height = "400px"),
          hr(),
          h4("Métricas del modelo (Random Forest)"),
          verbatimTextOutput("model_metrics")
        )
      )
    ),
    
    # ========== TAB 3: TOP ARTISTAS ==========
    tabPanel(
      "🎤 Top Artistas",
      sidebarLayout(
        sidebarPanel(
          h4("Configuración"),
          sliderInput(
            "top_n_artists",
            "Número de artistas a mostrar:",
            min = 5,
            max = 30,
            value = 15,
            step = 1
          ),
          numericInput(
            "min_canciones",
            "Mínimo de canciones por artista:",
            value = 20,
            min = 1,
            max = 100
          ),
          hr(),
          p("Visualiza los artistas con mayor popularidad promedio.")
        ),
        mainPanel(
          h3("Top Artistas por Popularidad Promedio"),
          plotlyOutput("top_artists_plot", height = "600px"),
          hr(),
          h4("Tabla de datos"),
          DTOutput("top_artists_table")
        )
      )
    ),
    
    # ========== TAB 4: SERIE TEMPORAL ==========
    tabPanel(
      "📈 Tendencias",
      sidebarLayout(
        sidebarPanel(
          h4("Nota importante"),
          p("⚠️ Este dataset NO contiene fechas reales de lanzamiento."),
          p("Se ha simulado una distribución temporal basada en la posición de las canciones."),
          hr(),
          selectInput(
            "temporal_genre",
            "Filtrar por meta-género:",
            choices = c("Todos", unique(spotify$meta_genre)),
            selected = "Todos"
          ),
          checkboxInput(
            "show_trend",
            "Mostrar línea de tendencia",
            value = TRUE
          )
        ),
        mainPanel(
          h3("Promedio de popularidad por período simulado"),
          plotlyOutput("temporal_plot", height = "500px"),
          hr(),
          p(strong("Interpretación:")),
          p("Se construyó un índice temporal artificial basado en el orden de las canciones.")
        )
      )
    )
  )
)

# ============================================================================
# SERVIDOR (LÓGICA REACTIVA)
# ============================================================================

server <- function(input, output, session) {
  
  # ========== DATOS FILTRADOS ==========
  filtered_data <- reactive({
    data <- spotify %>%
      filter(popularity >= input$pop_range[1],
             popularity <= input$pop_range[2])
    
    if (input$meta_genre_filter != "Todos") {
      data <- data %>% filter(meta_genre == input$meta_genre_filter)
    }
    
    data
  })
  
  # ========== GRÁFICO: DISPERSIÓN ==========
  output$scatter_pop_dance <- renderPlotly({
    data <- filtered_data()
    
    # Limitar puntos si hay demasiados
    if (nrow(data) > 5000) {
      data <- sample_n(data, 5000)
    }
    
    plot_ly(
      data,
      x = ~danceability,
      y = ~popularity,
      color = ~meta_genre,
      type = "scatter",
      mode = "markers",
      marker = list(size = 5, opacity = 0.6),
      text = ~paste("Canción:", track_name, "<br>Artista:", artist_name,
                    "<br>Popularidad:", popularity)
    ) %>%
      layout(
        title = "Relación entre Danceability y Popularidad",
        xaxis = list(title = "Danceability"),
        yaxis = list(title = "Popularidad"),
        hovermode = "closest"
      )
  })
  
  # ========== HISTOGRAMA: ENERGÍA ==========
  output$hist_energy <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~energy, type = "histogram", 
            marker = list(color = "#1DB954")) %>%
      layout(
        xaxis = list(title = "Energy"),
        yaxis = list(title = "Frecuencia"),
        bargap = 0.1
      )
  })
  
  # ========== HISTOGRAMA: TEMPO ==========
  output$hist_tempo <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, x = ~tempo, type = "histogram",
            marker = list(color = "#C4B5FD")) %>%
      layout(
        xaxis = list(title = "Tempo (BPM)"),
        yaxis = list(title = "Frecuencia"),
        bargap = 0.1
      )
  })
  
  # ========== PREDICCIÓN ==========
  prediction <- eventReactive(input$predict_btn, {
    new_data <- data.frame(
      danceability = input$pred_danceability,
      energy = input$pred_energy,
      loudness = input$pred_loudness,
      acousticness = input$pred_acousticness,
      instrumentalness = input$pred_instrumentalness,
      valence = input$pred_valence,
      speechiness = input$pred_speechiness,
      liveness = input$pred_liveness,
      tempo = input$pred_tempo,
      duration_min = input$pred_duration_min
    )
    
    predict(mod_rf, newdata = new_data)
  })
  
  output$prediction_box <- renderUI({
    pred_value <- round(prediction(), 1)
    
    color <- if (pred_value >= 70) {
      "#28a745"
    } else if (pred_value >= 40) {
      "#ffc107"
    } else {
      "#dc3545"
    }
    
    div(
      style = paste0("background-color: ", color, 
                     "; color: white; padding: 30px; 
                     border-radius: 10px; text-align: center;"),
      h2("Popularidad predicha:"),
      h1(style = "font-size: 4em; margin: 0;", pred_value),
      p("(escala de 0 a 100)")
    )
  })
  
  # ========== IMPORTANCIA DE VARIABLES ==========
  output$var_importance <- renderPlot({
    importance_df <- as.data.frame(importance(mod_rf))
    importance_df$variable <- rownames(importance_df)
    
    importance_df %>%
      arrange(desc(`%IncMSE`)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
      geom_col(fill = "#1DB954") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Top 10 variables más importantes",
        x = "",
        y = "% Incremento en MSE"
      )
  })
  
  # ========== MÉTRICAS DEL MODELO ==========
  output$model_metrics <- renderPrint({
    pred_test <- predict(mod_rf, newdata = test)
    rmse <- sqrt(mean((test$popularity - pred_test)^2))
    mae <- mean(abs(test$popularity - pred_test))
    r2 <- cor(test$popularity, pred_test)^2
    
    cat("Métricas en conjunto de prueba:\n")
    cat("-------------------------------\n")
    cat("RMSE: ", round(rmse, 2), "\n")
    cat("MAE:  ", round(mae, 2), "\n")
    cat("R²:   ", round(r2, 3), "\n")
    cat("\nInterpretación:\n")
    cat("El modelo explica aproximadamente el ", round(r2 * 100, 1), 
        "% de la\nvariabilidad en la popularidad usando solo características acústicas.")
  })
  
  # ========== TOP ARTISTAS ==========
  top_artists_data <- reactive({
    spotify %>%
      separate_rows(artist_name, sep = ";") %>%
      mutate(artist_single = str_trim(artist_name)) %>%
      group_by(artist_single) %>%
      summarise(
        n_canciones = n_distinct(track_id),
        popularidad_promedio = mean(popularity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_canciones >= input$min_canciones) %>%
      arrange(desc(popularidad_promedio)) %>%
      slice_head(n = input$top_n_artists)
  })
  
  output$top_artists_plot <- renderPlotly({
    data <- top_artists_data()
    
    plot_ly(
      data,
      x = ~popularidad_promedio,
      y = ~reorder(artist_single, popularidad_promedio),
      type = "bar",
      orientation = "h",
      marker = list(color = "#1DB954"),
      text = ~paste("Canciones:", n_canciones, 
                    "<br>Popularidad:", round(popularidad_promedio, 1)),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Popularidad Promedio"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  output$top_artists_table <- renderDT({
    top_artists_data() %>%
      rename(
        Artista = artist_single,
        `Número de canciones` = n_canciones,
        `Popularidad promedio` = popularidad_promedio
      ) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # ========== SERIE TEMPORAL ==========
  output$temporal_plot <- renderPlotly({
    data_temporal <- spotify %>%
      arrange(track_id) %>%
      mutate(periodo = row_number() %/% 1000)
    
    if (input$temporal_genre != "Todos") {
      data_temporal <- data_temporal %>%
        filter(meta_genre == input$temporal_genre)
    }
    
    temporal_agg <- data_temporal %>%
      group_by(periodo) %>%
      summarise(
        popularidad_promedio = mean(popularity, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- plot_ly(
      temporal_agg,
      x = ~periodo,
      y = ~popularidad_promedio,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1DB954"),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Promedio de popularidad por período (simulado)",
        xaxis = list(title = "Período (índice artificial)"),
        yaxis = list(title = "Popularidad Promedio")
      )
    
    if (input$show_trend) {
      fit <- lm(popularidad_promedio ~ periodo, data = temporal_agg)
      temporal_agg$trend <- predict(fit)
      
      p <- p %>%
        add_trace(
          data = temporal_agg,
          x = ~periodo,
          y = ~trend,
          type = "scatter",
          mode = "lines",
          line = list(dash = "dash", color = "red"),
          name = "Tendencia"
        )
    }
    
    p
  })
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================

shinyApp(ui = ui, server = server)