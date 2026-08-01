## ============================================================
## Feature engineering compartido para titles/credits (train y test)
## Cualquier estadistico calculado sobre el train (top paises, medianas
## de imputacion) se pasa como parametro para aplicarlo igual al test,
## evitando fuga de informacion.
## ============================================================

library(dplyr)
library(stringr)

parsear_lista <- function(x) {
  x <- gsub("\\[|\\]|'", "", x)
  x <- str_split(x, ",\\s*")
  lapply(x, function(v) v[v != ""])
}

GENEROS_TODOS <- c("comedy", "drama", "action", "thriller", "romance", "crime",
                    "documentation", "family", "fantasy", "scifi", "animation",
                    "european", "horror", "music", "history", "war", "reality",
                    "sport", "western")

## Estadisticos que deben calcularse SOLO con el train
calcular_stats_train <- function(titles_train, credits_train) {
  paises_lista <- parsear_lista(titles_train$production_countries)
  pais_principal <- sapply(paises_lista, function(v) if (length(v) == 0) NA else v[1])
  top_paises <- names(sort(table(pais_principal), decreasing = TRUE))[1:9]

  cert_limpio <- ifelse(is.na(titles_train$age_certification) | titles_train$age_certification == "",
                         "Desconocida", titles_train$age_certification)
  cert_counts <- table(cert_limpio)
  categorias_cert <- names(cert_counts[cert_counts >= 20])

  list(
    top_paises      = top_paises,
    mediana_votes   = median(titles_train$imdb_votes, na.rm = TRUE),
    categorias_cert = categorias_cert
  )
}

construir_features <- function(titles, credits, stats) {
  genres_lista <- parsear_lista(titles$genres)
  paises_lista <- parsear_lista(titles$production_countries)

  datos <- titles %>%
    mutate(
      pais_principal = sapply(paises_lista, function(v) if (length(v) == 0) NA_character_ else v[1]),
      n_generos = lengths(genres_lista),
      n_paises  = lengths(paises_lista),
      type      = factor(type, levels = c("MOVIE", "SHOW")),
      age_certification = ifelse(is.na(age_certification) | age_certification == "", "Desconocida", age_certification),
      imdb_votes = ifelse(is.na(imdb_votes), stats$mediana_votes, imdb_votes),
      numero_temporadas = ifelse(is.na(seasons), 0, seasons)
    )

  ## dummies de genero (multi-etiqueta)
  for (g in GENEROS_TODOS) {
    datos[[paste0("genero_", g)]] <- sapply(genres_lista, function(v) as.integer(g %in% v))
  }

  ## agrupamos age_certification con muy pocas observaciones (umbral calculado solo en train)
  datos <- datos %>%
    mutate(age_certification_agr = ifelse(age_certification %in% stats$categorias_cert,
                                           age_certification, "Otra"))

  ## pais agrupado (top del train + "Otro") para modelos de efectos fijos;
  ## pais_principal completo se usa aparte como grouping var de efectos mixtos
  datos <- datos %>%
    mutate(pais_agrupado = ifelse(pais_principal %in% stats$top_paises, pais_principal, "Otro"),
           pais_agrupado = ifelse(is.na(pais_agrupado), "Otro", pais_agrupado),
           pais_principal = ifelse(is.na(pais_principal), "Desconocido", pais_principal))

  ## info de elenco/direccion desde credits
  resumen_credits <- credits %>%
    group_by(id) %>%
    summarise(
      cantidad_actores    = sum(role == "ACTOR"),
      cantidad_directores = sum(role == "DIRECTOR"),
      personal_total      = n(),
      .groups = "drop"
    )

  director_principal <- credits %>%
    filter(role == "DIRECTOR") %>%
    group_by(id) %>%
    summarise(director_principal = first(name), .groups = "drop")

  datos <- datos %>%
    left_join(resumen_credits, by = "id") %>%
    left_join(director_principal, by = "id") %>%
    mutate(
      cantidad_actores    = ifelse(is.na(cantidad_actores), 0, cantidad_actores),
      cantidad_directores = ifelse(is.na(cantidad_directores), 0, cantidad_directores),
      personal_total      = ifelse(is.na(personal_total), 0, personal_total),
      director_principal  = ifelse(is.na(director_principal), "Sin_director", director_principal),
      comedy              = as.integer(genero_comedy)
    )

  datos
}
