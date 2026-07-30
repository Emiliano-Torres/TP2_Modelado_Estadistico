library(ggplot2)
library(dplyr)

titles_train<- read.csv("titles_train.csv")

credits_train<- read.csv("credits_train.csv")

titles_test <- read.csv("titles_test.csv")

agregar_features <- function(datos, credits) {
  
  cantidad_personal <- credits %>%
    group_by(id) %>%
    summarise(
      personal_pelicula = n(),
      .groups = "drop"
    )
  
  datos %>%
    left_join(cantidad_personal, by = "id") %>%
    mutate(
      personal_pelicula = ifelse(is.na(personal_pelicula), 0, personal_pelicula),
      dummy_show = ifelse(type == "SHOW", 1, 0)
    )
}




separar_train_test <- function(datos, proporcion_train = 0.8, seed = 123) {
  if (proporcion_train <= 0 || proporcion_train >= 1) {
    stop("proporcion_train debe estar entre 0 y 1.")
  }
  
  set.seed(seed)
  
  indices_train <- sample(
    seq_len(nrow(datos)),
    size = floor(proporcion_train * nrow(datos))
  )
  
  list(
    train = datos[indices_train, , drop = FALSE],
    test = datos[-indices_train, , drop = FALSE]
  )
}

predecir_y_evaluar <- function(
    modelo,
    datos_test,
    variable_objetivo,
    guardar_csv = FALSE,
    nombre_archivo = "predicciones.csv"
) {
  if (!variable_objetivo %in% names(datos_test)) {
    stop("La variable objetivo no existe en datos_test.")
  }
  
  predicciones <- predict(modelo, newdata = datos_test)
  
  valores_reales <- datos_test[[variable_objetivo]]
  
  validos <- !is.na(valores_reales) & !is.na(predicciones)
  
  mse <- mean(
    (valores_reales[validos] - predicciones[validos])^2
  )
  
  resultados <- data.frame(
    real = valores_reales,
    prediccion = as.numeric(predicciones),
    error = valores_reales - as.numeric(predicciones),
    error_cuadrado = (valores_reales - as.numeric(predicciones))^2
  )
  
  if (guardar_csv) {
    write.table(
      predicciones,
      file = nombre_archivo,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
  }
  
  list(
    mse = mse,
    predicciones = predicciones,
    resultados = resultados
  )
}


datos= agregar_features(titles_train, credits_train)

datos = separar_train_test(datos)


train = datos$train

test = datos$test


# Modelo 1: Efectos mixtos

library(lme4)

modelo1 <- lmer(
  imdb_score ~ imdb_votes+ release_year + runtime  + (1 | dummy_show + age_certification),
  data = train
)

res1 = predecir_y_evaluar(modelo1, test, "imdb_score")

summary(modelo1)


# Modelo 2: Spline cubicos naturales
library(mgcv)

modelo2 <- gam(imdb_score ~ s(runtime + personal_movie, bs = "cr", k=20) +
                 s(release_year, bs = "cr", k=20) +
                 s(dummy_show + age_certification, bs='re')
               ,data = train)

res2 = predecir_y_evaluar(modelo2, test, "imdb_score")

summary(modelo2)


#Modelo 3:
library(mgcv)

modelo3 <- gam(imdb_score ~ s(runtime + personal_movie, bs = "cr", k=20) +
                 s(release_year, bs = "cr", k=20) +
                 s(dummy_show + age_certification, bs='re')
               ,data = train)

res3 = predecir_y_evaluar(modelo2, test, "imdb_score")

summary(modelo2)

#Modelo 4:


#Modelo 5:




