suppressMessages({library(dplyr); library(mgcv)})
source("features.R")

titles_train  <- read.csv("titles_train.csv")
credits_train <- read.csv("credits_train.csv")
titles_test   <- read.csv("titles_test.csv")
credits_test  <- read.csv("credits_test.csv")

stats   <- calcular_stats_train(titles_train, credits_train)
train_f <- construir_features(titles_train, credits_train, stats)
test_f  <- construir_features(titles_test, credits_test, stats)

train_f$pais_principal <- factor(train_f$pais_principal)
test_f$pais_principal  <- factor(test_f$pais_principal, levels = levels(train_f$pais_principal))

generos_formula <- paste(paste0("genero_", GENEROS_TODOS), collapse = " + ")

## Modelo ganador: splines + efecto aleatorio de pais
f_ganador <- as.formula(paste(
  "imdb_score ~ s(runtime) + s(release_year) + s(imdb_votes) + type +",
  "age_certification_agr +", generos_formula, "+ s(pais_principal, bs = 're')"
))
modelo_ganador <- gam(f_ganador, data = train_f)

## Modelo de respaldo (sin el termino de pais) para los paises nunca vistos en train
f_respaldo <- as.formula(paste(
  "imdb_score ~ s(runtime) + s(release_year) + s(imdb_votes) + type +",
  "age_certification_agr + pais_agrupado +", generos_formula
))
modelo_respaldo <- gam(f_respaldo, data = train_f)

pred <- predict(modelo_ganador, newdata = test_f)
faltantes <- is.na(pred)
cat("Titulos con pais no visto en train (uso modelo de respaldo):", sum(faltantes), "\n")
pred[faltantes] <- predict(modelo_respaldo, newdata = test_f[faltantes, ])

stopifnot(length(pred) == nrow(titles_test))
stopifnot(!any(is.na(pred)))

write.table(
  pred,
  file = "predicciones_Torres_LeonardisAyala.csv",
  sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE
)

cat("Predicciones guardadas. Resumen:\n")
print(summary(pred))
