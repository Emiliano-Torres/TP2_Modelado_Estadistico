library(dplyr)
library(lme4)
library(mgcv)
source("features.R")

titles_train  <- read.csv("titles_train.csv")
credits_train <- read.csv("credits_train.csv")

stats <- calcular_stats_train(titles_train, credits_train)
datos <- construir_features(titles_train, credits_train, stats)

generos_formula <- paste(paste0("genero_", GENEROS_TODOS), collapse = " + ")

set.seed(123)
idx   <- sample(nrow(datos), round(0.75 * nrow(datos)))
train <- datos[idx, ]
test  <- datos[-idx, ]

mse <- function(pred, real) mean((pred - real)^2)

resultados <- list()

## ---- Modelo 1: lm base ----
f1 <- as.formula(paste("imdb_score ~ runtime + release_year + imdb_votes + type +",
                        "age_certification_agr + pais_agrupado +", generos_formula))
m1 <- lm(f1, data = train)
resultados$m1_lm_base <- mse(predict(m1, test), test$imdb_score)

## ---- Modelo 2: lm con features de elenco/temporadas ----
f2 <- as.formula(paste("imdb_score ~ runtime + release_year + imdb_votes + type +",
                        "age_certification_agr + pais_agrupado + numero_temporadas +",
                        "cantidad_actores + cantidad_directores + n_generos +", generos_formula))
m2 <- lm(f2, data = train)
resultados$m2_lm_rico <- mse(predict(m2, test), test$imdb_score)

## ---- Modelo 3: efectos mixtos, random intercept por pais ----
f3 <- as.formula(paste("imdb_score ~ runtime + release_year + imdb_votes + type +",
                        "age_certification_agr +", generos_formula, "+ (1 | pais_principal)"))
m3 <- lmer(f3, data = train)
pred3 <- predict(m3, newdata = test, allow.new.levels = TRUE)
resultados$m3_mixto_pais <- mse(pred3, test$imdb_score)

## ---- Modelo 4: efectos mixtos, random intercept por director ----
f4 <- as.formula(paste("imdb_score ~ runtime + release_year + imdb_votes + type +",
                        "age_certification_agr +", generos_formula, "+ (1 | director_principal)"))
m4 <- lmer(f4, data = train)
pred4 <- predict(m4, newdata = test, allow.new.levels = TRUE)
resultados$m4_mixto_director <- mse(pred4, test$imdb_score)

## ---- Modelo 5: GAM con splines ----
f5 <- as.formula(paste("imdb_score ~ s(runtime) + s(release_year) + s(imdb_votes) + type +",
                        "age_certification_agr + pais_agrupado +", generos_formula))
m5 <- gam(f5, data = train)
resultados$m5_gam_splines <- mse(predict(m5, test), test$imdb_score)

## ---- Modelo 6: GAM con splines + efecto aleatorio de pais ----
train$pais_principal <- factor(train$pais_principal)
test$pais_principal  <- factor(test$pais_principal, levels = levels(train$pais_principal))
f6 <- as.formula(paste("imdb_score ~ s(runtime) + s(release_year) + s(imdb_votes) + type +",
                        "age_certification_agr +", generos_formula, "+ s(pais_principal, bs = 're')"))
m6 <- gam(f6, data = train)
pred6 <- predict(m6, newdata = test)
pred6[is.na(pred6)] <- mean(train$imdb_score)  # paises nuevos -> promedio global
resultados$m6_gam_re_pais <- mse(pred6, test$imdb_score)

cat("=== MSE de testeo (75/25 sobre train) ===\n")
tabla <- data.frame(modelo = names(resultados), MSE = unlist(resultados))
tabla <- tabla[order(tabla$MSE), ]
print(tabla, row.names = FALSE)

cat("\n=== ICC del modelo mixto por pais (m3) ===\n")
vc <- as.data.frame(VarCorr(m3))
tau2 <- vc$vcov[vc$grp == "pais_principal"]
sigma2 <- vc$vcov[vc$grp == "Residual"]
cat("tau2:", tau2, " sigma2:", sigma2, " ICC:", tau2 / (tau2 + sigma2), "\n")

cat("\n=== ICC del modelo mixto por director (m4) ===\n")
vc4 <- as.data.frame(VarCorr(m4))
tau2_4 <- vc4$vcov[vc4$grp == "director_principal"]
sigma2_4 <- vc4$vcov[vc4$grp == "Residual"]
cat("tau2:", tau2_4, " sigma2:", sigma2_4, " ICC:", tau2_4 / (tau2_4 + sigma2_4), "\n")

cat("\n=== summary GAM m6 (edf por termino) ===\n")
print(summary(m6))
