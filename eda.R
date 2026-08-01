library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidytext)

titles  <- read.csv("titles_train.csv")
credits <- read.csv("credits_train.csv")

## ---- Helper: parsear listas tipo Python ['a', 'b'] -> vector de strings ----
parsear_lista <- function(x) {
  x <- gsub("\\[|\\]|'", "", x)
  x <- str_split(x, ",\\s*")
  lapply(x, function(v) v[v != ""])
}

titles$genres_lista <- parsear_lista(titles$genres)
titles$paises_lista <- parsear_lista(titles$production_countries)
titles$pais_principal <- sapply(titles$paises_lista, function(v) if (length(v) == 0) NA else v[1])
titles$n_generos <- lengths(titles$genres_lista)
titles$n_paises  <- lengths(titles$paises_lista)

# ============================================================
# 1a. Genero vs score
# ============================================================
scores_genero <- titles %>%
  select(id, imdb_score, genres_lista) %>%
  unnest_longer(genres_lista, values_to = "genero") %>%
  filter(!is.na(genero), genero != "")

resumen_genero <- scores_genero %>%
  group_by(genero) %>%
  summarise(n = n(), media = mean(imdb_score), mediana = median(imdb_score), .groups = "drop") %>%
  arrange(desc(media))

cat("=== Score medio por genero (ordenado desc) ===\n")
print(resumen_genero, n = 30)

p_genero <- resumen_genero %>%
  mutate(genero = reorder(genero, media)) %>%
  ggplot(aes(x = genero, y = media)) +
  geom_col(fill = "#2A9D8F") +
  geom_hline(yintercept = mean(titles$imdb_score), linetype = "dashed", color = "grey40") +
  coord_flip() +
  labs(title = "Score medio de IMDB por genero", x = NULL, y = "Score medio",
       subtitle = "Linea punteada: promedio global") +
  theme_minimal()
ggsave("eda_genero.png", p_genero, width = 7, height = 5, dpi = 120)

# ============================================================
# 1b. Actor / director vs score
# ============================================================
datos_credits <- credits %>% inner_join(titles %>% select(id, imdb_score), by = "id")

resumen_personas <- datos_credits %>%
  group_by(name, role) %>%
  summarise(cantidad = n_distinct(id), promedio = mean(imdb_score), mediana = median(imdb_score), .groups = "drop")

cat("\n=== Top 10 directores (>=5 titulos) por promedio ===\n")
resumen_personas %>% filter(role == "DIRECTOR", cantidad >= 5) %>% arrange(desc(promedio)) %>% head(10) %>% print()

cat("\n=== Bottom 10 directores (>=5 titulos) por promedio ===\n")
resumen_personas %>% filter(role == "DIRECTOR", cantidad >= 5) %>% arrange(promedio) %>% head(10) %>% print()

cat("\n=== Top 10 actores (>=5 titulos) por promedio ===\n")
resumen_personas %>% filter(role == "ACTOR", cantidad >= 5) %>% arrange(desc(promedio)) %>% head(10) %>% print()

cat("\n=== Bottom 10 actores (>=5 titulos) por promedio ===\n")
resumen_personas %>% filter(role == "ACTOR", cantidad >= 5) %>% arrange(promedio) %>% head(10) %>% print()

# ============================================================
# 1c. Palabras de titulo / descripcion vs score
# ============================================================
data("stop_words")

palabras_desc <- titles %>%
  select(id, imdb_score, description) %>%
  unnest_tokens(palabra, description) %>%
  anti_join(stop_words, by = c("palabra" = "word")) %>%
  filter(!str_detect(palabra, "^[0-9]+$"))

resumen_palabras_desc <- palabras_desc %>%
  group_by(palabra) %>%
  summarise(n = n(), media = mean(imdb_score), .groups = "drop") %>%
  filter(n >= 30) %>%
  arrange(desc(media))

cat("\n=== Top 15 palabras de descripcion (n>=30) por score medio ===\n")
print(head(resumen_palabras_desc, 15))
cat("\n=== Bottom 15 palabras de descripcion (n>=30) por score medio ===\n")
print(tail(resumen_palabras_desc, 15))

palabras_titulo <- titles %>%
  select(id, imdb_score, title) %>%
  unnest_tokens(palabra, title) %>%
  anti_join(stop_words, by = c("palabra" = "word")) %>%
  filter(!str_detect(palabra, "^[0-9]+$"))

resumen_palabras_titulo <- palabras_titulo %>%
  group_by(palabra) %>%
  summarise(n = n(), media = mean(imdb_score), .groups = "drop") %>%
  filter(n >= 15) %>%
  arrange(desc(media))

cat("\n=== Top 15 palabras de titulo (n>=15) por score medio ===\n")
print(head(resumen_palabras_titulo, 15))
cat("\n=== Bottom 15 palabras de titulo (n>=15) por score medio ===\n")
print(tail(resumen_palabras_titulo, 15))

# ============================================================
# Otras relaciones utiles para el modelado
# ============================================================
cat("\n=== Correlaciones numericas con imdb_score ===\n")
cat("runtime:      ", cor(titles$runtime, titles$imdb_score), "\n")
cat("release_year: ", cor(titles$release_year, titles$imdb_score), "\n")
cat("imdb_votes:   ", cor(titles$imdb_votes, titles$imdb_score, use = "complete.obs"), "\n")
cat("n_generos:    ", cor(titles$n_generos, titles$imdb_score), "\n")
cat("n_paises:     ", cor(titles$n_paises, titles$imdb_score), "\n")

cat("\n=== Score medio por tipo (movie/show) ===\n")
titles %>% group_by(type) %>% summarise(n = n(), media = mean(imdb_score)) %>% print()

cat("\n=== Score medio por certificacion de edad ===\n")
titles %>% mutate(age_certification = ifelse(age_certification == "", "Desconocida", age_certification)) %>%
  group_by(age_certification) %>% summarise(n = n(), media = mean(imdb_score)) %>% arrange(desc(media)) %>% print()
