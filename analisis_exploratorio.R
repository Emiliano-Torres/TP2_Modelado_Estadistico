library(ggplot2)
library(dplyr)
library(jsonlite)


### Analisis explotarorio de datos

titles_train<- read.csv("titles_train.csv")

credits_train<- read.csv("credits_train.csv")

titles_train[1,]

summary(titles_train)

summary(credits_train)

View(titles_train)

View(credits_train)


#hay votos y puntaje como promedio de votos
titles_train$imdb_score = as.numeric(titles_train$imdb_score)
titles_train$imdb_votes = as.numeric(titles_train$imdb_votes)



cor(x=titles_train$imdb_votes , y=titles_train$imdb_score,use="complete.obs")

# extraemos todos los generos


titles_train$genres <- lapply(
  gsub("'", "\"", titles_train$genres),
  fromJSON
)
titles_train$genres <- lapply(titles_train$genres, fromJSON)

str(titles_train$genres)


generos <- unique(unlist(titles_train[[8]]))


scores <- data.frame(
  genero = unlist(titles_train$genres),
  imdb_score = rep(titles_train$imdb_score, lengths(titles_train$genres))
)

generos <- unique(scores$genero)

ggplot(scores, aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ genero) 

ggplot(scores, aes(x = genero, y = imdb_score)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución de IMDb Score por género",
    x = "Género",
    y = "IMDb Score"
  ) +
  theme_minimal()

#la verdad que siento que no especialmente

ggplot(scores[scores$genero=='history',], aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ genero)




datos_credits <- merge(
  credits_train,
  titles_train,
  by = "id"
)


resumen_personas <- datos_credits %>%
  filter(!is.na(imdb_score)) %>%
  group_by(name, role) %>%
  summarise(
    cantidad = n_distinct(id),
    promedio_imdb = mean(imdb_score, na.rm = TRUE),
    mediana_imdb = median(imdb_score, na.rm = TRUE),
    .groups = "drop"
  )

actores_validos <- resumen_personas %>%
  filter(role == "ACTOR", cantidad >= 10) %>%
  arrange(desc(promedio_imdb)) %>%
  #slice_head(n = 15) %>%
  pull(name)


datos_credits %>%
  filter(
    role == "ACTOR",
    name %in% actores_validos,
    !is.na(imdb_score)
  ) %>%
  ggplot(aes(
    x = reorder(name, imdb_score, FUN = median),
    y = imdb_score
  )) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución de puntajes IMDb por actor",
    subtitle = "15 actores con mayor promedio y al menos 10 títulos",
    x = "Actor",
    y = "IMDb score"
  ) +
  theme_minimal()

directores_validos <- resumen_personas %>%
  filter(role == "DIRECTOR", cantidad >= 5) %>%
  arrange(desc(promedio_imdb)) %>%
  #slice_head(n = 15) %>%
  pull(name)


datos_credits %>%
  filter(
    role == "DIRECTOR",
    name %in% directores_validos,
    !is.na(imdb_score)
  ) %>%
  ggplot(aes(
    x = reorder(name, imdb_score, FUN = median),
    y = imdb_score
  )) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución de puntajes IMDb por director",
    subtitle = "15 directores con mayor promedio y al menos 5 títulos",
    x = "Director",
    y = "IMDb score"
  ) +
  theme_minimal()


