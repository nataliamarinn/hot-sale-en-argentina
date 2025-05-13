# Librerías necesarias
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(tm)        # Stopwords
library(tidytext)  # Tokenización
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


# Cargar dataset de mails
hotsale <- read_csv("Mails Hot Sale 2024.csv")

hotsale <- hotsale %>%
  mutate(
    Fecha    = lubridate::dmy_hms(Fecha),
    Dia      = lubridate::as_date(Fecha),
    Hora     = lubridate::hour(Fecha),
    Semana   = lubridate::wday(Fecha, label = TRUE),
    Remitente = str_trim(str_remove(Remitente, "<.*>"))
  )


stopwords_es <- stopwords::stopwords("es")
stopwords_es <- stopwords_es[stopwords_es != "sin"] # quiero quedarme sin por cuotas sin interés

limpiar_texto <- function(texto, stopwords) {
  texto %>%
    stri_trans_general("Latin-ASCII") %>%
    str_remove_all("https?://\\S+|www\\.\\S+") %>%
    str_remove_all("\\S+@\\S+") %>%
    str_replace_all("[^\\w\\s$%#.,\\p{So}]", "") %>%
    str_to_lower() %>%
    str_split("\\s+") %>%
    lapply(function(palabras) palabras[!palabras %in% stopwords]) %>%
    sapply(paste, collapse = " ")
}


# Aplicar la limpieza al asunto y cuerpo del mail
hotsale <- hotsale %>%
  mutate(
    Asunto_limpio = limpiar_texto(Asunto, stopwords = stopwords_es),
    Texto_limpio  = limpiar_texto(`Texto del cuerpo`, stopwords = stopwords_es)
  )



# Normalizar expresiones frecuentes en el asunto del mail
hotsale <- hotsale %>%
  mutate(
    Asunto_limpio = str_replace_all(Asunto_limpio, "hot\\s+sale", "hot_sale"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "hot\\s+days", "hot_days"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "hot\\s+week", "hot_week"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "sin\\s+interes", "sin_interes"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "(\\d+)\\s+(cuotas?)", "\\1_\\2"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "(\\d+)%\\s+off", "\\1%_off"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "(\\d+)%", "\\1_%"),
    Asunto_limpio = str_replace_all(Asunto_limpio, "\\$(\\d+(?:[.,]\\d+)?)", "_\\1")
  )

# Tokenizar el asunto y contar palabras
frecuencias <- hotsale %>%
  unnest_tokens(palabra, Asunto_limpio, token = "words") %>%
  anti_join(data.frame(word = stopwords_es), by = c("palabra" = "word")) %>%
  count(palabra) %>%
  arrange(desc(n))

ggplot(frecuencias %>% slice_max(n, n = 10), aes(x = reorder(palabra, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 10 Palabras Más Frecuentes en los Asuntos",
    x = "Palabra",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Crear la nube de palabras (top 70)
top_70_palabras <- frecuencias %>%
  slice_max(n, n = 70) %>%
  mutate(
    palabra = str_replace_all(palabra, "hot\\s+sale", "hot_sale"),
    palabra = str_replace_all(palabra, "(\\d+)\\s*(cuotas?)", "\\1_cuotas"),
    palabra = str_replace_all(palabra, "(\\d+)%\\s*off", "\\1%_off"),
    palabra = str_replace_all(palabra, "(\\d+)_off", "\\1off"),
    palabra = str_replace_all(palabra, "_off", "off"),
    palabra = str_replace_all(palabra, "(1[1-9])_", "\\1%"),
    palabra = str_replace_all(palabra, "(\\d{2,})_", "\\1%"),
    palabra = str_replace_all(palabra, "12%cuotas", "12_cuotas")
  )

wordcloud(
  words = top_70_palabras$palabra,
  freq = top_70_palabras$n,
  max.words = 50,
  random.order = FALSE,
  colors = rep("#861a58", 70),
  scale = c(5, 0.5),
  rot.per = 0,
  background.color = "transparent",
  vfont = c("sans serif", "plain"),
  family = "Arial",
  random.color = FALSE,
  min.freq = 2,
  fixed.asp = TRUE
)


hotsale <- hotsale %>%
  mutate(
    Texto_limpio = str_replace_all(Texto_limpio, "envio\\s+gratis", "envio_gratis"),
    Texto_limpio = str_replace_all(Texto_limpio, "(\\d+)\\s*(cuotas?)", "\\1_cuotas"),
    Texto_limpio = str_replace_all(Texto_limpio, "(\\d+)%\\s*(off)", "\\1%_off"),
    Texto_limpio = str_replace_all(Texto_limpio, "2\\s*x\\s*1", "2x1")
  )


# Función auxiliar para contar ocurrencias de términos clave
contar_terminos <- function(terminos, columna) {
  tibble(
    termino = terminos,
    cantidad = sapply(terminos, function(x) sum(str_count(columna, fixed(x))))
  )
}

descuentos_clasicos <- c(
  "50%_off", "55%_off", "60%_off", "30%_off", "80%_off", "70%_off",
  "40%_off", "35%_off", "25%_off", "20%_off", "15%_off", "10%_off", "5%_off"
)
conteo_descuentos <- contar_terminos(descuentos_clasicos, hotsale$Texto_limpio)


promos_adicionales <- c("envio_gratis", "sin cargo desde", "2x1", "reintegro", "envio desde", "24_cuotas")
conteo_promos <- contar_terminos(promos_adicionales, hotsale$Texto_limpio)
