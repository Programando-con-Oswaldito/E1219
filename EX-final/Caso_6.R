library(tidyverse)
library(rio)

suscripciones <-
  import("base_suscripciones.csv", sheet = "base_suscripciones")

suscripciones_corregido <- suscripciones %>%
  mutate(id_suscripcion = ifelse(id_suscripcion < 0, id_suscripcion * (-1), id_suscripcion)) %>%
  mutate(id_cliente = ifelse(id_cliente < 0, id_cliente * (-1), id_cliente)) %>%
  mutate(nombre_cliente = ifelse(is.na(nombre_cliente), "No registrado", nombre_cliente)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_inicio),
                               as.character(as.Date(fecha_inicio, format = "%d-%m-%Y")),
                               fecha_inicio)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_inicio),
                               as.character(as.Date(fecha_inicio, format = "%Y-%m-%d")),
                               fecha_inicio)) %>%
  mutate(fecha_fin = case_when(
    estado == "Activa" ~ NA_character_,
    estado == "Desconocido" ~ "Desconocido",
    TRUE ~ as.character(as.Date(fecha_fin, format = "%Y-%m-%d"))
  )) %>%
  mutate(estado = ifelse(is.na(estado), "Desconocido", estado)) %>%
  mutate(temp_fecha_inicio = ifelse(fecha_inicio > fecha_fin & !is.na(fecha_fin), fecha_fin, fecha_inicio),
         temp_fecha_fin = ifelse(fecha_inicio > fecha_fin & !is.na(fecha_fin), fecha_inicio, fecha_fin)) %>%
  select(-fecha_inicio, -fecha_fin) %>%
  rename(fecha_inicio = temp_fecha_inicio, fecha_fin = temp_fecha_fin) %>%
  select(id_suscripcion, id_cliente, nombre_cliente, fecha_inicio, fecha_fin, estado)