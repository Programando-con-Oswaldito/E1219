library(tidyverse)
library(rio)

camapanias <-
  import("base_campañas.csv", sheet = "base_campañas")

Camapanias_corregido <- camapanias %>%
  mutate(id_campaña = ifelse(id_campaña < 0, id_campaña * (-1), id_campaña)) %>%
  mutate(nombre_campaña = ifelse(is.na(nombre_campaña), "No registrado", nombre_campaña)) %>%
  mutate(plataforma = ifelse(is.na(plataforma), "Desconocido", plataforma)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_inicio),
                              as.character(as.Date(fecha_inicio, format = "%d-%m-%Y")),
                              fecha_inicio)) %>%
  mutate(fecha_inicio = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_inicio),
                              as.character(as.Date(fecha_inicio, format = "%Y-%m-%d")),
                              fecha_inicio)) %>%
  mutate(presupuesto = ifelse(is.na(presupuesto), -1, presupuesto))