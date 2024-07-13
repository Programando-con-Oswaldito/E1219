library(tidyverse)
library(rio)

ventas <-
  import("base_ventas.csv", sheet = "base_ventas")


Ventas_corregido <- ventas %>%
  mutate(id_vendedor = ifelse(id_vendedor < 0, id_vendedor * (-1), id_vendedor)) %>%
  mutate(nombre_vendedor = ifelse(is.na(nombre_vendedor), "No identificado", nombre_vendedor)) %>%
  mutate(zona = ifelse(is.na(zona), "Desconocido", zona)) %>%
  mutate(fecha_venta = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_venta),
                              as.character(as.Date(fecha_venta, format = "%d-%m-%Y")),
                              fecha_venta)) %>%
  mutate(fecha_venta = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_venta),
                              as.character(as.Date(fecha_venta, format = "%Y-%m-%d")),
                              fecha_venta)) %>%
  mutate(monto_venta = ifelse(is.na(monto_venta), -1, monto_venta))


