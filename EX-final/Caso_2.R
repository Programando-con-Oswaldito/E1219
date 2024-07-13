library(tidyverse)
library(rio)

envios <- import("base_envíos.csv", sheet = "base_envíos")

Envios_corregido <- envios %>%
  mutate(id_envío = ifelse(id_envío < 0, id_envío * (-1), id_envío)) %>%
  mutate(origen = ifelse(is.na(origen), "No registrado", origen)) %>%
  mutate(destino = ifelse(is.na(destino), "No reportado", destino)) %>%
  mutate(fecha_envío = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_envío),
                              as.character(as.Date(fecha_envío, format = "%d-%m-%Y")),
                              fecha_envío)) %>%
  mutate(fecha_envío = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_envío),
                              as.character(as.Date(fecha_envío, format = "%Y-%m-%d")),
                              fecha_envío)) %>%
  mutate(fecha_entrega = ifelse(is.na(fecha_entrega), as.character(fecha_entrega), as.character(as.Date(fecha_entrega, format = "%d-%m-%Y")))) %>%
  mutate(monto_envío = ifelse(is.na(monto_envío), -1, monto_envío)) %>%
  mutate(temp_fecha_envio = ifelse(!is.na(fecha_entrega) & as.Date(fecha_envío) > as.Date(fecha_entrega), as.character(as.Date(fecha_entrega)), as.character(as.Date(fecha_envío))),
         fecha_entrega = ifelse(!is.na(fecha_entrega) & as.Date(fecha_envío) > as.Date(fecha_entrega), as.character(as.Date(fecha_envío)), as.character(fecha_entrega)),
         fecha_envío = temp_fecha_envio) %>%
  select(-temp_fecha_envio)