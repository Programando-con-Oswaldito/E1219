library(tidyverse)
library(rio)

pedidos <- import("base_pedidos.csv", sheet = "base_pedidos")

pedidos_corregido <- pedidos %>%
  mutate(id_pedido = ifelse(id_pedido < 0, id_pedido * (-1), id_pedido)) %>%
  mutate(id_cliente = ifelse(id_cliente < 0, id_cliente * (-1), id_cliente)) %>%
  mutate(nombre_producto = ifelse(is.na(nombre_producto), "No registrado", nombre_producto)) %>%
  mutate(precio_unitario = ifelse(is.na(precio_unitario), "0", precio_unitario)) %>%
  mutate(fecha_pedido = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_pedido),
                              as.character(as.Date(fecha_pedido, format = "%d-%m-%Y")),
                              fecha_pedido)) %>%
  mutate(fecha_pedido = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_pedido),
                              as.character(as.Date(fecha_pedido, format = "%Y-%m-%d")),
                              fecha_pedido)) %>%
  mutate(fecha_envio = ifelse(is.na(fecha_envio), as.character(fecha_envio), as.character(as.Date(fecha_envio, format = "%d-%m-%Y")))) %>%
  mutate(temp_fecha_pedido = ifelse(!is.na(fecha_envio) & as.Date(fecha_pedido) > as.Date(fecha_envio), as.character(as.Date(fecha_envio)), as.character(as.Date(fecha_pedido))),
         fecha_envio = ifelse(!is.na(fecha_envio) & as.Date(fecha_pedido) > as.Date(fecha_envio), as.character(as.Date(fecha_pedido)), as.character(fecha_envio)),
         fecha_pedido = temp_fecha_pedido) %>%
  select(-temp_fecha_pedido) %>%
  mutate(estado_pedido = ifelse(is.na(estado_pedido), "Desconocido", estado_pedido)) %>%
  mutate(precio_unitario = ifelse(is.na(precio_unitario), -1, precio_unitario))
