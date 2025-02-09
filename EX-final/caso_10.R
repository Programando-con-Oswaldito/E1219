library(tidyverse)
library(rio)

registros_med <- import("base_registros_medicos.csv", sheet = "base_registros_medicos")

# Tabla con todas las transformaciones
registros_med_corregido <- registros_med %>%
  mutate(id_registro = ifelse(id_registro < 0, id_registro * (-1), id_registro)) %>%
  mutate(id_paciente = ifelse(id_paciente < 0, id_paciente * (-1), id_paciente)) %>%
  mutate(fecha_admision = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_admision),
                               as.character(as.Date(fecha_admision, format = "%d-%m-%Y")),
                               fecha_admision)) %>%
  mutate(fecha_admision = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_admision),
                               as.character(as.Date(fecha_admision, format = "%Y-%m-%d")),
                               fecha_admision)) %>%
  mutate(fecha_alta = case_when(
    estado == "Hospitalizado" ~ NA_character_,
    estado == "Desconocido" ~ "Desconocido",
    TRUE ~ as.character(as.Date(fecha_alta, format = "%Y-%m-%d"))
  )) %>%
  mutate(costo = ifelse(is.na(costo), -1, costo)) %>%
  mutate(diagnostico = ifelse(is.na(diagnostico), "No registrado", diagnostico)) %>%
  mutate(estado = ifelse(is.na(estado), "Desconocido", estado)) %>%
  mutate(temp_fecha_admision = ifelse(fecha_admision > fecha_alta & !is.na(fecha_alta), fecha_alta, fecha_admision),
         temp_fecha_alta = ifelse(fecha_admision > fecha_alta & !is.na(fecha_alta), fecha_admision, fecha_alta)) %>%
  select(-fecha_admision, -fecha_alta) %>%
  rename(fecha_admision = temp_fecha_admision, fecha_alta = temp_fecha_alta) 
 