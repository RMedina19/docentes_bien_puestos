#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados de la primera vuelta
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          27 de agosto de 2021
# Última actualización:       27 de agosto de 2021
#------------------------------------------------------------------------------#


# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp <- "02_datos_crudos/"
out <- "03_datos_limpios/"


# 1. Cargar datos --------------------------------------------------------------

df_codebook     <- read_excel("02_datos_crudos/df_codebook.xls")
df_crudo        <- read_excel("02_datos_crudos/df_respuestas_text_v1.xlsx")

# Explorar datos 
View(df_crudo)
names(df_crudo)
table(df_crudo$SbjNam)
table(df_crudo$id)

dim(df_crudo)


# 2. Procesar datos------------------------------------------------------------


# v_names <- names(df_codebook_crudo)
# 
# # Guardar el texto de las preguntas
# df_preguntas <- df_codebook_crudo       %>% 
#     rename(
#         q_id   = v_names[2], 
#         a_id   = v_names[4], 
#         q_text = v_names[5])            %>% 
#     filter(is.na(a_id), q_id %in% 1:53) %>% 
#     select(q_id, q_text)                %>% 
#     # Quitar indicaciones y código html
#     mutate(
#         q_text = str_replace_all(q_text, "<b>"    , ""), 
#         q_text = str_replace_all(q_text, "</b>"   , ""), 
#         q_text = str_replace_all(q_text, "<p>"    , ""), 
#         q_text = str_replace_all(q_text, "</p>"   , ""), 
#         q_text = str_replace_all(q_text, "<br>"   , ""),  
#         q_text = str_replace_all(q_text, "<div>"  , ""), 
#         q_text = str_replace_all(q_text, "</div>" , ""), 
#         q_text = str_replace_all(q_text, "<font color=\"#000000\">", ""), 
#         q_text = str_replace_all(q_text, "<font color=\"#008000\">", ""), 
#         q_text = str_replace_all(q_text, "<font color=\"#0000FF\">", ""), 
#         q_text = str_replace_all(q_text, "</font>", ""), 
#         q_text = str_replace_all(q_text, "&nbsp;" , " "))
# 
# # Guardar el texto de las opciones de respuesta
# # Guardar el texto de las respuestas
# df_respuestas     <- df_codebook_crudo    %>% 
#     rename(
#         q_id   = v_names[2], 
#         a_id   = v_names[4], 
#         a_text = v_names[5])            %>% 
#     select(q_id, a_id, a_text)          %>% 
#     filter(
#         !is.na(q_id), 
#         !is.na(a_id))


# Resultados 
df_limpio <- df_crudo           %>% 
    filter(SbjNam == "campo1")  %>% 
    select(c(Date, id:comentario))

v_names <- names(df_limpio)

# Limpieza de ejemplo 
df_q0 <- df_limpio                  %>% 
    rename(respuestas = v_names[1]) %>% 
    select(respuestas)              %>% 
    group_by(respuestas)            %>% 
    summarise(total = n())          %>% 
    mutate(
        q_id = 0, 
        t_id = NA)                  %>% 
    select(q_id, t_id, respuestas, total)

# Base vacía 
df_resultados <- data.frame(q_id = NA, t_id = NA, respuestas = NA, total = NA)

# 
for (i in 2:length(v_names)){
    
    # Procesar datos
    df_q <- df_limpio                   %>% 
        rename(respuestas = v_names[i]) %>% 
        select(respuestas)              %>% 
        group_by(respuestas)            %>% 
        summarise(total = n())          %>% 
        mutate(
            q_id = i, 
            t_id = NA)                  %>% 
        select(q_id, t_id, respuestas, total)
    
    df_resultados <- df_resultados %>% bind_rows(df_resultados)
}

View(df_resultados)
