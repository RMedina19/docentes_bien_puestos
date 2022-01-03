#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados antes de la intervención
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     regina.medina@alumnos.cide.edu
# Fecha de creación:          01 de diciembre de 2021
# Última actualización:       01 de diciembre de 2021
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

df_codebookr    <- read_excel("02_datos_crudos/df_codebook.xls")
df_crudo        <- read_excel("02_datos_crudos/df_post_final.xlsx")

# Cargar los datos del cedros 
load(paste0(out, "df_limpio_expost_cedros.RData"))

# Cargar coodebook 
load(paste0(out, "df_codebook.RData"))

# Explorar datos 
# View(df_crudo)
names(df_crudo)
table(df_crudo$SbjNam)
table(df_crudo$id)
dim(df_crudo)

# 2. Procesar datos-------------------------------------------------------------

## 2.1. Limpiar los resultados -------------------------------------------------

df_cedros <- df_limpio_expost_cedros                        %>% 
    select(-c(estudios:cap_inst))

# IDs repetidos del CECYTEA (identificado en limpieza exante)
v_cecyte_rep <- c(45, 50)

# Resultados 
df_limpio <- df_crudo                                       %>% 
    # Eliminar toda la información de los pilotos 
    filter(SbjNam == "participantes_salida")                %>% 
    # Hay una observación con ID repetido y no coincide con los datos previos 
    filter(!(id == 25 & edad == 57))                        %>% 
    # Seleccionar variables relevantes
    select(c(Date, id:comentario, -starts_with("T_")))      %>% 
    # Cambiar ids repetidos 
    mutate(id = ifelse((id %in% v_cecyte_rep & sexo == "Mujer"), 
        id+100, id))                                        %>% 
    # Juntar respuestas del CEDROS 
    bind_rows(df_cedros)
    
# Guardar nombres de variables relevantes
v_names <- names(df_limpio)
v_names <- v_names[v_names != "nivel_estudios_otro"]

# Limpieza de ejemplo 
df_q0 <- df_limpio                              %>% 
    rename(respuestas = v_names[1])             %>% 
    select(respuestas)                          %>% 
    group_by(respuestas)                        %>% 
    summarise(total = n())                      %>% 
    mutate(         
        q_id = 0,           
        t_id = NA)                              %>% 
    select(q_id, t_id, respuestas, total)

# Base vacía 
df_respuestas <- data.frame(q_code = NA, a_text = NA, total = NA)

# Unir todas las preguntas procesadas por frecuencia
for (i in 2:length(v_names)){
    
    # Procesar datos
    df_q <- df_limpio                           %>% 
        rename(a_text = v_names[i])             %>% 
        select(a_text)                          %>% 
        group_by(a_text)                        %>% 
        summarise(total = n())                  %>% 
        mutate(     
            q_code = v_names[i])                %>% 
        mutate_all(as.character)                %>% 
        select(q_code, a_text, total)
    
    df_respuestas <- df_respuestas              %>% 
        bind_rows(df_q)                         %>% 
        filter(!is.na(q_code))
}

# View(df_respuestas)

## 2.3. Unir codebook con frecuencia de respuestas -----------------------------

# Quitar del codebook las variables de preguntas de la primevera vuelta
df_codebook <- df_codebook %>% filter(!(q_id %in% c(5:10)))

# Crear una base con los nombres de las variables y los identificadores para 
# poder unir los dos data frames
v_combination   <- unique(df_codebook$c_id)
v_variables     <- unique(df_respuestas$q_code)

# length(v_combination)
# length(v_variables)

df_index = data.frame(c_id = v_combination, q_code = v_variables)
# View(df_index)

# Unir bases de codebook y respuestas y pulir 
df_resultados <- df_codebook                    %>% 
    select(-c(a_id, a_text))                    %>% 
    # Agregar los índices en común 
    left_join(df_index, by = "c_id")            %>% 
    # Unir con respuestas
    full_join(df_respuestas, by = c("q_code"))  %>% 
    # Eliminar respuestas duplicadas
    distinct()                                  %>% 
    # Remplazar respuestas -1 por "No aplica" 
    mutate(a_text = ifelse(
        a_text == "-1", "No aplica", a_text))   %>% 
    # Calcular porcentajes 
    group_by(q_code)                            %>% 
    mutate(
        total      = as.numeric(total),
        porcentaje = total/sum(total), 
        p_text     = paste0(
            round(porcentaje*100, 1), "%"))     %>% 
    # Seleccionar variables finales 
    select(q_id, q_code, q_text, t_text, a_text, total, porcentaje, p_text)

# Controles de calidad 
df_freq_total <- df_resultados                  %>% 
    group_by(q_code)                            %>% 
    summarise(total_answers = sum(total))   

table(df_freq_total$total_answers) # Todas suman 13, así que es correcto 

# View(df_resultados)

table(table(df_limpio$id)) # Ya no hay ningún id repetido

# 3. Guardar datos -------------------------------------------------------------

# Microdatos limpios
df_resultados_expost_microdatos  <- df_limpio %>% 
    mutate(momento = "Posterior a intervención") %>% 
    select(momento, everything())

# Frecuencias de los resultados 
df_resultados_expost_frecuencias <- df_resultados %>% 
    mutate(momento = "Posterior a intervención") %>% 
    select(momento, everything()) 

# Guardar en formato RData 
save(df_resultados_expost_microdatos, 
    file = "03_datos_limpios/df_resultados_expost_microdatos.RData")

save(df_resultados_expost_frecuencias, 
    file = "03_datos_limpios/df_resultados_expost_frecuencias.RData")

# Guardar en formato xlsx
openxlsx::write.xlsx(df_resultados_expost_microdatos, 
    file = "03_datos_limpios/df_resultados_expost_microdatos.xlsx",
    overwrite = T)

openxlsx::write.xlsx(df_resultados_expost_frecuencias, 
    "03_datos_limpios/df_resultados_expost_frecuencias.xlsx", overwrite = T)



# FIN --------------------------------------------------------------------------    
