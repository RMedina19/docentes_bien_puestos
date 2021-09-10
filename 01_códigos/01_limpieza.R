#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados de la primera vuelta
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          27 de agosto     de 2021
# Última actualización:       10 de septiembre de 2021
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
df_crudo        <- read_excel("02_datos_crudos/df_respuestas_text_v1.xlsx")

# Explorar datos 
# View(df_crudo)
names(df_crudo)
table(df_crudo$SbjNam)
table(df_crudo$id)
dim(df_crudo)

# View(df_codebook)


# 2. Procesar datos-------------------------------------------------------------

## 2.1. Limpiar el libro códigos -----------------------------------------------

# Nombre de las variables crudas 
v_names <- names(df_codebookr)

# Guardar el texto de las preguntas
df_preguntas <- df_codebookr            %>%
    rename(
        q_id   = v_names[2],
        a_id   = v_names[4],
        q_text = v_names[5])            %>%
    filter(is.na(a_id), q_id %in% 1:53) %>%
    select(q_id, q_text)                %>%
    # Quitar indicaciones y código html
    mutate(
        q_text = str_replace_all(q_text, "<b>"    , ""),
        q_text = str_replace_all(q_text, "</b>"   , ""),
        q_text = str_replace_all(q_text, "<p>"    , ""),
        q_text = str_replace_all(q_text, "</p>"   , ""),
        q_text = str_replace_all(q_text, "<br>"   , ""),
        q_text = str_replace_all(q_text, "<div>"  , ""),
        q_text = str_replace_all(q_text, "</div>" , ""),
        q_text = str_replace_all(q_text, "<font color=\"#000000\">", ""),
        q_text = str_replace_all(q_text, "<font color=\"#008000\">", ""),
        q_text = str_replace_all(q_text, "<font color=\"#0000FF\">", ""),
        q_text = str_replace_all(q_text, "</font>", ""),
        q_text = str_replace_all(q_text, "&nbsp;" , " "))

# Guardar el texto de los temas     
df_tematicas <- df_codebookr                %>% 
    filter(`Is Topic` == T)                 %>% 
    rename(
        q_id   = v_names[2],
        t_id   = v_names[4],
        t_text = v_names[5])                %>%
    select(q_id, t_id, t_text)              %>%
    filter( 
        !is.na(q_id),   
        !is.na(t_id))   
    
# Guardar el texto de las respuestas    
df_respuestas     <- df_codebookr           %>%
    filter(`Is Topic` == F)                 %>% 
    rename( 
        q_id   = v_names[2],    
        a_id   = v_names[4],    
        a_text = v_names[5])                %>%
    select(q_id, a_id, a_text)              %>%
    filter(
        !is.na(q_id),
        !is.na(a_id))


# Unir codebook limpio 
df_codebook <- df_preguntas                 %>% 
    left_join(df_tematicas,  by = "q_id")   %>% 
    left_join(df_respuestas, by = "q_id")   %>% 
    # Crear un identificador para las combinaciones posibles (pregunta + tema)
    mutate(c_id = paste(
        q_id, t_id, sep = "-"))             %>% 
    filter(c_id != "18-NA")

length(unique(df_codebook$c_id))

## 2.2. Limpiar los resultados -------------------------------------------------

# Resultados 
df_limpio <- df_crudo                       %>% 
    filter(SbjNam == "campo1")              %>% 
    select(c(Date, id:comentario))

v_names <- names(df_limpio)
v_names <- v_names[v_names != "nivel_estudios_otro"]

# Limpieza de ejemplo 
df_q0 <- df_limpio                          %>% 
    rename(respuestas = v_names[1])         %>% 
    select(respuestas)                      %>% 
    group_by(respuestas)                    %>% 
    summarise(total = n())                  %>% 
    mutate(     
        q_id = 0,       
        t_id = NA)                          %>% 
    select(q_id, t_id, respuestas, total)

# Base vacía 
df_respuestas <- data.frame(q_code = NA, a_text = NA, total = NA)


# Unir todas las preguntas procesadas por frecuencia
for (i in 2:length(v_names)){

    # Procesar datos
    df_q <- df_limpio                       %>% 
        rename(a_text = v_names[i])         %>% 
        select(a_text)                      %>% 
        group_by(a_text)                    %>% 
        summarise(total = n())              %>% 
        mutate( 
            q_code = v_names[i])            %>% 
        mutate_all(as.character)            %>% 
        select(q_code, a_text, total)
    
    df_respuestas <- df_respuestas %>% bind_rows(df_q) %>% filter(!is.na(q_code))
}

# View(df_respuestas)


## 2.3. Unir codebook con frecuencia de respuestas -----------------------------

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

    
# 3. Guardar datos -------------------------------------------------------------

# Renombrar base
df_resultados_frecuencias <- df_resultados 

# Guardar en formato R 
save(df_resultados_frecuencias, 
    file = "03_datos_limpios/df_resultados_frecuencias.RData")

# Guardar en formato xlsx
df_formato_xlsx  <- df_resultados %>% 
    rename(
        `Número de la pregunta` = q_id, 
        `Código de pregunta`    = q_code, 
        `Pregunta`              = q_text, 
        `Temática`              = t_text, 
        `Respuesta`             = a_text, 
        `Frecuencia`            = total, 
        `Porcentaje (numérico)` = porcentaje, 
        `Porcentaje`            = p_text
    ) 

openxlsx::write.xlsx(df_formato_xlsx, 
    "03_datos_limpios/df_resultados_frecuencias.xlsx")

# FIN --------------------------------------------------------------------------    
    