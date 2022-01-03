#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados antes de la intervención
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     regina.medina@alumnos.cide.edu
# Fecha de creación:          27 de agosto    de 2021
# Última actualización:       01 de diciembre de 2021
#------------------------------------------------------------------------------#

# Pendientes: 
#  - Guardar escala de En Desacuerdo/De acuerdo como tipo factor 
#  - Dividir análisis por escuelas 


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
df_crudo        <- read_excel("02_datos_crudos/df_pre_final.xlsx")

# Explorar datos 
# View(df_crudo)
names(df_crudo)
table(df_crudo$SbjNam)
table(df_crudo$id)
dim(df_crudo)

# View(df_codebook)

# Ver que los códigos de identificación única no se repitan
table(table(df_crudo$id)) 

v_id_rep <- unique((as.data.frame(table(df_crudo$id))   %>% 
        filter(Freq > 1))$Var1)

# Hay 10 códigos que se repiten 2 veces
# Hay 2  códigos que se repiten 3 veces

# Ver si los códigos repetidos se tratan de la misma persona 
df_id_rep <- df_crudo                                   %>% 
    filter(id %in% v_id_rep, 
        !(SbjNam %in% c("piloto", "STGTester")))        %>% 
    select(Date, id:cap_inst, -consent)
    
# Casi todos los id son de la misma persona (pero son el antes y el despúes 
# del CEDROS). Solo los códigos de 45 y 50 parecen haber sido utilizados por
# personas distintas (así que representan 4 personas, todas del Cecytea)

v_cedros_rep <- c(3, 8, 11, 13, 15, 16, 17, 20, 23, 24)
v_cecyte_rep <- c(45, 50)

# 2. Procesar datos-------------------------------------------------------------

## 2.1. Limpiar el libro códigos -----------------------------------------------

# Nombre de las variables crudas 
v_names <- names(df_codebookr)

# Guardar el texto de las preguntas
df_preguntas <- df_codebookr                    %>%
    rename(     
        q_id   = v_names[2],        
        a_id   = v_names[4],        
        q_text = v_names[5])                    %>%
    filter(is.na(a_id), q_id %in% 1:53)         %>%
    select(q_id, q_text)                        %>%
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
        q_text = str_replace_all(q_text, "&nbsp;" , " "), 
        # Abreviar pregunta de consentimiento informado
        q_text = ifelse(str_detect(q_text, "Confidencialidad"), 
            "¿Acepta participar en la encuesta?", q_text), 
        # Abreviar pregunta abierta para comentarios
        q_text = ifelse(str_detect(q_text, "enormemente"), 
            "Sección abierta para comentarios finales", q_text)
        )

# Guardar el texto de los temas     
df_tematicas <- df_codebookr                    %>% 
    filter(`Is Topic` == T)                     %>% 
    rename(
        q_id   = v_names[2],
        t_id   = v_names[4],
        t_text = v_names[5])                    %>%
    select(q_id, t_id, t_text)                  %>%
    filter( 
        !is.na(q_id),   
        !is.na(t_id))   
    
# Guardar el texto de las respuestas    
df_respuestas     <- df_codebookr               %>%
    filter(`Is Topic` == F)                     %>% 
    rename(     
        q_id   = v_names[2],        
        a_id   = v_names[4],        
        a_text = v_names[5])                    %>%
    select(q_id, a_id, a_text)                  %>%
    filter( 
        !is.na(q_id),   
        !is.na(a_id))   
    
    
# Unir codebook limpio  
df_codebook <- df_preguntas                     %>% 
    left_join(df_tematicas,  by = "q_id")       %>% 
    left_join(df_respuestas, by = "q_id")       %>% 
    # Crear un identificador para las combinaciones posibles (pregunta + tema)
    mutate(c_id = paste(
        q_id, t_id, sep = "-"))                 %>% 
    filter(c_id != "18-NA")

length(unique(df_codebook$c_id))


## 2.2. Limpiar los resultados -------------------------------------------------

# Resultados 
df_limpio <- df_crudo                                       %>% 
    filter(
        # Eliminar toda la información de los pilotos 
        SbjNam == "campo1", 
        # Quitar las respuestas de los profesores de cedros después de la intrv.
        !(Date > "2021-10-26" & (id %in% v_cedros_rep)))    %>% 
    # Seleccionar variables relevantes
    select(c(Date, id:comentario))                          %>% 
    mutate(
        # Homologar respuestas de la institución donde trabajan 
        escuela = case_when(
            str_detect(escuela, "edros")             ~ "Colegio Cedros", 
            grepl("cecyt", escuela, ignore.case = T) ~ "Cecytea Rincón de Romos", 
            T ~ escuela), 
        # Cambiar ids repetidos (coincidió que en cada par había una sola mujer)
        id = ifelse((id %in% v_cecyte_rep & sexo == "Mujer"), id+100, id))                                     

# Guardar los datos del cedros recopilados despúes de la intervención    
df_limpio_expost_cedros <- df_crudo                         %>% 
    filter((Date > "2021-10-26" & (id %in% v_cedros_rep)))  %>% 
    select(c(Date, id:comentario))                          %>% 
    mutate(escuela = "Colegio Cedros")    


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
df_resultados_exante_microdatos  <- df_limpio       %>% 
    mutate(momento = "Previo a intervención")       %>% 
    select(momento, everything())
    
# Frecuencias de los resultados 
df_resultados_exante_frecuencias <- df_resultados   %>% 
    mutate(momento = "Previo a intervención")       %>% 
    select(momento, everything()) 

# Guardar en formato R 
save(df_resultados_exante_microdatos, 
    file = "03_datos_limpios/df_resultados_exante_microdatos.RData")

save(df_resultados_exante_frecuencias, 
    file = "03_datos_limpios/df_resultados_exante_frecuencias.RData")

# Renombrar para formato xlsx
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

# Guardar en formato xlsx
openxlsx::write.xlsx(df_resultados_exante_microdatos, 
    file = "03_datos_limpios/df_resultados_exante_microdatos.xlsx",
    overwrite = T)

openxlsx::write.xlsx(df_formato_xlsx, 
    "03_datos_limpios/df_resultados_exante_frecuencias.xlsx", overwrite = T)


# Guardar microdatos del colegio cedros post intervención 
save(df_limpio_expost_cedros, 
    file = "03_datos_limpios/df_limpio_expost_cedros.RData")

# Guardar codebook
save(df_codebook, file = "03_datos_limpios/df_codebook.RData")

# FIN --------------------------------------------------------------------------    
