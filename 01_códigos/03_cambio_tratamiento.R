#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Medir cambio después del tratamiento
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     regina.medina@alumnos.cide.edu
# Fecha de creación:          10 de diciembre de 2021
# Última actualización:       10 de diciembre de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp <- "03_datos_limpios/"
out <- "04_figuras/04_cambio_escuelas/"

# 1. Cargar datos --------------------------------------------------------------

load(paste0(inp, "df_resultados_exante_microdatos.RData"))
load(paste0(inp, "df_resultados_expost_microdatos.RData"))
load(paste0(inp, "df_codebook.RData"))

# ----- Tema para gráficas 
tema        <- theme_bw() + 
    theme(
        legend.position = "top", 
        plot.title.position = "plot") 

# ----- Escalas de valores 
v_vf        <- c("Verdadero", "Falso", "NS/NR")
v_acuerdo   <- c("Totalmente en desacuerdo", "En desacuerdo", 
    "Ni de acuerdo, ni en desacuerdo", "De acuerdo", "Totalmente de acuerdo", 
    "NS/NR")

# 2. Procesar datos ------------------------------------------------------------

# Microdatos de resultados previos al tratamiento
df_pre  <- df_resultados_exante_microdatos          %>% 
    select(-c(momento, Date, consent))              %>% 
    rename_at(
        vars(c_adiccion:comentario), function(x){paste0("pre-", x)}) %>% 
    filter(!is.na(escuela))

# Microdatos de resultados posteriores al tratamiento
df_post <- df_resultados_expost_microdatos          %>% 
    select(-c(edad, momento, Date, consent))        %>% 
    rename_at(
        vars(c_adiccion:comentario), function(x){paste0("post-", x)}) 


dim(df_pre)
dim(df_post)

# Unir bases
df_join <- df_pre %>% inner_join(df_post, by = c("id", "sexo")) %>% 
    filter(!is.na(escuela)) %>% 
    mutate()

dim(df_join)

# Cambiar a formato largo 
df_pivot <- df_join                                                     %>% 
    mutate_all(~as.character(.))                                        %>% 
    pivot_longer(
        cols = c("pre-c_adiccion":"post-comentario"),
        names_to = c("momento", "pregunta"), 
        names_sep = "-", 
        values_to = "respuesta") %>% 
    mutate(
        momento = case_when(
            momento == "pre"  ~ "Antes del curso", 
            momento == "post" ~ "Despúes del curso"), 
        respuesta = ifelse(respuesta == "Totalemente de acuerdo", 
            "Totalmente de acuerdo", respuesta)) 

v_qcode <- unique(df_pivot$pregunta)

#---- Pregunta 1 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[1])                                   %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text[df_codebook$q_id == 11 & df_codebook$a_id == 1])

# Crear vector con los textos finales de las preguntas 
df_new_codes <- data.frame(v_pnum = NA, v_qcode = NA, v_pregunta = NA)

df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 1, v_qcode = v_qcode[1], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent)


ggsave(filename = paste0(out, "01", ".png"), width = 6, height = 6)


#---- Pregunta 2 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[2])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text[df_codebook$q_id == 11 & df_codebook$a_id == 2])[2]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 2, v_qcode = v_qcode[2], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent)

ggsave(filename = paste0(out, "02", ".png"), width = 6, height = 6)



#---- Pregunta 3 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[3])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text[df_codebook$q_id == 11 & df_codebook$a_id == 2])[3]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 3, v_qcode = v_qcode[3], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent)

ggsave(filename = paste0(out, "03", ".png"), width = 6, height = 6)

#---- Pregunta 4
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[4])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text[df_codebook$q_id == 11 & df_codebook$a_id == 2])[4]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 4, v_qcode = v_qcode[4], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent)

ggsave(filename = paste0(out, "04", ".png"), width = 6, height = 6)


#---- Pregunta 4
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[5])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text[df_codebook$q_id == 11 & df_codebook$a_id == 2])[5]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 5, v_qcode = v_qcode[5], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent)

ggsave(filename = paste0(out, "05", ".png"), width = 6, height = 6)

#---- Pregunta 6
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[6])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))           %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[7]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 6, v_qcode = v_qcode[6], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    expand_limits(x = c(0, .8))

ggsave(filename = paste0(out, "06", ".png"), width = 6, height = 6)


#---- Pregunta 7
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[7])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))           %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 7, v_qcode = v_qcode[7], v_pregunta= v_pregunta))

# Gráfica
v_pregunta <- unique(df_codebook$t_text)[8]

ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "07", ".png"), width = 6, height = 6)



#---- Pregunta 8
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[8])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))           %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[9]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 8, v_qcode = v_qcode[8], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "08", ".png"), width = 6, height = 6)

#---- Pregunta 9
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[9])                                      %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))           %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[10]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 9, v_qcode = v_qcode[9], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    expand_limits(x = c(0, 0.7))

ggsave(filename = paste0(out, "09", ".png"), width = 6, height = 6)

#---- Pregunta 10: Abstinencia
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[10])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[11]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(data.frame(v_pnum = 10, v_qcode = v_qcode[10], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "10", ".png"), width = 6, height = 6)

#---- Pregunta 11: Tipo de consumo 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[11])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[12]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 11, v_qcode = v_qcode[11], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "11", ".png"), width = 6, height = 6)


#---- Pregunta 12: Efectos negativos
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[12])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[13]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 12, v_qcode = v_qcode[12], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "12", ".png"), width = 6, height = 6)



#---- Pregunta 13: Riesgos y daños
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[13])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[14]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 13, v_qcode = v_qcode[13], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "13", ".png"), width = 6, height = 6)




#---- Pregunta 14: Anexo
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[14])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[15]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 14, v_qcode = v_qcode[14], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "14", ".png"), width = 6, height = 6)


#---- Pregunta 15: Expulsión escuela
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[15])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[16]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 15, v_qcode = v_qcode[15], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "15", ".png"), width = 6, height = 6)


#---- Pregunta 16: Director
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[16])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[17]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 16, v_qcode = v_qcode[16], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "16", ".png"), width = 6, height = 6)


#---- Pregunta 17: Actuar igual 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[17])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[18]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 17, v_qcode = v_qcode[17], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "17", ".png"), width = 6, height = 6)


#---- Pregunta 18: Mala influencia
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[18])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[19]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 18, v_qcode = v_qcode[18], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "18", ".png"), width = 6, height = 6)



#---- Pregunta 19: Alcohol y tabaco
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[19])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_acuerdo))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[20]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 19, v_qcode = v_qcode[19], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "19", ".png"), width = 6, height = 6)


#---- Pregunta 20: Empatía
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[20])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    # mutate(respuesta = factor(respuesta, levels = v_vf))                %>% 
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- "¿Cuál de las siguientes frases consideras que definen a la empatía?"

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 20, v_qcode = v_qcode[20], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = percent, y = respuesta, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "20", ".png"), width = 6, height = 6)


#---- Pregunta 21: Reconocimiento 
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[21])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>%
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[21]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 21, v_qcode = v_qcode[21], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "21", ".png"), width = 6, height = 6)

#---- Pregunta 22: Historia personal
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[22])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>%
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[22]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 22, v_qcode = v_qcode[22], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "22", ".png"), width = 6, height = 6)


#---- Pregunta 23: Comprender sin sentimientos
df_data <- df_pivot                                                     %>% 
    filter(pregunta == v_qcode[23])                                     %>% 
    mutate(respuesta = ifelse(is.na(respuesta), "NS/NR", respuesta))    %>% 
    group_by(escuela, momento, respuesta)                               %>% 
    summarise(total = n())                                              %>% 
    mutate(respuesta = factor(respuesta, levels = v_vf))                %>%
    group_by(escuela, momento)                                          %>% 
    mutate(
        pob = sum(total), 
        percent = total/pob) %>% 
    ungroup()

v_pregunta <- unique(df_codebook$t_text)[23]

# Crear vector con los textos finales de las preguntas 
df_new_codes <- df_new_codes %>% 
    bind_rows(
        data.frame(v_pnum = 23, v_qcode = v_qcode[23], v_pregunta= v_pregunta))

# Gráfica
ggplot(df_data, 
    # Datos
    aes(x = respuesta, y = percent, fill = momento)) +
    facet_grid(escuela~momento) +
    # Geoms
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = scales::percent(percent)), 
        position = position_dodge(0.9)) +
    # Estilo
    labs(
        title = str_wrap(paste0(v_pregunta, "\n"), width = 60), 
        fill = "", 
        y = "", 
        x = "") +
    tema +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(20))

ggsave(filename = paste0(out, "23", ".png"), width = 6, height = 6)



# 3. Bucle con cambio agregado (sin comparación por escuela) -------------------

# 4. Guardar base 
df_resultados_escuela <- df_pivot %>% 
    filter(pregunta != "comentario") %>% 
    group_by(escuela, momento, pregunta, respuesta) %>% 
    summarise(total = n()) %>% 
    group_by(escuela, momento, pregunta) %>% 
    mutate(
        porcentaje = total/sum(total),
        momento = ifelse(momento == "Antes del curso", "exante", "expost")) %>% 
    rename(v_qcode = pregunta) %>% 
    ungroup() %>%
    # left_join(df_new_codes, by = "v_qcode") %>% 
    # select(escuela, momento, p_id = v_qcode, pregunta = v_pregunta, 
    #     respuesta, total, porcentaje) %>% 
    pivot_wider(names_from = momento, values_from = c(total, porcentaje)) %>% 
    arrange()


