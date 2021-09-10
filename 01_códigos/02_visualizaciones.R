#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados de la primera vuelta
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     regina.medina@alumnos.cide.edu
# Fecha de creación:          10 de septiembre de 2021
# Última actualización:       10 de septiembre de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Limpiar área de trabajo 
rm(list=ls())

# Cargar librerías 
require(pacman)
p_load(readxl, tidyverse, tidyr, stringr, scales, beepr)

# Dirección 
inp <- "03_datos_limpios/"
out <- "04_figuras/"

# 1. Cargar datos --------------------------------------------------------------

# Base unificada de todas las respuestas
load(paste0(inp, "df_resultados_frecuencias.RData"))

# Cambiar nombre de la base para facilitar manipulación
df_data <- df_resultados_frecuencias %>% rename(freq = total)

# 2. Bucle para genearción de figuras ------------------------------------------

# Guardar nombre de las variables 
v_vars  <- unique(df_data$q_code)

# Generar vectores para gráfica 

# Vectores generales 
v_ylab  <- "Número de respuestas"
v_capt  <- "Fuente: Elaboración propia con datos de la encuesta \"Docentes bien puestos\" del CIDE-PPD."

# 2.1. Ensayo ------------------------------------------------------------------

# Datos de la variables 
n_var   <- v_vars[1]
df_var  <- df_data %>% filter(q_code == n_var) %>% mutate(as.character(a_text))
n_title <- ifelse(is.na(unique(df_var$t_text)), df_var$q_text, paste0(df_var$q_text, " \n", df_var$t_text))
n_limit <- max(df_var$freq)+0.1*max(df_var$freq)
v_label <- str_wrap((unique(df_var$a_text)), width = 80)

ggplot(df_var, 
    aes(x = a_text, y = freq, fill = "#457b9d")) +
    geom_col() +
    geom_label(aes(label = paste0(freq, "\n (", p_text, ")" )), 
        size = 2, fill = "white") +
    labs(
        title =  str_wrap(paste0(n_title, "\n"), width = 10),
        x = "", 
        y = v_ylab, 
        caption = v_capt
    ) +
    theme_bw() +
    ylim(0, n_limit) +
    scale_fill_manual(values = "#457b9d") +
    guides(fill = "none")+
    theme(axis.text.x = element_text(angle = 0), 
        # plot.title = element_text(size=800/nchar(n_title), hjust=0.5)
    ) +
    # scale_x_discrete(labels = str_wrap(unique(df_var$a_text), width = 10)) +
    # scale_x_discrete(labels = function(a_text) str_wrap(a_text, width = 10)) +
    scale_x_discrete(labels = wrap_format(20)) 

# 2.2. Ejecución del bucle -----------------------------------------------------

for(i in 1:length(v_vars)){
    # Guardar datos de la variable
    n_var   <- v_vars[i]
    df_var  <- df_data %>% filter(q_code == n_var)
    n_title <- ifelse(is.na(unique(df_var$t_text)), df_var$q_text, paste0(df_var$q_text, ": \n", df_var$t_text))
    n_qcode <- unique(df_var$q_code)
    n_limit <- max(df_var$freq)+0.1*max(df_var$freq)
    
    print(paste(i, n_var))
    
    # Generar gráficas 
    p <- ggplot(df_var, 
        aes(x = a_text, y = freq, fill = "#457b9d")) +
        geom_col() +
        geom_label(aes(label = paste0(freq, "\n (", p_text, ")" )), 
            size = 2, fill = "white") +
        labs(
            title =  str_wrap(paste0(n_title, "\n"), width = 80),
            x = "", 
            y = v_ylab, 
            caption = v_capt
        ) +
        theme_bw() +
        ylim(0, n_limit) +
        scale_fill_manual(values = "#457b9d") +
        guides(fill = "none")+
        theme(
            axis.text.x = element_text(angle = 0)) +
        scale_x_discrete(labels = wrap_format(20))
    
    ggsave(p, file = paste0(out, i, "-", n_qcode, ".png"), width = 8, height = 8)
}

print(p)
beep(5)

# FIN. -------------------------------------------------------------------------
