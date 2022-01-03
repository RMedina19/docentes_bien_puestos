#------------------------------------------------------------------------------#
# Proyecto:                   Docentes bien puestos
# Objetivo:                   Procesar resultados de la primera vuelta (por escuela)
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
