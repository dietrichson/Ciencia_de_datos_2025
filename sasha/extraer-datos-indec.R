# Script para extraer datos
filename <- "./data/usu_individual_T324.txt"
file.exists(filename)
my_data <- readr::read_delim(filename, delim = ";")
