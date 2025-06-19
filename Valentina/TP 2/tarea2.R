# Cargar datos
filename <- "https://raw.githubusercontent.com/dietrichson/Ciencia_de_datos_2025/refs/heads/main/data/usu_individual_T324.txt"
datos <- readr::read_delim(filename, delim = ";")

# Filtrar sexo
datos2 <- subset(datos, P47T > 0)
sexo <- factor(datos2$CH04, levels = c(1, 2), labels = c("Varón", "Mujer"))

# Cálculo de la media de ingreso por sexo
media_ingreso <- tapply(datos2$P47T, sexo, mean, na.rm = TRUE)

tabla_media <- as.table(media_ingreso)

# Gráfico de barras del ingreso promedio por sexo
barplot(
  tabla_media,
  main = "Ingreso promedio por sexo",
  xlab = "Sexo",
  ylab = "Media de ingreso (P47T)",
  ylim = c(0, max(tabla_media) * 1.1)
)

# Tabla del ingreso promedio por sexo
tabla_media