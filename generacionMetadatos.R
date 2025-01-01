# Directorio donde están las carpetas de imágenes
base_dir <- "./data/"

# Etiquetas (nombres de las carpetas)
etiquetas <- c("noche","nublado","soleado")

# Crear un data.frame vacío para almacenar los resultados
dataset <- data.frame(nombre_imagen = character(),
                      url = character(),
                      etiqueta = character(),
                      stringsAsFactors = FALSE)

# Recorrer cada carpeta y agregar las imágenes al dataset
for (etiqueta in etiquetas) {
  # Obtener la lista de archivos en la carpeta
  ruta_carpeta <- file.path(base_dir, etiqueta)
  archivos <- list.files(ruta_carpeta, full.names = TRUE)
  
  # Crear un data.frame temporal con la información
  datos_temporales <- data.frame(
    url = archivos,
    etiqueta = etiqueta,
    stringsAsFactors = FALSE
  )
  
  # Agregar al dataset principal
  dataset <- rbind(dataset, datos_temporales)
}

# Exportar a un archivo CSV
write.csv(dataset, file = "dataset_metadata.csv", row.names = FALSE)

cat("¡El archivo CSV fue generado correctamente!\n")
