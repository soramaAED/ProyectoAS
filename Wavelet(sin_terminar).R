## Transformada Wavelet 

library(WaveletComp)

# Función para calcular características basadas en la descomposición wavelet
extraer_caracteristicas_wavelet <- function(imagenes_train) {
  # Crear una matriz para almacenar las características
  caracteristicas_wavelet <- matrix(0, nrow = length(imagenes_train), ncol = 4)
  colnames(caracteristicas_wavelet) <- c("LL", "LH", "HL", "HH")
  
  # Iterar sobre cada imagen
  for (j in 1:length(imagenes_train)) {
    # Cargar la imagen
    img <- imagenes_train[[j]]
    
    # Convertir a escala de grises si tiene 3 canales (RGB)
    if (dim(img)[3] == 3) {
      img <- grayscale(as.cimg(img))
    }
    
    # Realizar la transformada wavelet (usando la DWT 2D)
    wavelet <- dwt(img, filter = "la8", n.levels = 3)
    
    # Obtener las subbandas: LL, LH, HL, HH
    aprox1 <- wavelet@V[[1]]
    aprox2 <- wavelet@V[[2]]
    aprox3 <- wavelet@V[[3]]
    detalle1 <- wavelet@W[[1]]
    detalle2 <- wavelet@W[[2]]
    detalle3 <- wavelet@W[[3]]
  }
  
  return(caracteristicas_wavelet)
}

# Ejemplo de uso
car_wavelet_train <- extraer_caracteristicas_wavelet(imagenes_train)
car_wavelet_test <- extraer_caracteristicas_wavelet(imagenes_test)

# Mostrar las características extraídas
print(car_wavelet_train)
print(car_wavelet_test)
