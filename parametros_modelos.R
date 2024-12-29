################################################################ SVR #################################################################

# Búsqueda de hiperparámetros (Grid Search)
tune_grid <- expand.grid(
  cost = c(0.1, 1, 10),  # Valores posibles para el parámetro de penalización
  gamma = c(0.01, 0.1, 1)  # Valores posibles para el parámetro del kernel radial
)

best_model <- NULL
best_accuracy <- 0

for (i in 1:nrow(tune_grid)) {
  set.seed(123)
  
  #Ajuste del modelo
  modelo_svr <- svm(
    X_train, y = y_train,
    type = "C-classification",
    kernel = "radial",
    cost = tune_grid$cost[i],
    gamma = tune_grid$gamma[i]
  )
  
  # Predicciones
  predicciones <- predict(modelo_svr, X_test)
  
  # Evaluar precisión
  accuracy <- mean(predicciones == y_test)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- modelo_svr
  }
}

# Mostrar los mejores hiperparámetros
cat("Mejor modelo encontrado con: \n")
cat("Cost:", best_model$cost, "\nGamma:", best_model$gamma, "\n")



############################################################# Arboles ################################################################

tune_grid <- expand.grid(
  mtry = c(2, sqrt(ncol(X_train)), ncol(X_train) / 2),  # Variables por split
  ntree = c(100, 200, 500)  # Número de árboles
)

best_model <- NULL
best_accuracy <- 0

for (i in 1:nrow(tune_grid)) {
  set.seed(123)
  
  #Ajuste del modelo
  modelo_rf_tuned <- randomForest(
    x = X_train,
    y = y_train,
    ntree = tune_grid$ntree[i],
    mtry = tune_grid$mtry[i],
    importance = TRUE
  )
  
  # Predicciones
  predicciones <- predict(modelo_rf, X_test)
  
  # Evaluar precisión
  accuracy <- mean(predicciones == y_test)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- modelo_rf
  }
}

# Mostrar los mejores hiperparámetros
cat("\nMejor modelo encontrado con:\n")
cat("Número de árboles (ntree):", best_model$ntree, "\n")
cat("Número de variables por split (mtry):", best_model$mtry, "\n")



############################################################### XGBoost ############################################################

# Conversión de datos a matriz
X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)
dtrain <- xgb.DMatrix(data = X_train_matrix, label = as.numeric(y_train) - 1)
dtest <- xgb.DMatrix(data = X_test_matrix, label = as.numeric(y_test) - 1)

# Búsqueda de hiperparámetros
tune_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),       # Tasa de aprendizaje
  max_depth = c(3, 6, 9),        # Profundidad máxima de los árboles
  nrounds = c(50, 100, 200)      # Número de rondas
)

best_model <- NULL
best_accuracy <- 0
best_params <- NULL

for (i in 1:nrow(tune_grid)) {
  set.seed(123)
  
  #Ajuste del modelo
  modelo_xgb <- xgboost(
    params = list(
      objective = "multi:softmax", 
      num_class = length(levels(y_train)),
      eta = tune_grid$eta[i],
      max_depth = tune_grid$max_depth[i],
      eval_metric = "merror"
    ),
    data = dtrain,
    nrounds = tune_grid$nrounds[i],
    verbose = 0
  )
  
  # Predicciones
  predicciones <- predict(modelo_xgb, dtest)
  predicciones <- factor(predicciones, levels = 0:(length(levels(y_train)) - 1), labels = levels(y_train))
  
  # Evaluar precisión
  accuracy <- mean(predicciones == y_test)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- modelo_xgb
    best_params <- tune_grid[i, ]
  }
}

cat("\nMejor modelo encontrado con:\n")
print(best_params)



######################################################### KNN ########################################################################

valoresk <- 1:20

best_model <- NULL
best_accuracy <- 0

for (k in valoresk) {
  set.seed(123)
  
  # Predicciones
  predicciones_knn <- knn(train = X_train, test = X_test, cl = y_train, k = k)
  
  # Evaluar precisión
  accuracy <- mean(predicciones_knn == y_test)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
    best_model <- predicciones_knn
  }
}

cat("Mejor modelo encontrado con k =", best_k, "\n")
cat("Precisión obtenida con k =", best_k, ":", best_accuracy, "\n")



############################################################ Naive Bayes #############################################################


library(e1071)

valores_laplace <- c(0, 0.5, 1, 2)

best_model <- NULL
best_accuracy <- 0
best_laplace <- NULL

for (laplace in valores_laplace) {
  
  # Ajustar del modelo
  modelo_nb <- naiveBayes(x = X_train, y = y_train, laplace = laplace)
  
  # Hacer las predicciones
  predicciones <- predict(modelo_nb, X_test)
  
  # Calcular la precisión
  accuracy <- mean(predicciones == y_test)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- modelo_nb
    best_laplace <- laplace
  }
}

# Mostrar el mejor modelo
cat("Mejor modelo con Laplace =", best_laplace, "con precisión de", best_accuracy, "\n")


############################################################ Logistico ###############################################################

library(nnet)

# Definir el espacio de búsqueda de los hiperparámetros
tune_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),  # Valores de alpha entre 0 (Ridge) y 1 (Lasso)
  lambda = 10^seq(-4, 1, by = 0.5)  # Valores de lambda en una escala logarítmica
)

best_model <- NULL
best_accuracy <- 0

for (i in 1:nrow(tune_grid)) {
  set.seed(123)
  
  # Ajuste del modelo
  modelo_logistico <- multinom(X_train, y_train, family = "binomial", alpha = tune_grid$alpha[i], lambda = tune_grid$lambda[i])
  
  # Realizar predicciones usando el modelo entrenado
  predicciones <- predict(modelo_logistico, X_test, s = tune_grid$lambda[i], type = "response")
  
  # Calcular la precisión del modelo
  accuracy <- mean((predicciones > 0.5) == y_test)  # Convertir las probabilidades en clases (0 o 1)
  
  # Guardar el mejor modelo
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- modelo_logistico
  }
}
