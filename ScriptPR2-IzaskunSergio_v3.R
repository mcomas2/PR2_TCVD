#Leer el csv que en ;
data2read <- read.csv2(file.choose(), fileEncoding = "UTF-8")

#Tratar los valores duplicados:
data2clean <- data2read[!duplicated(data2read$Orden.mas.vendidos), ]

#Tratar los valores vacios (1a parte):
data2clean$Genero[data2clean$Genero == ""] <- NA

#Tratar la columna precios: 
data2clean$Precio <- substr(data2clean$Precio, 1, nchar(data2clean$Precio) - 1)
data2clean$Precio <- gsub(",", ".", data2clean$Precio) 
data2clean$Precio <- as.numeric(data2clean$Precio)
is_numeric2 <- is.numeric(data2clean$Precio)
print(is_numeric2) #Si es TRUE lo hemos convertido a var. numerica correctamente

#Imputar valores faltantes en la columna Genero con la moda (2a parte):
moda_genero <- names(sort(table(data2clean$Genero), decreasing = TRUE))[1]  #Obtener la moda
data2clean$Genero[is.na(data2clean$Genero)] <- moda_genero  #Reemplazar valores NA con la moda

#Las columnas con valores categoricas en factores
data2clean$Título <- as.factor(data2clean$Título)
data2clean$Autor <- as.factor(data2clean$Autor)
data2clean$Genero <- as.factor(data2clean$Genero)

#Tratar con outliners en el Precio
bp <- boxplot(data2clean$Precio, main = "Precios Outliners", col = "green", border = "red")
outliers <- bp$out
print(outliers)

#Dividir los valores atípicos entre 10
data2clean$Precio[data2clean$Precio %in% outliers] <- data2clean$Precio[data2clean$Precio %in% outliers] / 10

#Resultado final
summary(data2clean)
boxplot(data2clean$Precio, main = "Precios Adjusted Outliners", col = "green", border = "blue")

write.csv2(data2clean, "data2clean_adjusted.csv", row.names = FALSE, fileEncoding = "UTF-8")

if (!require("caTools")) install.packages("caTools", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("randomForest")) install.packages("randomForest", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("carData")) install.packages("carData", dependencies = TRUE)
if (!require('factoextra')) install.packages('factoextra', dependencies=TRUE)

# Cargar el paquete 'factoextra'
library(factoextra)

library(caTools)
library(dplyr)
library(randomForest)
library(ggplot2)

data2clean_adjusted <- read.csv2(file.choose(), fileEncoding = "UTF-8")

#4.1

# Fijar una semilla para reproducibilidad
set.seed(123)

# Dividir el dataset en entrenamiento (70%) y prueba (30%)
division <- sample.split(data2clean_adjusted$Orden.mas.vendidos, SplitRatio = 0.7)
entrenamiento <- subset(data2clean_adjusted, division == TRUE)
test <- subset(data2clean_adjusted, division == FALSE)

# Reasignar las categorías de Genero

entrenamiento$Genero <- as.character(entrenamiento$Genero)

entrenamiento$Genero[entrenamiento$Genero %in% c("Fantasía", "Aventuras", "Ciencia ficción")] <- "Fantasía"
entrenamiento$Genero[entrenamiento$Genero %in% c("Novela negra, thriller o suspense.", "Distopía","Romántica", "Novela histórica", "Contemporáneo")] <- "Realista"


entrenamiento$Genero <- factor(entrenamiento$Genero)

test$Genero <- as.character(test$Genero)
test$Genero[test$Genero %in% c("Fantasía", "Aventuras", "Ciencia ficción")] <- "Fantasía"
test$Genero[test$Genero %in% c("Novela negra, thriller o suspense.", "Distopía","Romántica", "Novela histórica", "Contemporáneo")] <- "Realista"


test$Genero <- factor(test$Genero)

# Ajustar los hiperparámetros del modelo Random Forest con las nuevas categorías
modeloRF <- randomForest(Orden.mas.vendidos ~ Precio + Genero, data = entrenamiento, ntree = 300, mtry = 2)

# Ver el resumen del modelo ajustado
print(modeloRF)

# Hacer predicciones en el conjunto de prueba
predicciones <- predict(modeloRF, test)

# Comparar las predicciones con los valores reales
comparacion <- data.frame(Real = test$Orden.mas.vendidos, Predicted = predicciones)

# Calcular el error medio absoluto
EMA <- mean(abs(comparacion$Real - comparacion$Predicted))
print(paste("Error Medio Absoluto del modelo:", EMA))

# Gráfico de importancia de variables
importance(modeloRF)
varImpPlot(modeloRF, main="Importancia de Variables - Modelo Random Forest")

# Gráfico de predicciones vs valores reales
ggplot(comparacion, aes(x=Real, y=Predicted)) +
  geom_point(color="blue") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  ggtitle("Predicciones vs. Valores Reales") +
  xlab("Valores Reales") +
  ylab("Predicciones") +
  theme_minimal()


autores_extranjeros <- c("Michael McDowell", "Frank Herbert", "Haruki Murakami", "Jeanette Winterson", 
                         "William Faulkner", "Henry James", "Paul Auster", "Rebecca Makkai", 
                         "Jon Fosse", "Jo Nesbo", "John Kennedy Toole", "Stefan Zweig", 
                         "Irene Némirovsky", "Bonnie Garmus", "T. S. Eliot")

# Paso 3: Añadir la columna Autor_extranjero
data2clean_adjusted$Autor_extranjero <- ifelse(data2clean_adjusted$Autor %in% autores_extranjeros, "Extranjero", "No Extranjero")

# Convertir la nueva columna a factor
data2clean_adjusted$Autor_extranjero <- as.factor(data2clean_adjusted$Autor_extranjero)

division2 <- sample.split(data2clean_adjusted$Orden.mas.vendidos, SplitRatio = 0.7)
entrenamiento2 <- subset(data2clean_adjusted, division2 == TRUE)
test2 <- subset(data2clean_adjusted, division2 == FALSE)

# Reasignar las categorías de Genero

entrenamiento2$Genero <- as.character(entrenamiento2$Genero)
entrenamiento2$Genero[entrenamiento2$Genero %in% c("Fantasía", "Aventuras", "Ciencia ficción")] <- "Fantasía"
entrenamiento2$Genero[entrenamiento2$Genero %in% c("Novela negra, thriller o suspense.", "Distopía", "Romántica", "Novela histórica", "Contemporáneo")] <- "Realista"
entrenamiento2$Genero <- factor(entrenamiento2$Genero)

test2$Genero <- as.character(test2$Genero)
test2$Genero[test2$Genero %in% c("Fantasía", "Aventuras", "Ciencia ficción")] <- "Fantasía"
test2$Genero[test2$Genero %in% c("Novela negra, thriller o suspense.", "Distopía", "Romántica", "Novela histórica", "Contemporáneo")] <- "Realista"
test2$Genero <- factor(test2$Genero)

modeloRF2 <- randomForest(Orden.mas.vendidos ~ Precio + Genero + Autor_extranjero, data = entrenamiento2, ntree = 500, mtry = 2)

# Ver el resumen del modelo ajustado
print(modeloRF2)

# Hacer predicciones en el conjunto de prueba
predicciones2 <- predict(modeloRF2, test2)

# Comparar las predicciones con los valores reales
comparacion2 <- data.frame(Real2 = test2$Orden.mas.vendidos, Predicted2 = predicciones2)

# Calcular el error medio absoluto
EMA2 <- mean(abs(comparacion2$Real2 - comparacion2$Predicted2))
print(paste("Error Medio Absoluto del modelo ajustado:", EMA2))



# Gráfico de importancia de variables
importance(modeloRF2)
varImpPlot(modeloRF2, main="Importancia de Variables - Modelo Ajustado")

# Gráfico de predicciones vs valores reales
ggplot(comparacion2, aes(x=Real2, y=Predicted2)) +
  geom_point(color="blue") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  ggtitle("Predicciones vs. Valores Reales") +
  xlab("Valores Reales") +
  ylab("Predicciones") +
  theme_minimal()



#MODELO NO SUPERVISADO


library(cluster)
library(ggplot2)
library(dplyr)


# Convertir la variable 'Genero' en variables dummy
dummy <- model.matrix(~Genero - 1, data=data2clean_adjusted)
clustering <- cbind(data2clean_adjusted$Precio, dummy)

# Estandarizar las variables
clustering <- scale(clustering)

# Determinar el número óptimo de clusters usando el método del codo
set.seed(123)
fviz_nbclust(clustering, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Método del codo")

# Ajustar el modelo K-means con el número óptimo de clusters (k=3 en este caso)
set.seed(123)
resultado <- kmeans(clustering, centers = 3, nstart = 25)

# Añadir los clusters al dataframe original
data2clean_adjusted$clusters <- as.factor(resultado$cluster)

# Visualizar los resultados del clustering
ggplot(data2clean_adjusted, aes(x=Precio, y=Orden.mas.vendidos, color=clusters)) +
  geom_point(size=2) +
  labs(title="Clustering de Libros por Precio y Género",
       x="Precio",
       y="Orden de Más Vendidos") +
  theme_minimal()

# Opcional: Visualización con PCA para entender la separación de clusters
pca_resultado <- prcomp(clustering, scale = TRUE)
fviz_pca_ind(pca_resultado,
             geom.ind = "point", 
             col.ind = data2clean_adjusted$clusters,
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Cluster") +
  labs(title="Clustering de Libros con PCA",
       x="Componente Principal 1",
       y="Componente Principal 2") +
  theme_minimal()


#4.2

data$Genero <- as.factor(data$Genero)

# Prueba de Shapiro-Wilk para normalidad
shapiro.test(data2clean_adjusted$Precio)

epsilon <- 0.01  # Pequeño valor constante
data2clean_adjusted$log_Precio <- log(data2clean_adjusted$Precio + epsilon)

shapiro_test_log <- shapiro.test(data2clean_adjusted$log_Precio)
shapiro_test_log

library(car)
# Prueba de Levene para homocedasticidad
leveneTest(Precio ~ Genero, data = data2clean_adjusted)

# Prueba de Kruskal-Wallis para comparar medianas entre grupos
kruskal.test(Precio ~ Genero, data = data2clean_adjusted)

# ANOVA para comparar medias entre grupos
anova <- aov(Precio ~ Genero, data = data2clean_adjusted)
summary(anova)


