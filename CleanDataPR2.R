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
