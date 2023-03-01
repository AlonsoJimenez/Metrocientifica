#Iniciamos generando los vectores de las mediciones de masa y diametro para la esfera de plastico
masas <- c(543.12, 543.10, 543.16, 543.13, 543.12)
diametros <- c(120.02, 120.09, 120.12, 119.94, 120.01)

#Ahora generamos las incertidumbres

error = 0.03

iteraciones = 1000000

masaMedida <- rnorm(n = iteraciones, mean = mean(masas)-error, sd=(sd(masas)/sqrt(length(masas)))) #Tomamos en cuenta el error sistematico que presenta el equipo
diametroMedido <- rnorm(n = iteraciones, mean = mean(diametros), sd=(sd(diametros)/sqrt(length(diametros))))


#Ahora generamos incertidumbres por resolucion

resMasa <- runif(iteraciones, -0.005, 0.005)
resDiametro <- runif(iteraciones, -0.005, 0.005)

#Generamos incertisumbre por certificado de la balanza

certMasa <- rnorm(iteraciones, 0, 0.01/2)

#Generamos incertidumbre por EMP del vernier

empDiametro <-  runif(iteraciones, -0.04, 0.04)


#Ahora sumamos estas contribuciones para encontrar valores y poder comparar con el metodo GUM

diametro = diametroMedido + resDiametro + empDiametro
masa = masaMedida + resMasa + certMasa

#Aplicamos formula para encontrar la densidad

densidad = (6*masa)/(diametro^3*pi)

#Ahora reportamos el valor de la densidad y su incertidumbre estandar
mean(densidad)
sd(densidad)

#Generamos el histograma para analizar el comportamiento de la incertidumbre

hist(densidad, breaks = 150)

#Ahora comparamos los resultados con los dados por el metodo GUM

print("Valor de masa: ")
mean(masa)
print("Valor de diametro: ")
mean(diametro)
print("Incertiumbre combinada de masa: ")
sd(masa)
print("Incertiumbre combinada de diametro: ")
sd(diametro)
print("Incertiumbre estandar de resolucion del diametro: ")
sd(resDiametro)
print("Incertiumbre estandar de resolucion de la masa: ")
sd(resMasa)
print("Incertiumbre estandar de repetibilidad de la masa: ")
sd(masaMedida)
print("Incertiumbre estandar de repetibilidad del diametro: ")
sd(diametroMedido)
print("Incertiumbre estandar de certificado de la balanza: ")
sd(certMasa)
print("Incertiumbre estandar por EMP del vernier: ")
sd(empDiametro)

#Ahora debemos generar un modelo que modifique esta densidad segun la temperatura para esto sobre estimaremos el aporte de un cambio de temperatura a una distribucion rectangular de +- 3 grados celcius

temperatura <- runif(iteraciones, -3, 3)

#Definimos el coeficiente de expansion termica

alpha = 110*10^-6 # unidades 1/grados celcius, coeficiente de polietileno cuenta con densidad similar a la encontrada

#Ahora analizamos el cambio de volumen de la esfera como deltaV = 3*alpha*temperatura*volumenInicial

volumenInicial <- 4*pi*(diametro/2)^3/3

deltaV <- 3*alpha*temperatura*volumenInicial

nuevoVolumen = volumenInicial + deltaV

#Recalculamos densidad y su incertidumbre

nuevaDensidad = masa/nuevoVolumen

#Mostramos resultados

print("Valor de nueva densidad")
mean(nuevaDensidad)
print("Nueva incertidumbre estandar de la densidad")
sd(nuevaDensidad)
hist(nuevaDensidad, breaks = 150)

