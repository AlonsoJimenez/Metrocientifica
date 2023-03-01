#Se definen los valores de las dimensiones para calcular el area por formula de heron
# Cada una de estas variables o dimensiones tienen una incertidumbre asociada y se desea conocer la incertidumbre final del area

a <- 100
b <- 90
c <- 80

#Cada valor tiene una incertidumbre por resolucion, rectangular, de 0.5mm
incer <- 0.5

#Ahora definimos la cantidad de iteraciones para montecarlo

iteraciones <- 1000000

#Ahora creamos vectores con los valores y sus incertidumbres
A <- runif(iteraciones, a-incer, a+incer)
B <- runif(iteraciones, b-incer, b+incer)
C <- runif(iteraciones, c-incer, c+incer)

#Calculamos el area

S = (A + B + C)*0.5
Area <- sqrt(S*(S-A)*(S-B)*(S-C))

#Mostramos resultados


print(mean(Area))
print(sd(Area))


hist(C, nclass = 100)
hist(Area, nclass = 100)
