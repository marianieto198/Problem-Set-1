## Taller 1 Big Data

rm(list = ls())

require(pacman)

p_load(tidyverse, rvest)
p_load(corrplot) #Biblioteca de visualización de correlaciones
p_load(GGally) #Visualización
p_load(lattice) #Paquete de ML
p_load(caret) # Paquete de ML
p_load(ROSE) #Paquete para balancear los datos
p_load(recipes)
p_load(e1071)
p_load(AlphaPart)
p_load(skimr)
p_load(broom)
p_load(stargazer)
#----------Web Scrapping-------------------

#Probar el Scrapping con el primer chunk
Pagina1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>% #Leo el archivo html
            html_table() #Lo pongo en modo tabla

class(Pagina1) #La clase de los datos es una List, tiene que volverse un Dataframe

Pagina1 <- as.data.frame(Pagina1) # Ya queda como Data Frame

df <- data.frame() #Este sera el nombre del DF que se utilizara para nuestro analisis

for (i in 1:10) {
  
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html") #Cada archivo html se guarda en esta variable
  chunk_i <- read_html(url) %>%
            html_table() # Se guarda cada chunk como tabla de clase list
            
  chunk_i <- as.data.frame(chunk_i)  #Se vuelve un data frame
  df <- rbind(df, chunk_i) #Se incluyen las nuevas filas a nuestro DF master
}

remove(chunk_i, i, url, Pagina1)

#--------Data Cleaning-------------------

dim(df) # 32177 observaciones 178 variables
str(df)
head(df)
summary (df) # se puede ver que hay un gran número de observaciones vacias

#La primera variable se puede eliminar
df <- df %>%
      select(-Var.1)

# Utilizando el diccionario, identificamos variables categóricas
# para volverlas a tipo factor
df <- mutate_at(df, .vars = c(
    "cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
    "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
    "cotPension", "cuentaPropia", "depto", "directorio", "dominio",
    "dsi", "estrato1", "formal", "ina", "inac", "informal",
    "maxEducLevel", "p6050", "microEmpresa", "ocu", "oficio", 
    "orden", "p6090", "p6100", "p6210", "p6210s1", "p6240", "p6510",
    "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", "p6585s1",
    "p6585s1a2", "p6585s2", "p6585s2a2", "p6585s4", "p6585s4a2",
    "p6590", "p6610", "p6620", "p6630s1", "p6630s2", "p6630s3",
    "p6630s4", "p6630s6", "p6920", "p7040", "p7050", "p7090",
    "p7110", "p7120", "p7140s1", "p7140s2", "p7150", "p7160",
    "p7310", "p7350", "p7422", "p7472", "p7495", "p7500s1",
    "p7500s2", "p7500s3", "p7505", "p7510s1", "p7510s2",
    "p7510s3", "p7510s5", "p7510s6", "p7510s7", "pea", "pet", 
    "regSalud", "relab", "secuencia_p", "sex", "sizeFirm", "wap"),
    .funs = factor)

summary(df)

#Filtro de edad, Bogotá y que sean ocupadas

df <- df %>%
      filter(ocu == 1,
             dominio == "BOGOTA",
             age > 18)

dim(df)
summary(df) #se puede observar que aunque bajaron los NAs sigue habiendo una presencia significativa en muchas variables


#Se pueden observar un gran número de observaciones vacías en cada una de las variables
cantidad_na <- sapply(df, function(x) sum(is.na(x))) #Una función que me suma el número de NAs por variable
cantidad_na <- data.frame(cantidad_na) #Lo convierto en Data Frame
porcentaje_na <- cantidad_na/nrow(df) #Le saco el porcentaje de Missing values a cada variable

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
#El 45% de las variables tienen NAs


# Ordenamos de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))
# Convertimos el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# Quitamos las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
str_count(variables_sin_na) #Hay 58 variables sin NA
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))


porcentaje_na <- porcentaje_na[!filtro,] #Quedan solo 119 variables con NAs

orden <- porcentaje_na$variable[length(porcentaje_na$variable):1] #Se vuelven caracteres
porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden) #Se utilizan como factores para poder graficar

str(porcentaje_na) # Se revisa el tipo de variables

# Como son tantas variables vamos a hacer 3 gráficas
ggplot(porcentaje_na[1:50,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

ggplot(porcentaje_na[51:100,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

ggplot(porcentaje_na[101:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

filtro_eliminar <- porcentaje_na$cantidad_na > 0.05
variables_eliminar <- porcentaje_na$variable[filtro_eliminar]
k0 <- ncol(df)
df_sinna <- df %>%
  select(-variables_eliminar)
k1 <- ncol(df_sinna)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

porcentaje_na %>%
              filter(cantidad_na <= 0.05)

#Como tres variables son continuas se reemplaza por la mediana, para que queden en el percentil 50%
df_sinna <- df_sinna %>%
            mutate(impa = ifelse(is.na(impa), median(impa, na.rm = T), impa),
                   p7070 = ifelse(is.na(p7070), median(p7070, na.rm = T), p7070),
                   isa = ifelse(is.na(isa), median(isa, na.rm = T), isa))


sum(is.na(df_sinna$impa))
sum(is.na(df_sinna$p7070))
sum(is.na(df_sinna$isa))
            
#Para MaxLevelEduc toca sacar la moda y eso lo hacemos mirando su plot y después reemplazando
ggplot(df_sinna, aes(x = maxEducLevel)) + geom_bar()

#La moda es la categoría 7
moda_MaxEduc <- which(table(df_sinna$maxEducLevel) == max(table(df_sinna$maxEducLevel))) #Guardo el mayor valor
filtro2 <- is.na(df_sinna$maxEducLevel) #filtro solo los NAs
df_sinna$maxEducLevel[filtro2] <- moda_MaxEduc #Reemplazo los NAs por la moda
table(df_sinna$maxEducLevel) #Reviso

sum(is.na(df_sinna$maxEducLevel)) #Reviso

df_sinna %>% #Se revisa que todo el DF esté sin NAs
          is.na() %>%
          sum()

summary(df_sinna)




#Mirar las correlaciones de las variables numericas
rename(df, geih)

geih <- df
geih_sinna <- df_sinna
skim(geih_sinna)

remove(df, df_sinna)

numericgeih <- geih_sinna %>% select(is.numeric)

#Miramos las correlaciones
M = cor(numericgeih)
corrplot(M, method = "number")
#Outliers

ncol(numericgeih)

for (col in 1:28) {
  print(ggplot(numericgeih, aes(x = numericgeih[, col])) + geom_boxplot() + labs(x = colnames(numericgeih)[col]))
  #Sys.sleep(2)
}


outlieringreso <- boxplot.stats(numericgeih$ingtot)$out #Muchos outliers 1834
outlieredad <- boxplot.stats(numericgeih$age)$out #Pocos outliers, mayores de 84
outlierhoras <- boxplot.stats(numericgeih$totalHoursWorked)$out #3195 outliers

factorgeih <- geih_sinna %>% select(is.factor)

#Ver las modas de las categoricas
#No incluir los códigos de hogares
ncol(factorgeih)

for (col in 5:34) {
  print(ggplot(factorgeih, aes(x = factorgeih[, col])) + geom_bar() + labs(x = colnames(factorgeih)[col]))
  #Sys.sleep(2)
}


#-----------Punto 4-------------------
#Base a utilizar en este punto
geih_punto4 <- geih_sinna %>%
                select(ingtot, sex, age)

#Transformar el ingreso en logaritmo
geih_punto4 <- geih_punto4 %>%
                mutate(logIng = log(ingtot))

geih_punto4 <- geih_punto4 %>%
  filter(logIng > 0)

#Mirar las proporciones
table(geih_punto4$sex)


#Los 1 son hombres, hay que cambiar esto para hacer nuestro modelo
geih_punto4 <- geih_punto4 %>%
  mutate(female = ifelse(sex == "0", 1, 0))

geih_punto4 <- geih_punto4 %>%
  mutate(female = as.factor(female))

table(geih_punto4$female)

str(geih_punto4)
summary(geih_punto4)

#Correlación
M_p4 <- geih_punto4 %>%
          select(-sex) %>%
          cor()
M_p4 #No está tan clara la correlación

#Modelo lineal
#Estimar el modelo de gap
ModeloGap <- lm(logIng ~ female, data = geih_punto4)

geih_punto4 %>% ggplot(aes(x = logIng)) + geom_histogram()

summary(ModeloGap)

#El modelo muestra que la variable independiente es significativa y muestra que al ser mujer se reduce el ingreso en en un 19.6% Sin embargo,
#el R2 es solamente del 1.2%, lo que significa que la variable de género solo explica un 1.2% la varianza del modelo.
stargazer(ModeloGap, type = "text")


GapPredict <- predict(ModeloGap)

#Se incluyen los y estimados para comprarar el modelo
geih_punto4 <- geih_punto4 %>%
                mutate(fitValues = ModeloGap$fitted.values)

#Solo hay dos valores para los y estimados: uno  para mujer (13.97) y otro para hombre (14.07)
summary(geih_punto4)
exp(max(geih_punto4$fitValues))
exp(min(geih_punto4$fitValues))

#Analizamos algunas gráficas
ggplot(geih_punto4, aes(x = logIng, y = fitValues, colour = female)) + geom_point()          
ggplot(geih_punto4, aes(x = fitValues, y = logIng, colour = female)) + geom_point()
ggplot(geih_punto4, aes(x= logIng, fill = female)) + geom_histogram(position = "identity", alpha = 0.5)
ggplot(geih_punto4, aes(x= fitValues, fill = female)) + geom_histogram(position = "identity", alpha = 0.5)


#Modelo con edad y genero
geih_punto4 <- geih_punto4 %>%
                mutate(age2 = age^2)

ModeloGapAge <- lm(logIng ~ female + age + age2, data = geih_punto4)
summary(ModeloGapAge)
stargazer(ModeloGapAge, type = "text")

#Plot con los datos reales. Muestra que si tienen un mismo intercepto pero los ingresos divergen a lo largo del tiempo.
#Los hombres crecen y las mujeres decrecen
ggplot(geih_punto4, aes(x = age, y = logIng, colour = female)) + geom_point(alpha = 0.15) + geom_smooth(method = 'lm')

#Plot con los Y estimados. No hay intercepción, aunque muestran la misma pendiente a lo largo del gráfico. Llegan a un punto máximo
#Y empieza a decrecer
ggplot(geih_punto4, aes(x = age, y = ModeloGapAge$fitted.values, colour = female)) + geom_point(alpha = 0.15) + geom_smooth()


#Peak Age con Bootstrap

set.seed(1111)
ControlEntrenamiento <- trainControl(method = "boot", number = 1000) #Control de train para hacer el Bootstrapping
ModelosBoot_AgeCAP <- train(logIng ~ female + age + age2,
                            data = geih_punto4,
                            method = "lm",
                            trControl = ControlEntrenamiento) #Correr el modelo con 1000 interacciones para reducir los errores estandar

summary(ModelosBoot_AgeCAP$finalModel$coefficients) #Los errores estándar no se redujeron, es necesario intentar de otra forma

p_load(boot)

model_coef <- function(data, index){
  coef(lm(logIng ~ female + age + age2, data = data, subset = index)) #Crear la función para calcular los errores estándar
}
model_coef(geih_punto4, 1:16138) #Probar la función

ModeloBoot_AgeCAP2 <- boot(geih_punto4, model_coef, R=1000) #Correr el boot. Se puede ver que los errores estándar mejoraran con el boot


#Calculamos los intervalos de confianza para cada variable
lowerlimitFem <- ModelosBoot_AgeCAP$finalModel$coefficients[2] - (1.96*0.01356242)
upperlimitFem <- ModelosBoot_AgeCAP$finalModel$coefficients[2] + (1.96*0.01356242) 
ConfidenceIntervalFemale <- c(lowerlimitFem, upperlimitFem)

lowerlimitAge <- ModelosBoot_AgeCAP$finalModel$coefficients[3] - (1.96*0.003509626)
upperlimitAge <- ModelosBoot_AgeCAP$finalModel$coefficients[3] + (1.96*0.003509626)
ConfidenceIntervalAge <- c(lowerlimitAge, upperlimitAge)

lowerlimitAge2 <- ModelosBoot_AgeCAP$finalModel$coefficients[4] - (1.96*0.00004273566)
upperlimitAge2 <- ModelosBoot_AgeCAP$finalModel$coefficients[4] + (1.96*0.00004273566)
ConfidenceIntervalAge2 <- c(lowerlimitAge2, upperlimitAge2)

#Ninguna de las variables tiene overlap en los intervalos

max(ModelosBoot_AgeCAP$finalModel$fitted.values)
max(ModeloGapAge$fitted.values)

geih_punto4$fitValuesGAPAGE <- ModelosBoot_AgeCAP$finalModel$fitted.values

#Peakage en la gráfica
peaked_age <- geih_punto4 %>% 
  group_by(age, female) %>%
  summarise(max(fitValuesGAPAGE)) %>%
  arrange(female)         
#Para los dos géneros la edad de pico son los 44 años

peaked_age <- ModelosBoot_AgeCAP$finalModel$coefficients[3]/(-ModelosBoot_AgeCAP$finalModel$coefficients[4]*2)

#Ahora se realiza la regresion con la interacción
geih_punto4$numfem <- as.numeric(geih_punto4$female)
geih_punto4$numfem <- ifelse(geih_punto4$numfem == 2, 1, 0)

geih_punto4$agefem <- geih_punto4$numfem*geih_punto4$age

#Los coeficientes cambian, hay un mayor impacto de género. EL R2 aumenta a 4,24%
ModeloGapAgeInt <- lm(logIng ~ female + age + age2 + agefem, data = geih_punto4)
coef(ModeloGapAgeInt)
summary(ModeloGapAgeInt)

#Plot con los Y estimados. No hay intercepción, aunque muestran la misma pendiente a lo largo del gráfico. Llegan a un punto máximo
#Y empieza a decrecer
ggplot(geih_punto4, aes(x = age, y = ModeloGapAgeInt$fitted.values, colour = female)) + geom_point(alpha = 0.15) + geom_smooth()


model_coef <- function(data, index){
  coef(lm(logIng ~ female + age + age2 + agefem, data = data, subset = index)) #Crear la función para calcular los errores estándar
}
model_coef(geih_punto4, 1:16138) #Probar la función

ModeloBoot_AgeCAP2 <- boot(geih_punto4, model_coef, R=1000) #Correr el boot. Se puede ver que los errores estándar mejoraran con el boot
summary(ModeloBoot_AgeCAP2)


#Calculamos los intervalos de confianza para cada variable
lowerlimitFemI <- ModeloBoot_AgeCAP2$t0[2] - (1.96*0.0439099056)
upperlimitFemI <- ModeloBoot_AgeCAP2$t0[2] + (1.96*0.0439099056) 
ConfidenceIntervalFemaleI <- c(lowerlimitFemI, upperlimitFemI)

lowerlimitAgeI <- ModeloBoot_AgeCAP2$t0[3] - (1.96*0.0036893856)
upperlimitAgeI <- ModeloBoot_AgeCAP2$t0[3] + (1.96*0.0036893856)
ConfidenceIntervalAgeI <- c(lowerlimitAgeI, upperlimitAgeI)

lowerlimitAge2I <- ModeloBoot_AgeCAP2$t0[4] - (1.96*0.0000446136)
upperlimitAge2I <- ModeloBoot_AgeCAP2$t0[4] + (1.96*0.0000446136)
ConfidenceIntervalAge2I <- c(lowerlimitAge2I, upperlimitAge2I)

lowerlimitAgeFem <- ModeloBoot_AgeCAP2$t0[5] - (1.96*0.0011546835)
upperlimitAgeFem <- ModeloBoot_AgeCAP2$t0[5] + (1.96*0.0011546835)
ConfidenceIntervalAgeFem <- c(lowerlimitAgeFem, upperlimitAgeFem)

#peak age
PeakAgeMujer <- (ModeloBoot_AgeCAP2$t0[3] + ModeloBoot_AgeCAP2$t0[5])/(-2*ModeloBoot_AgeCAP2$t0[4])
#Las mujeres tienen su cúspide salarial a los 39.31 años

PeakAgeHombre <- ModeloBoot_AgeCAP2$t0[3]/(-2*ModeloBoot_AgeCAP2$t0[4])
#Los hombres, por su parte tienen su cúspide salarial a los 48.25 años