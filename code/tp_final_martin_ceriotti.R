library(ISLR)
library(glmnet)
library(caret)
library(dplyr)
library(MASS)
library(corrplot)

# Cargar librerías para tests
library(lmtest)
library(nortest)
library(car)

# Importación de datos.

library(readxl)
eph <- read_excel("data/eph.xlsx")
eph <- na.omit(eph)
set.seed(989269) 

# Consigna I: Regresión Lineal
# 1:

filas_train <- createDataPartition(eph$ingreso_hogar, p = 0.7, list = FALSE)
entrenar <- eph[filas_train, ]
probar <- eph[-filas_train, ]

corrplot(cor(entrenar))

# 2: Ajustar tres modelos diferentes de Regresión Lineal Múltiple con el método de los Mínimos Cuadrados
# Y ) ingreso_ocup

# Modelo 1
# Con Akaike (AIC) busco equilibrio entre bondad de ajuste y complejidad.
# Modelo de Selección Automática por Criterio de Información de Akaike (AIC)

mod_aic <- stepAIC(
  object = lm(ingreso_ocup ~ 1, data = entrenar),
  scope = list(upper = lm(ingreso_ocup ~ sexo + edad + est_civil +region+ horas_trabajo +educ_cat + formal + ant_laboral, data = entrenar)),
  direction = "forward", 
  trace = FALSE, 
  k = 2, # (2 = AIC, log(n) = BIC)
  steps = 1000 #máximo nro de pasos (1000 es el default)
)
# Vemos variables altamente significativas: formalSi, las horas trabajadas, el nivel educativo (Secundario y Superior), casi todas las regiones, la edad y el sexo.
# Las no significativas: 
 
# Género: las mujeres ganan, en promedio, 146.19 (miles de pesos) menos que los hombres, a igualdad del resto de variables.
# Educación: Comparado con no tener educación, tener nivel Superior incrementa el ingreso en 574.90 miles de pesos 
# promedio, a igualdad del resto de variables..
# Formalidad: El trabajador formal gana 306.22 miles de pesos más que el informal, manteniendo todo lo demás constante.
# Horas de Trabajo: Por cada hora adicional trabajada a la semana, el ingreso mensual aumenta en 10.25 miles de pesos.
# Edad: Por cada año adicional de vida, el ingreso sube 3.93 miles de pesos 
# Patagonia es la región con mayor impacto positivo (+351.50).
# Noroeste (NOA) y Noreste (NEA) muestran coeficientes negativos (aprox. -141 y -118 respectivamente), lo que refleja salarios promedio nominales más bajos en comparación con el resto del país

# El R ajustado es 0.3239. En temas socioeconomicos no es un % despreciable.
# El mejor modelo es:

summary(mod_aic)

# Modelo 2
# Penalización mayor por cada variable incluida, modelo más parsimonioso.
# Modelo de Selección Automática por Criterio de Información Bayesiano (BIC)
# Este modelo se seleccionó bajo el principio de parsimonia.
# BAIC tiene un poco menos de R2 ajustado y un poco más de error.
# Lo bueno es que no incluye antiguedad laboral (parsimonia).

mod_baic <- stepAIC(
  object = lm(ingreso_ocup ~ 1, data = entrenar),
  scope = list(upper = lm(ingreso_ocup ~ sexo + edad + est_civil +region+ horas_trabajo +educ_cat + formal + ant_laboral, data = entrenar)),
  direction = "forward", 
  trace = FALSE, 
  k = log(nrow(entrenar)),
  steps = 1000 #máximo nro de pasos (1000 es el default)
)

# El mejor modelo es:
summary(mod_baic)

# Modelo 3
# En teoría la instrucción y la experiencia son los motores principales de la productividad 
# y, por ende, del salario. Es por esto que elegí las variables.

mod_propio <- lm(ingreso_ocup ~ educ_cat + horas_trabajo + formal, data = entrenar)
summary(mod_propio)







# --- A. Linealidad y Homocedasticidad ---
plot(mod_aic, 1) # Gráfico: Valores Ajustados vs Residuos
bptest(mod_aic)  # Test de Breusch-Pagan (H0: Homocedasticidad)

# --- B. Normalidad ---
plot(mod_aic, 2) # Gráfico: QQ-Plot
ad.test(residuals(mod_aic)) # Test de Anderson-Darling (H0: Normalidad)

# --- C. Independencia de los Errores ---
dwtest(mod_aic) # Test de Durbin-Watson (H0: Autocorrelación nula)

# --- D. Multicolinealidad ---
vif(mod_aic) # Factor de Inflación de la Varianza (Idealmente < 5)

# --- E. Observaciones Influyentes y Atípicas ---
plot(mod_aic, 4) # Gráfico: Distancia de Cook


#Comparar los 3 modelos a través de las siguientes métricas de performance: CME, PRESS, Cp, AIC
#y BIC. En base a los resultados observados, elegir un modelo “ganador”.

