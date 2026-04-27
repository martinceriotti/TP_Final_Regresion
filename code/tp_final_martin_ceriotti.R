library(ISLR)
library(glmnet)
library(caret)
library(dplyr)
library(MASS)
library(corrplot)
library(dplyr)  
library(purrr)
library(kableExtra) 

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

# Métricas de bondad de ajuste y selección de modelos.
# 3
# Comparar los 3 modelos a través de las siguientes métricas de performance: CME, PRESS, Cp, AIC
# y BIC. En base a los resultados observados, elegir un modelo “ganador”.

criterios <- function(m, maxi) {
  
  SCE <- deviance(m)
  n <- length(residuals(m))
  p <- length(coefficients(m)) #incluye intercepto
  
  #CME
  cme <- SCE/(n-p)
  
  #R2 ajustado
  r2aj <- summary(m)$adj.r.squared
  
  #AKAIKE
  akaike <- AIC(m)
  
  #BIC
  schwarz <- BIC(m)
  
  tibble(CME = cme, R2Aj = r2aj, AIC = akaike, BIC = schwarz)
}

map_dfr(list(mod_aic, mod_baic, mod_propio), criterios, maxi = mod_aic) %>% 
  mutate_all(round, 4) %>% 
  mutate(
    Modelo = c("Mod AIC", "Mod BAIC", "Modelo Propio"),
    Explicativas = c(
      "sexo + edad + est_civil +region+ horas_trabajo +educ_cat + formal + ant_laboral",
      "sexo + edad + est_civil +region+ horas_trabajo +educ_cat + formal + ant_laboral",
      "educ_cat + horas_trabajo + formal"
    )
  ) %>% 
  dplyr::select(Modelo, Explicativas, CME, "\\(R_{aj}^2\\)" = R2Aj, AIC, BIC) %>% 
  kable(escape = FALSE) %>% 
  kable_styling(full_width = F) %>% 
  row_spec(0, background = "black", color = "white", bold = T) %>% 
  row_spec(1, background = "#A9F5A9") %>% 
  row_spec(2, background = "#A9D0F5") %>% 
  row_spec(3, background = "#F5A9A9")

# El modelo mod_aic destaca en 3 de los 4 indicadores.
# 4

# --- A. Linealidad y Homocedasticidad ---
plot(mod_aic, 1) # Gráfico: Valores Ajustados vs Residuos
bptest(mod_aic)  # Test de Breusch-Pagan (H0: Homocedasticidad)

# Gráfico Residuals vs Fitted: La varianza de los residuos aumenta claramente con los valores ajustados (forma de abanico) → indica heterocedasticidad.
# Breusch-Pagan: BP = 146.22, p-value < 2.2e-16 → Se rechaza H0 → Se confirma heterocedasticidad. El supuesto no se cumple.

# --- B. Normalidad ---
plot(mod_aic, 2) # Gráfico: QQ-Plot
ad.test(residuals(mod_aic)) # Test de Anderson-Darling (H0: Normalidad)
#QQ-Plot: Los residuos se desvían fuertemente de la línea teórica, especialmente en la cola derecha → no hay normalidad.
#Anderson-Darling: A = 316.49, p-value < 2.2e-16 → Se rechaza H0 → Se confirma no normalidad. El supuesto no se cumple.

# --- C. Independencia de los Errores ---
dwtest(mod_aic) # Test de Durbin-Watson (H0: Autocorrelación nula)
#Durbin-Watson: DW = 1.8716, p-value = 1.127e-07 → Se rechaza H0 → Existe autocorrelación positiva leve. El supuesto no se cumple estrictamente, aunque el DW está cerca de 2.

# --- D. Multicolinealidad ---
vif(mod_aic) # Factor de Inflación de la Varianza (Idealmente < 5)
# En general, valores de VIF mayores a 5 se consideran problemáticos. En este caso no tenemos ninguno. 
# No hay multicolinealidad problemática. El supuesto se cumple.

# --- E. Observaciones Influyentes y Atípicas ---

sort(cooks.distance(mod_aic), decreasing = TRUE)[1:10]
#los 10 mas influyentes.

plot(mod_aic, 5)        # Leverage vs Residuos estandarizados (incluye Cook)
influencePlot(mod_aic)  # Tabla con casos influyentes (library(car))

# Se identifican como puntos influyentes a todos aquellos para los cuales Di > 1.

# 3170 Residuo studentizado = 15.9 → outlier severo. Cook's D = 0.05 → influyente
# 3838 Residuo studentizado = 14.1 → outlier severo. Cook's D = 0.033 → influyente
# 1028, 2938 Alto leverage pero residuos bajos → leverage points, poco influyentes

# Interpretación de efectos sobre el ingreso ocupacional
# Formalidad laboral
# Ser trabajador formal aumenta el ingreso en $301 respecto a un trabajador informal, efecto altamente significativo (p < 0.001).
# 
# Horas trabajadas
# Por cada hora adicional trabajada, el ingreso aumenta $10.69, efecto altamente significativo (p < 0.001).
# 
# Nivel educativo
# Categoría de referencia: Sin instrucción.
# NivelEfectoSignificanciaPrimario+$103No significativo (p = 0.074)Secundario+$238Significativo (p < 0.001)Superior+$607Altamente significativo (p < 0.001)
# A mayor educación, mayor ingreso. El nivel primario no es estadísticamente distinto del nivel sin instrucción.
# 
# Región
# Categoría de referencia: Cuyo.
# RegiónEfectoSignificanciaGran Buenos Aires+$139SignificativoNoreste-$138SignificativoNoroeste-$135SignificativoPampeana+$92SignificativoPatagonia+$372Altamente significativo
# Patagonia tiene el mayor ingreso relativo; Noreste y Noroeste los menores.
# 
# Edad
# Por cada año adicional de edad, el ingreso aumenta $4.41, efecto significativo (p < 0.001).
# 
# Sexo
# Categoría de referencia: Varón. Las mujeres tienen en promedio $152 menos de ingreso, efecto altamente significativo (p < 0.001).
# 
# Estado civil
# Categoría de referencia: Casado.
# EstadoEfectoSignificanciaSeparado/divorciado-$120SignificativoSoltero-$138Altamente significativoUnido-$63SignificativoViudo-$184Significativo
# Todos los estados civiles distintos de casado presentan menor ingreso.
# 
# Antigüedad laboral
# Categoría de referencia: Menos de 3 meses.
# Ninguna categoría resulta estadísticamente significativa (todos los p-values > 0.05, salvo "6 meses a 1 año" con p = 0.098 marginal). La antigüedad laboral no tiene efecto significativo sobre el ingreso en este modelo.
# 
# Ajuste global
# El modelo explica el 32.7% de la variabilidad del ingreso (R²aj = 0.327), siendo globalmente significativo (F-statistic p < 0.001).

unique(eph$region)
