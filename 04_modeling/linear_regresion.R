# ***************************************************************
# Erick Gordón
# erick.gordon@rowsums.com
# https://www.linkedin.com/in/erickgordonb/
# ***************************************************************
# Date: 14-sep-2019
# ***************************************************************
library(tidyverse) 
library(DataExplorer)
library(janitor)
library(ggResidpanel)
source("./00_scripts/functions.R") 
# ***************************************************************

# Linear regression example
climate_tbl <- readr::read_csv("./00_data/in/climate_change.csv")
climate_tbl <- climate_tbl %>%
  clean_names()

climate_tbl %>%
  glimpse()

table(climate_tbl$year)
summary(climate_tbl)

summary_tbl <- climate_tbl %>% 
	gather(key, value,2:11) %>% 
	summary_col_by_group(key, col = value)

# datos de entrenamiento
training_data <- climate_tbl %>% 
	filter(year <= 2006)

# genera modelo con todsas las variables
model1 <- lm(temp ~ mei + co2 + ch4 + n2o + cfc_11 +  cfc_12 + tsi + aerosols, data = training_data)
summary(model1)

# establece correlaciones 
cor(training_data)

training_data %>%
  DataExplorer::plot_correlation()


# genera segundo modelo corregido, sale CF11 dado que posee una correlación fuerte con N20 y N20 posee una correlación más fuerte con la temp.
model2 <- lm(temp ~ mei + tsi +  aerosols + n2o, data = training_data)
summary(model2)

# genera un nuevo modelo, el cual es optimazado eliminando variables no necesarias con la función "step"
# the stepwise-selected model. The mode of stepwise search, can be one of "both", 
model3 <- step(model1)
summary(model3)

#Nota: Este modelo deja dentro del grupo las variables con colinealidad. Lo que genera que la funcion no necesariamente
#sea interpretable, es un modelo que ha equilibrado la calidad y simplicidad para una ponderacion particular de calidad 

# genera una prediccion con los datos de test. 
#test_data <- subset(climate_tbl, year > 2006)
test_data <- climate_tbl %>%
  filter(year > 2006)
predict_test <-  predict(model3, newdata=test_data)

# establecer el R^2 para comparararlo con el modelo original y establecer que tan cercano estuvo la prediccion
SSE <- sum((test_data$temp - predict_test)^2)
SST <- sum((test_data$temp - mean(training_data$temp))^2)
1 - SSE / SST

# Utilizando el modelo sin ajuste 
test_data <- climate_tbl %>%
  filter(year > 2006)
predict_test <-  predict(model2, newdata=test_data)

SSE <- sum((test_data$temp - predict_test)^2)
SST <- sum((test_data$temp - mean(training_data$temp))^2)
1 - SSE / SST

# *************************************************************
# validate the linear model 
# *************************************************************
# There are four key assumptions to check for this model.
#1. Linearity between the response variable and the predictor variables. - linear relationship
#2. Constant variance of the residuals - Homoscedasticity (1 y 2 residual vs predicted)
#3. Normality of the residuals (qqplot + shapiro.test)
#4. It must have no or little multicollinearity - this means the independent variables must not be too highly correlated with each other. This can be tested with a Correlation matrix and other tests
#5. Independence of observations. - No auto-correlation


resid_panel(model3)

lm_residual <- test_data$temp - predict_test
mean(test_data$temp)  
mean(predict_test)

# Assessing Outliers
car::outlierTest(model3) # Bonferonni p-value for most extreme obs
car::qqPlot(model3, main="QQ Plot") #qq plot for studentized resid 
car::leveragePlots(model3) # leverage plots


resid_interact(model3, plots = c("resid", "qq", "index", "hist"))

# 2. normallity test with few records, 
# Ho = la población está distribuida normalmente
# si el p-valor < alfa (nivel de significancia) entonces la hipótesis nula es rechazada 
# (se concluye que los datos no vienen de una distribución normal). 
# Si el p-valor es mayor a alfa, no se rechaza la hipótesis y se concluye que los datos siguen una distribución normal.
shapiro.test(lm_residual) #p-value = 0.263 < 0.05 ... no. No se rechaza Ho.
hist(lm_residual)


# heteroscedasticity - different variabilities (variance) from others **
# homoscedastic - all random variables in the sequence or vector have the same finite variance
# Ho: variance of the residuals is constant (homocedastic)
# rule: if p-value < 0.05 then reject Ho
car::ncvTest(model3)  # 0.88591 < 0.05  .. No. No se rechaza Ho
car::spreadLevelPlot(model3)

# Test for Autocorrelated Errors
# Ho: The Durbin-Watson test has the null hypothesis that the = "autocorrelation of the disturbances is 0"
# p-value < 0.05 reject Ho
car::durbinWatsonTest(model3) #  0 < 0.05 Sí. Se rechaza Ho. 


# comparar modelos
resid_compare(models = list(model3, 
                            model2, 
                            model1),
              plots = c("resid", "qq"),
              smoother = TRUE,
              qqbands = TRUE,
              title.opt = FALSE)


# plots
#Gráfica residual (arriba a la izquierda): Esta es una gráfica de los residuales versus los valores predictivos del modelo para evaluar los supuestos de linealidad y varianza constante. La tendencia de curvatura observada en el gráfico penguin_model sugiere una violación del supuesto de linealidad, y parece haber una violación del supuesto de varianza constante, ya que la varianza de los residuos aumenta a medida que aumentan los valores pronosticados.

#Gráfica Cuantil Normal (arriba a la derecha): también conocida como gráfica qq, esta gráfica nos permite evaluar el supuesto de normalidad. Parece haber una desviación de la normalidad en el extremo superior de los residuos del modelo pingüino, pero esto no es tan preocupante como los problemas de linealidad y varianza constante.

#Histograma (abajo a la derecha): este es un histograma de los residuos con una curva de densidad normal superpuesta con desviación estándar y media calculada a partir de los residuos. Proporciona una forma adicional de verificar el supuesto de normalidad. Este gráfico deja en claro que hay un ligero sesgo a la derecha en los residuos del modelo pingüino.

#Gráfica de índice (abajo a la izquierda): Esta es una gráfica de los residuos versus los números de observación. Puede ayudar a encontrar patrones relacionados con la forma en que se han ordenado los datos, lo que puede proporcionar información sobre tendencias adicionales en los datos que no se han tenido en cuenta en el modelo. No hay una tendencia obvia en el gráfico de índice penguin_model.

#Residuals vs Leverage: An influential case is one that, if removed, will affect the model so its inclusion or exclusion should be considered. (outliers)
