rm(list=ls())
library(nortest) # para normalidad (test de Lilliefors)
library(lmtest) # para la heterocedasticidad
library(multiColl) # para la multicolinealidad  


  
##############################
# en el archivo que me manda la alumna, en la línea 147, justo debajo de cuando define T y X en la página4 del PDF
# escribo lo siguiente:
#     datos = data.frame(Y, X, country)
#     write.table(datos, "02_datos.txt", sep=";", row.names=F)	
#     library(xlsx)
#     write.xlsx(datos, "02_datos.xlsx", sheetName="datos", append=FALSE, row.names=F)



# de esta forma salvo los datos que voy a utilizar a continuación

datos = read.table("02_datos.txt", header=T, sep=";")
head(datos, 9)
names(datos)

attach(datos)
  
  ef = lm(TG ~ PIBc + INT + DEmp + INF + TJ_POB + PEA_POB + MIG_POB, factor(pais))
  summary(ef)
  
  # residuos 
  
    e = ef$residuals  
    
    # observo normalidad
    
      minimo = min(e)
      maximo = max(e)
      rango = maximo - minimo
      intervalos = 10 # número de intervalos de igual amplitud que quiero crear
      hist(e, breaks=seq(minimo, maximo, rango/intervalos), col="lightblue", freq=F, xlab="Residuos", ylab="Porcentaje", ylim = c(0, 0.8)) 
      lines(density(e), col = "red", lwd = 2) # superponemos curva de densidad de los residuos
      discretizacion = seq(min(e), max(e), length = length(e))
      lines(discretizacion, dnorm(discretizacion, mean = mean(e), sd = sd(e)), col = "blue", lwd = 2) # superponemos una normal calculada a pertir de la media y varianza de los residuos
      # parece que hay una leve asimetría, pero nada más
      
    # observo heterocedasticidad
      
      # buscamos grupos de observaciones con distinta varianza: parece no observarse nada
      plot(e, ylab="Residuos", xlab="Observación", col="blue", lwd=3, main="Gráfico de dispersión de los residuos")
      plot(e^2, ylab="Residuos", xlab="Observación", col="blue", lwd=3, main="Gráfico de dispersión de los residuos al cuadrado")
      
      # se busca si la variabilidad de los residuos aumenta o disminuye conforme aumenta la variable independiente
      plot(PIBc, e^2, ylab="Residuos", xlab="pbicap", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a pbicap") # este es decreciente?
      plot(INT, e^2, ylab="Residuos", xlab="internet", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a internet")
      plot(DEmp, e^2, ylab="Residuos", xlab="desempleo", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a desempleo") # este es decreciente?
      plot(PEA_POB, e^2, ylab="Residuos", xlab="poblacion", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a pea_pob")
      plot(INF, e^2, ylab="Residuos", xlab="inflacion", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a inflacion") # este es decreciente?
      plot(MIG_POB, e^2, ylab="Residuos", xlab="esperanza de vida", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a inflacion") 
      plot(TJ_POB, e^2, ylab="Residuos", xlab="players_ppl", col="blue", lwd=3, main="Gráfico de dispersión de los residuos frente a tj_pob") # este es decreciente?
      
  # H0: normalidad
  
    ks.test(e, pnorm, 0, sd(e)) # no se rechaza H0
    shapiro.test(e) # se rechaza H0 
    lillie.test(e) # se rechaza H0
    
  # H0: homocedasticidad
  
    bptest(ef)# se rechaza H0

  # H0: autocorrelación (como los efectos fijos son por individuos, debemos preocuparnos por la heterocedasticidad)
    
    dwtest(ef) # se rechaza H0
    
  # multicolinealidad
    
    X = datos[,-c(1,9)]
    cte = rep(1, length(TG))
    X = cbind(cte, X)
    RdetR(X) # coeficiente de determinación muy pequeño ---> posible problema de multicolinealidad
    CVs(X)
    CVs(X) < 0.1 # para TRUE posible problema de multicolinealidad no esencial
    VIF(X)
    VIF(X) > 10 # para TRUE posible problema de multicolinealidad esencial (tienen coeficientes no significativamente distintos de cero)
      
detach(datos)

    