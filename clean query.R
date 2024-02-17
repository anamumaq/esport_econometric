#importar librerias
library(dplyr)
library(Ecdat) #
library(plm)
library(corrplot)

# Lectura de datos

df <- read.csv('df_17_18_19.csv')

head(df,10)


# orden para panel
df <- df %>% 
  select(year, country, total_earnings, 
         pbicap, gastoedu, internet, 
         -total_players, #afecta el modelo
         -exp_tech, -imp_tech, #NO APORTA AL MODELO y tiene nas
         desempleo, elect_acc, movil, urb,
         poblacion, innovacion,  age_work, 
         new_business, CPI, age_game,  -iso, -code) %>%
  arrange(country, decreasing = FALSE)


# Valores Na encontrados

sapply(df, function(x) sum(is.na(x)))

## Venezuela no tiene valores pbicap

df <- df[!is.na(df$pbicap),] #na.omit()

df <- df[!is.na(df$CPI),] # MACAOes considerada como region autonoma no como pais

df[is.na(df$internet),] # 2018 cambodia y trinidad y tobago
### Cambodia, hueco en 2018, 2019=52, 2017=33

df[df$country=='Cambodia', 'internet'][2] <- 
          (df[df$country=='Cambodia', 'internet'][1] + 
             df[df$country=='Cambodia', 'internet'][3])/2

### trinidad y tobago, hueco reemplazdo por el promedio
df[df$country=='Trinidad and Tobago', 'internet'][2] <- 
          (df[df$country=='Trinidad and Tobago', 'internet'][1]+
              df[df$country=='Trinidad and Tobago', 'internet'][3])/2
###

df[is.na(df$gastoedu),'country'] # nas
df[is.na(df$innovacion),'year'] # nas
df[is.na(df$new_business),'new_business'] # nas

df <- df%>%select(-gastoedu, -innovacion, -new_business)


# Normalizar datos

df_standar <- df %>% 
  mutate(across(c("total_earnings","pbicap", "poblacion", "age_work"), ~log(.) %>% as.vector))

head(df_standar,10)

df_corr <- df_standar%>%select(-urb, -age_work) 

summary(df_corr)
matriz_corr <- cor(df_corr[3:11])
corrplot(matriz_corr, method = 'number') # encuentro correlaciones

### Analizando mas correlaciones

library(psych)

pairs(df_corr%>%select(pbicap, internet,CPI, elect_acc))

# MAS DETALLE
pairs.panels(df_corr%>%select(total_earnings,pbicap, internet,CPI, elect_acc),
             smooth = FALSE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = FALSE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             # pch = 21,           # Símbolo pch
             lm = TRUE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             # factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             # ci = TRUE         # Si TRUE, añade intervalos de confianza a los ajustes
) 


################################################################################
# modelo con panel

dim(table(df_corr$country,df_corr$year))

colnames(df_corr)
## Regresion lineal multiple
reg_mco = lm(total_earnings~pbicap+internet+desempleo+elect_acc+movil+
               urb+poblacion+age_work+CPI+age_game,
             data=df_standar)
summary(reg_mco)
res_mco <- resid(reg_mco)
shapiro.test(res_mco)
library(car)

# con el df sin correlaciones
reg_mco = lm(total_earnings~pbicap+internet+desempleo+elect_acc+movil+
               poblacion+CPI+age_game,
             data=df_corr)
summary(reg_mco)
vif(reg_mco)

############################################
reg.mco.ind = lm(total_earnings~country+pbicap+gastoedu+internet+desempleo+elect_acc+movil+urb+poblacion+innovacion+age_work+new_business+CPI+age_game, data=df)
summary(reg.mco.ind)

reg.mco.per = lm(total_earnings~year+pbicap+gastoedu+internet+desempleo+elect_acc+movil+urb+poblacion+innovacion+age_work+new_business+CPI+age_game, data=df)
summary(reg.mco.per)


## fijos
#plm(total_earnings~pbicap+gastoedu+internet+desempleo+elect_acc+movil+urb+poblacion+innovacion+age_work+new_business+CPI+age_game,
reg.fijos.within = plm(total_earnings~pbicap+gastoedu+internet+desempleo+urb+poblacion+age_work+CPI,
                       index=c("country", "year"), 
                       model="within", data=df_standar)
summary(reg.fijos.within)



qqnorm(df_standar$pbicap, main = "QQ-Plot")
qqline(df_standar$pbicap)
