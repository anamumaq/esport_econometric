#importar librerias
library(dplyr)
# library(Ecdat) #
# library(plm)
library(corrplot)

# Lectura de datos

df <- read.csv('df_17_18_19.csv')

head(df,3)


# orden para panel
df <- df %>% 
  select(year, country, total_earnings, # y
         -total_players,-iso, #no aplica el modelo 
         pbicap, gdp_gr, GDP_growth, GNI_cap, # variables macroeconomicas
         gastoedu, -edu_pbi_per, # edu
         CPI, # corrupcion
         poder_adq, desempleo,# work 
         internet, elect_acc, movil, # tech access
         age_work, age_game, # edades
         poblacion, pop_growth, rural_per, urb # people
        ) %>%
  arrange(country, decreasing = FALSE)


# Valores Na encontrados

sapply(df, function(x) sum(is.na(x)))

## Venezuela no tiene valores pbicap

df[is.na(df$internet),] # 2018 cambodia y trinidad y tobago

### Cambodia, hueco en 2018, 2019=52, 2017=33

df[df$country=='Cambodia', 'internet'][2] <- 
          (df[df$country=='Cambodia', 'internet'][1] + 
             df[df$country=='Cambodia', 'internet'][3])/2

### trinidad y tobago, hueco reemplazdo por el promedio
df[df$country=='Trinidad and Tobago', 'internet'][2] <- 
          (df[df$country=='Trinidad and Tobago', 'internet'][1]+
              df[df$country=='Trinidad and Tobago', 'internet'][3])/2
#######
# Filtro paises por conflicto o restricciones gubernaentales
df[is.na(df$poder_adq),'country'] # nas
df[is.na(df$GNI_cap),'country'] # nas

df <- df[!df$country %in% c('Cuba','Syrian Arab Republic'),]
###

df[is.na(df$gastoedu),c('country', 'year')] # nas

# 
df <- df%>%select(-age_work, # personas relacionado a pobacion
                  -urb,  # igual a rural
                  )

##################################
# por mientras

df_standar <- df[!df$country %in% c('Bosnia and Herzegovina',
                                    'Egypt', 'India', 'Iraq',
                                    'Ireland', 'Kuwait', 'Mexico',
                                    'North Macedonia', 'Saudi Arabia',
                                    'Sri Lanka', 'Tunisia',
                                    'United Arab Emirates'),]

sapply(df_standar, function(x) sum(is.na(x)))
###############################
# Normalizar datos


df_standar <- df_standar %>% 
  mutate(across(c("total_earnings", "pbicap", "poblacion", "GNI_cap"), ~log(.) %>% as.vector))

head(df_standar,3)



summary(df_standar)


df_corr <- df_standar%>%select(-GNI_cap, # mide lo imso de pbi calidad de vida
                               -GDP_growth, 
                               -elect_acc)


matriz_corr <- cor(df_corr[3:15])
corrplot(matriz_corr, method = 'number', number.cex = 0.7) # encuentro correlaciones

####
# save data clean

# write.csv(df_corr, 'df_tfm_clean_test.csv', row.names = FALSE)
