#importar librerias

library('dplyr')

# Lectura de datos

df <- read.csv('df_17_18_19.csv')

head(df,10)


df %>% 
  select(year, country, total_earnings, total_players,
         pbicap, gastoedu, internet, exp_tech,
         imp_tech, desempleo, elect_acc, movil, urb,
         poblacion, innovacion,  age_work, 
         new_business, CPI, age_game,  -iso, -code ) %>%
  
str(df)
