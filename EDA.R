library(dplyr)
library(Ecdat)
library(plm)
library(corrplot)
library(psych)
library(ggplot2)
library(purrr)
library(car)

# read csv
# df_panel <- read.csv('df_tfm_clean_test.csv')
df_panel <- df_corr

##############
# EDA

#Segun la literatura el GPC y la poblacion 
#son variables que impactan directamente al desempeno del deporte
# library(ggpubr)

ggplot(df_panel, aes(y=total_earnings, x=pbicap)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red")

cor(df_corr$total_earnings, df_corr$pbicap, method = "pearson")

# Conociendo lols paises

df_panel%>% count(country)


ggplot(df_panel, aes(x = country, y = total_earnings, 
                    fill = factor(year),group = year)) + 
  geom_col() +
  labs(x = "country", y = "total_earnings [.Log]",
       fill = "year")


ggplot(df_panel, aes(x = factor(country), y = total_earnings)) +
  geom_boxplot() +
  # scale_x_discrete(breaks = seq(1970, 2007, by = 5)) +
  labs(x = "country", y = "total_earnings [.Log]")+
  theme(axis.text.x = element_text(angle = 90))


# win.graph()
# scatterplot(total_earnings~year, boxplots=FALSE, 
#             smooth=TRUE, reg.line=FALSE, data=df_corr)

plotmeans(total_earnings ~ year, main="Heterogeineity across years", data=df_corr)


ggplot(df_corr, aes(x = factor(year), y = total_earnings)) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun.data = function(y) mean_se(y, mult = 1), geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Heterogeineity across years", x = "Year", y = "Y")

### Analizando mas correlaciones



pairs(df_standar%>%select(pbicap, 
                          GNI_cap, #paridad poder adq
                          CPI, # corrupcion
                          poder_adq, #poder adq
                          gdp_gr,# crecimiento pbi percapita
                          GDP_growth, # crecimiento pbi 
))

# win.graph()
# pairs(df_corr[3:16])




# Supongamos que tienes un DataFrame llamado df


# Función para crear un gráfico para una columna
plot_hist_density <- function(column) {
  ggplot(df_corr, aes_string(column)) +
    geom_histogram(aes(y = ..density..), bins = 30, colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal()
}

# Aplicar la función a cada columna del DataFrame
# df_corr %>% 
#   purrr::keep(is.numeric) %>% 
#   names() %>% 
#   purrr::map(plot_hist_density) %>% 
#   gridExtra::grid.arrange(grobs = ., ncol = 2)
#######################

# MAS DETALLE
pairs.panels(df_corr%>%select(total_earnings,pbicap, internet,CPI, poder_adq,age_game),
             smooth = FALSE,     # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = FALSE,   # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             # pch = 21,         # Símbolo pch
             lm = TRUE,          # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             # factor = 2,       # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             # ci = TRUE         # Si TRUE, añade intervalos de confianza a los ajustes
) 


################################################################################
# MODELOS


# modelo con panel

dim(table(df_panel$country,df_panel$year))

colnames(df_panel)

## Regresion lineal multiple
# primer modelo de observaciones como independiente 
reg_mco = lm(total_earnings~
               pbicap+
               poblacion,
             # gdp_gr+
             # gastoedu+
             # CPI+
             # poder_adq+
             # desempleo+internet+
             # movil+
             # age_game+
             # pop_growth+
             # rural_per,
             data=df_panel)
summary(reg_mco)

# Segundo modelo (intercepto a cada pais)
reg_mco_country = lm(total_earnings~
               pbicap+
               poblacion +
               factor(country),
             # gdp_gr+
             # gastoedu+
             # CPI+
             # poder_adq+
             # desempleo+internet+
             # movil+
             # age_game+
             # pop_growth+
             # rural_per,
             data=df_panel)
summary(reg_mco_country)

df_panel_m2 <- df_panel %>%
  mutate(te_predict = fitted(reg_mco_country))


ggplot(data = df_panel_m2, aes(x = total_earnings, y = te_predict, 
                              label = country, group = country)) +
  geom_point() +
  # add pais-specific lines
  geom_smooth(method = "lm", se = F, color = "black") +
  # add pooled line
  geom_smooth(mapping = aes(x = total_earnings, y = te_predict), inherit.aes = F,
              method = "lm", se = T, color = "black", linetype = "dashed") +
  # label lines
  geom_text(
    data = df_panel_m2 %>% 
      group_by(country) %>% 
      top_n(1, total_earnings) %>% 
      slice(1),
    mapping = aes(label = country), vjust = 1
  )


# con el df sin correlaciones
reg_mco = lm(total_earnings~pbicap+internet+desempleo+elect_acc+movil+
               poblacion+CPI+age_game,
             data=df_corr)
summary(reg_mco)
vif(reg_mco)

yhat <- reg_mco$fitted
# normalidad residuos
res_mco <- resid(reg_mco)
shapiro.test(res_mco)
# hetero
plot(df_corr$pbicap, y=df_corr$total_earnings,
     pch=19, xlab="pbicap", ylab="total_earnings") 
abline(lm(df_corr$total_earnings~df_corr$pbicap),lwd=3, col="red")



############################################
reg.mco.ind = lm(total_earnings~country+
                   # pbicap+
                   gdp_gr+gastoedu+CPI+
                   # poder_adq+
                   desempleo+internet+
                   movil+
                   age_game+poblacion+
                   pop_growth+
                   rural_per,
                 data=df_corr)
summary(reg.mco.ind)
res_mco_ind <- resid(reg.mco.ind)
shapiro.test(res_mco_ind)


reg.mco.per = lm(total_earnings~year+
                   # pbicap+
                   gdp_gr+gastoedu+CPI+
                   # poder_adq+
                   desempleo+internet+
                   movil+
                   age_game+poblacion+
                   pop_growth+
                   rural_per,
                 data=df_corr)
summary(reg.mco.per)
res_mco_per <- resid(reg.mco.per)
shapiro.test(res_mco_per)


## fijos

reg.fijos.within = plm(total_earnings~
                         pbicap+
                         poblacion,
                         # gdp_gr+gastoedu+CPI+
                         # poder_adq+
                         # desempleo+
                         # internet+
                         # movil+
                         # age_game+
                         # pop_growth,
                       # rural_per,
                       index=c("country", "year"), 
                       model="within", data=df_panel)
summary(reg.fijos.within)
res_fijos.within <- resid(reg.fijos.within)
shapiro.test(res_fijos.within)



qqnorm(df_corr$pbicap, main = "QQ-Plot")
qqline(df_corr$pbicap)
