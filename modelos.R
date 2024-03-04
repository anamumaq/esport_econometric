#######################
# modelo pooling
# https://towardsdatascience.com/a-guide-to-panel-data-regression-theoretics-and-implementation-with-python-4c84c5055cf8
model_1 <- plm(total_earnings ~ gdp_gr+gastoedu+poder_adq+
                 desempleo+internet+movil+age_game+poblacion+pop_growth+rural_per,
               data = df_panel, model = "pooling")

fittedvals_pooled_OLS <- fitted.values(model_1)
residuals_pooled_OLS <- residuals(model_1)

# homocedasticidad

ggplot(data.frame(fittedvals = fittedvals_pooled_OLS, 
                  residuals = residuals_pooled_OLS),
       aes(x = fittedvals, y = residuals)) +
  geom_point(color = 'blue') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(x = 'Predicted Values', 
       y = 'Residuals', 
       title = 'Test de homosedasticidad')#parece q no hay homo

test_breusch_pagan <- bptest(model_1)
print(test_breusch_pagan) # menor a 0.05 existe heterosedasticidad

# no autoco
test_durbin_watson <- dwtest(model_1)
print(test_durbin_watson) # no se obtiene nada
# se concluyen que este modelo no es bueno porque inclumple varios supuestos

#######################
# modelo RE aleatorios 

re_model <- plm(total_earnings ~ gdp_gr+gastoedu,
                # +poder_adq+
                # desempleo+internet+movil+age_game+
                # poblacion+pop_growth+rural_per, #para no tener intercepto
                data = df_panel, model = "random", 
                # effect = "individual",
                index=c("country", "year"), 
                # random.method = 'nerlove'
)
#Swamy no sirve para este modelo porque tengo 11 var y 3 pauses
resultado_re <- summary(re_model)

# modelo FE fijos

fe_model <- plm(total_earnings ~ gdp_gr+gastoedu,
                # +poder_adq+
                #   desempleo+internet+movil+age_game+
                #   poblacion+pop_growth+rural_per,
                data = df_panel, model = "within")
resultado_fe <- summary(fe_model)

# Evaluo con housman cual me conviene

phtest(resultado_fe_index, resultado_re)
print("Chi-Squared:", hausman_test[1])
print("Degrees of Freedom:", hausman_test[2])
print("p-Value:", hausman_test[3])



# Efectos fijos pero con index # con constantes a lo largo del tiempo las otras variables
fe_model_index = plm(total_earnings ~ gdp_gr+gastoedu,
                     # +poder_adq+
                     #     desempleo+internet+movil+age_game+
                     #     poblacion+pop_growth+rural_per,
                     index=c("country", "year"), 
                     model="within", data=df_panel)
resultado_fe_index <- summary(fe_model_index)


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
