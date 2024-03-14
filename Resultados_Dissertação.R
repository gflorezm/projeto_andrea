## Modelos a partir dos dados já processados #####

tab_mod <- read.csv ('dataframes/tab_mod.csv', sep = ',')
tab_ana_arbo <- read.csv ('dataframes/teste', sep = ',')


if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(MASS)) install.packages('MASS'); library(MASS)
if(!require(effects)) install.packages('effects'); library(effects)

# Hipótese 1 - diversidade de fitofisionomias X riqueza

library(ggplot2)

efeitos2 <- as.data.frame(effects::effect('shannon',
                                          modfull2))
ggplot(efeitos2,
       aes(x = shannon, y = log(fit))) +
      geom_ribbon(aes(ymin = log(lower), ymax = log(upper)),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      labs(x = 'Diversidade de coberturas vegetais (Shannon)',
           y = 'Riqueza (log)') +
      theme_bw()

# Hipótese 2 - riqueza X cobertura florestal e arbórea


modfull3 <- glm(riqueza ~ area_pq_km2*cob_vegetacao + shannon, 
                data = tab_mod_arbflo, family = poisson)

summary(modfull3)


# Hipótese 3 - área X riqueza X cobertura vegetal

# selecao de modelos 

modfull <- glm(riqueza ~ areapa_scale*cob_scale + shannon, 
               data = tab_mod, family = poisson)

MASS::stepAIC(modfull, direction = c('both', 'backward', 'forward'),
              trace = TRUE)

mod0 <- glm(riqueza ~ 1,
            data = tab_mod, family = poisson)

mod1 <- glm(riqueza ~ areapa_scale + shannon, 
            data = tab_mod, family = poisson)

mod2 <- glm(riqueza ~ areapa_scale + cob_scale, 
            data = tab_mod, family = poisson)

mod3 <- glm(riqueza ~ shannon + cob_scale, 
            data = tab_mod, family = poisson)

mod4 <- glm(riqueza ~ areapa_scale,
            data = tab_mod, family = poisson)

mod5 <- glm(riqueza ~ shannon, 
            data = tab_mod, family = poisson)

mod6 <- glm(riqueza ~ cob_scale, 
            data = tab_mod, family = poisson)

bbmle::AICctab(mod0, mod1, mod2, mod3, mod4, mod5, mod6, modfull,
               base = TRUE)

#Tabela Modelos

install.packages("sjPlot")
library(sjPlot)

pl <- c(
      '(Intercept)' = "Intercept",
      'shannon' = "Shannon",
      'cob_scale' = "Cobertura de Vegetação", 
      'areapa_scale' ="Área do Parque (Km2)",
      'areapa_scale:cob_scale' ="Área do Parque (Km2) x Cobertura de Vegetação"
)

tab_model(modfull, mod1, dv.labels = c("Modelo Cheio","Modelo 1"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras",
          string.p = "p",
          string.est="Estimate")

tab_model(mod2, mod4, dv.labels = c("Modelo 2","Modelo 4"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")

tab_model(mod3, mod5, dv.labels = c("Modelo 3","Modelo 5"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")

tab_model(mod6, mod0, dv.labels = c("Modelo 6","Modelo 0"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")

# gráfico de efeitos 

modfull2 <- glm(riqueza ~ area_pq_km2*cob_vegetacao + shannon, 
                data = tab_mod, family = poisson)

summary(modfull2)

efeitos <- as.data.frame(
      effects::effect(
            'area_pq_km2:cob_vegetacao',
            modfull2,
            xlevels=list(cob_vegetacao = c(60, 80, 100))
      )
) |>
      mutate(
            cobs = factor(
                  paste('Cobertura: ',cob_vegetacao, "%", sep=""), 
                  levels = c("Cobertura: 60%", "Cobertura: 80%", "Cobertura: 100%")
            ),
            fit = log(fit),
            upper = log(upper),
            lower = log(lower)
      )


extract_coeff_df <- function(y,x) {
      
      mod <- lm(y ~ x)
      
      dt <- data.frame(
            alpha = coef(mod)[1],
            beta = coef(mod)[2]
      )
      
      return(dt)
}

beta_coef <- efeitos %>% 
      group_by(cobs) %>% 
      summarize(extract_coeff_df(fit, area_pq_km2))

# gráfico 1
efeitos %>% 
      # filter(log(upper) < 10) %>%
      ggplot(aes(x = area_pq_km2, y = fit)) +
      geom_ribbon(aes(ymin = lower, 
                      ymax = upper),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      facet_grid(. ~ cobs) +
      labs(x = expression ("Área do parque ("~km^2~")"),
           y = 'Riqueza (log)') +
      theme_bw()

#------------------- Hipotese 4 - Urbanização ---------------------------------

#Teste dos Modelos 

modfull4 <- glm(riqueza ~ cob_scale*BV_M2 + shannon +areapa_scale, 
                data = tab_mod, family = poisson)

summary(modfull4)

MASS::stepAIC(modfull4, direction = c('both', 'backward', 'forward'),
              trace = TRUE)

mod8 <- glm(riqueza ~ areapa_scale + shannon + BV_M2, 
            data = tab_mod, family = poisson)

mod9 <- glm(riqueza ~ areapa_scale + cob_scale + BV_M2, 
            data = tab_mod, family = poisson)

mod10 <- glm(riqueza ~ shannon + cob_scale + BV_M2, 
             data = tab_mod, family = poisson)

mod11 <- glm(riqueza ~ areapa_scale + BV_M2,
             data = tab_mod, family = poisson)

mod12 <- glm(riqueza ~ shannon + BV_M2, 
             data = tab_mod, family = poisson)

mod13 <- glm(riqueza ~ cob_scale +BV_M2, 
             data = tab_mod, family = poisson)

mod14 <- glm(riqueza ~ BV_M2, 
             data = tab_mod, family = poisson)

bbmle::AICctab(mod7, mod8, mod9, mod10, mod11, mod12, mod13, modfull4,
               base = TRUE)

plot(modfull4)

summary(modfull4)

modfull5 <- glm(riqueza ~ BV_M2*cob_vegetacao + shannon + area_pq_km2, 
                data = tab_mod, family = poisson)
summary(modfull5)

#Tabela Modelos

pl <- c(
      '(Intercept)' = "Intercept",
      'BV_M2' = "Volume de Edificações (m3/m2)",
      'shannon' = "Shannon",
      'cob_scale' = "Cobertura de Vegetação", 
      'areapa_scale' ="Área do Parque (Km2)",
      'BV_M2:cob_scale' ="Área do Parque (Km2) x Cobertura de Vegetação"
)

tab_model(modfull4, mod7, dv.labels = c("Modelo Cheio","Modelo 1"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras",
          string.p = "p",
          string.est="Estimate")

tab_model(mod8, mod9, dv.labels = c("Modelo 2","Modelo 4"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")

tab_model(mod10, mod11, dv.labels = c("Modelo 3","Modelo 5"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")

tab_model(mod12, mod13, dv.labels = c("Modelo 6","Modelo 0"), 
          pred.labels = pl, 
          transform = NULL, 
          show.aic=T,
          show.est=T,
          show.r2=F,
          string.pred = "Variáveis preditoras", 
          string.p = "p",
          string.est="Estimate")



#Gráfico de efeitos

efeitos <- as.data.frame(
      effects::effect(
            'BV_M2:cob_vegetacao',
            modfull5,
            xlevels=list(cob_vegetacao = c(60, 80, 100))
      )
) |>
      mutate(
            cobs = factor(
                  paste('Cobertura: ',cob_vegetacao, "%", sep=""), 
                  levels = c("Cobertura: 60%", "Cobertura: 80%", "Cobertura: 100%")
            ),
            fit = log(fit),
            upper = log(upper),
            lower = log(lower)
      )

extract_coeff_df <- function(y,x) {
      
      mod <- lm(y ~ x)
      
      dt <- data.frame(
            alpha = coef(mod)[1],
            beta = coef(mod)[2]
      )
      
      return(dt)
}

beta_coef <- efeitos %>% 
      group_by(cobs) %>% 
      summarize(extract_coeff_df(fit, BV_M2))



# gráfico 1
efeitos %>% 
      # filter(log(upper) < 10) %>%
      ggplot(aes(x = BV_M2, y = fit)) +
      geom_ribbon(aes(ymin = lower, 
                      ymax = upper),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      facet_grid(. ~ cobs) +
      labs(x = expression ("Volume de Edificações ("~m^3/m^2~")"),
           y = 'Riqueza (log)') +
      theme_bw()




#Hipótese água

efeitos3 <- as.data.frame(effects::effect('AGUA_pq_m2',
                                          modfull7))

ggplot(efeitos3,
       aes(x = AGUA_pq_m2, y = log(fit))) +
      geom_ribbon(aes(ymin = log(lower), ymax = log(upper)),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      labs(x = expression ("Presença de Corpos de água ("~m^2~")"),
           y = 'Riqueza (log)') +
      theme_bw()
