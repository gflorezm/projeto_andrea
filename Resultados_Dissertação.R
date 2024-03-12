## Modelos a partir dos dados já processados #####

tab_mod <- read.csv ('dataframes/tab_mod.csv', sep = ',')
str(tab_mod)

if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(MASS)) install.packages('MASS'); library(MASS)
if(!require(effects)) install.packages('effects'); library(effects)

# Hipótese 1 - diversidade de fitofisionomias X riqueza
ggplot(efeitos2,
       aes(x = shannon, y = log(fit))) +
      geom_ribbon(aes(ymin = log(lower), ymax = log(upper)),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      labs(x = 'Diversidade de coberturas vegetais (Shannon)',
           y = 'Riqueza (log)') +
      theme_bw()

# Hipótese 2 - área X riqueza X cobertura vegetal

# selecao de modelos 

modfull <- glm(riqueza ~ areapa_scale*cob_scale + shannon, 
               data = tab_mod, family = poisson)

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

MASS::stepAIC(modfull, direction = c('both', 'backward', 'forward'),
              trace = TRUE)



# gráfico de efeitos 

modfull2 <- glm(riqueza ~ area_pq_km2*cob_vegetacao + shannon, 
                data = tabela_analise_0, family = poisson)


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




efeitos2 <- as.data.frame(effects::effect('shannon',
                                          modfull2))
