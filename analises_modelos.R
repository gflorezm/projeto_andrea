tab_1 <- read.csv('dataframes/areapq_veg_corrigida_riq.csv', sep = ',')


str(tab_1)

library(tidyverse)
library(tidyr)

#pacote para fazer analise de diversidade
install.packages('vegan')
library(vegan)

# tentativa de acertar o pq_id ####

tab_0 <- tab_1 |> 
      filter(is.na(pq_id))

id <- 200

tab_0$pq_id[1] <- 200

for(i in 2:length(tab_0$av_id)) {
      if(tab_0$av_id[i] == tab_0$av_id[i-1] 
         & tab_0$area_pq_m2[i] == tab_0$area_pq_m2[i-1])
      {
            tab_0$pq_id[i] <- tab_0$pq_id[i-1]
      }
      else{
            id <- id + 1
            tab_0$pq_id[i] <- id
      }
}
      

tab1_1 <- tab_1 |> 
      filter(!is.na(pq_id)) |> 
      bind_rows(tab_0)

# criar uma tabela de classes vegetais em cada coluna, com o valor da área (m2) por parque id.
# mudei o id.pq para ser só ID 
# Mudei o nome das classes vegetais para usar o código. O formato é classe_1 a classe_15?

area_veg <- tab1_1 |> 
      select(av_id, cd_categ, area_veg_m) |> 
      mutate(categ_veg = paste0('classe_', cd_categ)) |> 
      group_by(av_id, categ_veg) |> 
      summarize(area = sum(area_veg_m, na.rm = TRUE)) |> 
      tidyr::pivot_wider(names_from = categ_veg, values_from = area) |> 
      rename(ID = `av_id`) |> 
      mutate_at(vars(classe_13:classe_6), ~replace(., is.na(.), 0))

# tabela da diversidade da paisagem ####

div_paisagem <- data.frame(
      'ID' = area_veg$ID,
      'area_vegetada' = area_veg |> 
            select(classe_13:classe_6) |> 
            rowSums(na.rm = TRUE))

# diversidade de shannon 
div_paisagem$shannon <- area_veg |> 
      select(classe_13:classe_6) |> 
      vegan::diversity()

# numero de classes de vegetação
div_paisagem$n_classes <- area_veg |> 
      select(classe_13:classe_6) |> 
      vegan::specnumber()


# tabela riqueza_area ####
riqueza_area <- tab1_1 |> 
      select(av_id, pq_id, riqueza, area_pq_m2) |> 
      group_by(av_id, pq_id) |> 
      summarize(riqueza = mean(riqueza),
                area_pq_m2 = mean(area_pq_m2, na.rm = TRUE)) |> 
      rename(ID = `av_id`) |> 
      filter(!is.na(riqueza)) |> 
      group_by(ID) |> 
      summarize(riqueza = mean(riqueza),
                area_pq_m2 = sum(area_pq_m2, na.rm = TRUE))

# tabela para a análise
tabela_analise_0 <- riqueza_area |> 
      left_join(div_paisagem) |> 
      mutate(cob_vegetacao = 100*area_vegetada/area_pq_m2) |> 
      mutate(cob_vegetacao = ifelse(cob_vegetacao > 100, 
                                    100, cob_vegetacao))


      
# correlacao entre preditores
corrplot::corrplot(
      cor(
            tabela_analise_0 |> 
                  select(area_pq_m2:cob_scale)
      )
)

# correlacao numerica
cor(
      tabela_analise_0 |> 
            select(area_pq_m2:cob_scale)
)

### primeiro modelinho de teste ####

mod <- glm(riqueza ~ area_pq_m2 + shannon + cob_vegetacao, 
          data = tabela_analise_0, family = poisson)

plot(mod)


## reescalar a variavel area porque é muito grande ###

tabela_analise_0 <- tabela_analise_0 |> 
      mutate(areapa_scale = (area_pq_m2 - mean(area_pq_m2))/sd(area_pq_m2),
             cob_scale = asin(sqrt(cob_vegetacao/100)),
             area_pq_km2 = area_pq_m2/1000)

save(tabela_analise_0, file = 'dataframes/dados_analise.RData')
 

# Modelos a partir dos dados já processados #####
load('dataframes/dados_analise.RData')

if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(MASS)) install.packages('MASS'); library(MASS)
if(!require(effects)) install.packages('effects'); library(effects)


modfull <- glm(riqueza ~ areapa_scale*cob_scale + shannon, 
           data = tabela_analise_0, family = poisson)

plot(modfull)  

summary(modfull)
summary(mod)

# selecao de modelos 
MASS::stepAIC(modfull, direction = c('both', 'backward', 'forward'),
              trace = TRUE)


mod0 <- glm(riqueza ~ 1,
            data = tabela_analise_0, family = poisson)

mod1 <- glm(riqueza ~ areapa_scale + shannon, 
            data = tabela_analise_0, family = poisson)

mod2 <- glm(riqueza ~ areapa_scale + cob_scale, 
            data = tabela_analise_0, family = poisson)

mod3 <- glm(riqueza ~ shannon + cob_scale, 
            data = tabela_analise_0, family = poisson)

mod4 <- glm(riqueza ~ areapa_scale,
            data = tabela_analise_0, family = poisson)

mod5 <- glm(riqueza ~ shannon, 
            data = tabela_analise_0, family = poisson)

mod6 <- glm(riqueza ~ cob_scale, 
            data = tabela_analise_0, family = poisson)

bbmle::AICctab(mod0, mod1, mod2, mod3, mod4, mod5, mod6, modfull,
               base = TRUE)

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

# gráfico 2
ggplot(efeitos2,
       aes(x = shannon, y = log(fit))) +
      geom_ribbon(aes(ymin = log(lower), ymax = log(upper)),
                  alpha = 0.4) +
      geom_line(color = 'blue') +
      labs(x = 'Diversidade de coberturas vegetais (Shannon)',
           y = 'Riqueza (log)') +
      theme_bw()

summary(modfull)
confint(modfull)
