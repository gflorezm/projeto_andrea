tab_1 <- read.csv('dataframes/areapq_veg_corrigida_riq.csv', sep = ',')


str(tab_1)

library(tidyverse)
library(tidyr)

#pacote para fazer analise de diversidade
install.packages('vegan')
library(vegan)


# criar uma tabela de classes vegetais em cada coluna, com o valor da área (m2) por parque id.
# mudei o id.pq para ser só ID 
# Mudei o nome das classes vegetais para usar o código. O formato é classe_1 a classe_15?

area_veg <- tab_1 |> 
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

div_paisagem$shannon <- area_veg |> 
      select(classe_13:classe_6) |> 
      vegan::diversity()

div_paisagem$n_areas <- area_veg |> 
      select(classe_13:classe_6) |> 
      vegan::specnumber()


# tabela riqueza_area ####
riqueza_area <- tab_1 |> 
      select(av_id, riqueza, area_pq_m2) |> 
      group_by(av_id) |> 
      summarize(riqueza = mean(riqueza),
                area_pq_m2 = mean(area_pq_m2, na.rm = TRUE)) |> 
      rename(ID = `av_id`) |> 
      filter(!is.na(riqueza))


tabela_analise_0 <- riqueza_area |> 
      left_join(area_veg)
      


### primeiro modelinho de teste ####

mod <- lm(riqueza ~ area_pq_m2, data = tabela_analise_0)

summary(mod)
      

      