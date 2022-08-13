tab_1 <- read.csv('dataframes/tab_1.csv', sep = ',')


library(tidyverse)
library(tidyr)

# cirar uma planilha com os atributos do parque ####
parques <- tab_1 |> 
      select(novo_id_pq:cd_coord_y) |> 
      unique() |> 
      group_by(novo_id_pq, nome_pq, class_area_verde, categoria_area_verde) |> 
      summarize(
            area_pq = mean(area_pq, na.rm = TRUE),
            cd_coord_x = mean(cd_coord_x, na.rm = TRUE),
            cd_coord_y = mean(cd_coord_y, na.rm = TRUE)
      ) |> 
      rename(ID = novo_id_pq)

write.csv(parques, file = 'dataframes/0_parques.csv', row.names = FALSE,
          fileEncoding = "UTF8")



# criar uma tabela de classes vegetais em cada coluna, com o valor da área (m2) por parque id.
# mudei o id.pq para ser só ID 
# Mudei o nome das classes vegetais para usar o código. O formato é classe_1 a classe_15?

area_clas_veg <- tab_1 |> 
      select(novo_id_pq, cd_categ, qt_area) |> 
      mutate(categ_veg = paste0('classe_', cd_categ)) |> 
      group_by(novo_id_pq, categ_veg) |> 
      summarize(area = sum(qt_area, na.rm = TRUE)) |> 
      tidyr::pivot_wider(names_from = categ_veg, values_from = area) |> 
      rename(ID = `novo_id_pq`)

write.csv(area_clas_veg, file = 'dataframes/1_id_area_class_veg.csv', row.names = FALSE,
          fileEncoding = "UTF8")

# tabela riqueza de aves #####

riqueza_aves <- tab_1 |> 
      select(novo_id_pq, riqueza) |> 
      mutate(riqueza = as.numeric(riqueza)) |> 
      group_by(novo_id_pq) |> 
      summarize(riqueza = mean(riqueza, na.rm = TRUE)) |> 
      rename(ID = `novo_id_pq`)

write.csv(riqueza_aves, file = 'dataframes/1_id_riqueza_aves.csv', row.names = FALSE,
          fileEncoding = "UTF8")

# TABELA PARQUES AREA E COORD ####

pq_area_unico <- parques |> 
      group_by(ID) |> 
      summarize(
            area_pq = sum(area_pq, na.rm = TRUE),
            cd_coord_x = mean(cd_coord_x, na.rm = TRUE),
            cd_coord_y = mean(cd_coord_y, na.rm = TRUE)
      )

write.csv(pq_area_unico, file = 'dataframes/1_id_area_coord_parque.csv', 
          row.names = FALSE,
          fileEncoding = "UTF8")

# tabela auxiliar categorias vegetais ####

categorias_vegetais <- tab_1 |> 
      mutate(categ_veg = paste0('classe_', cd_categ)) |> 
      select(classes_vegetais, categ_veg) |> 
      unique()

write.csv(categorias_vegetais, file = 'dataframes/0_classes_vegetais.csv', 
          row.names = FALSE,
          fileEncoding = "UTF8")
