tab_1 <- read.csv('tab_1_areaveg.csv')


library(tidyverse)
library(tidyr)

# cirar uma planilha com os atributos do parque ####
parques <- tab_1 |>
      select(id.pq:`categoria.area.verde`) |> 
      unique()
      

# criar uma tabela de classes vegetais em cada coluna, com o valor da área (m2) por parque id.
# mudei o id.pq para ser só ID 
# Mudei o nome das classes vegetais para usar o código. O formato é classe_1 a classe_15?

area_clas_veg <- tab_1 |> 
      select(id.pq, cd_categ, qt_area) |> 
      mutate(categ_veg = paste0('classe_', cd_categ)) |> 
      group_by(id.pq, categ_veg) |> 
      summarize(area = sum(qt_area, na.rm = TRUE)) |> 
      tidyr::pivot_wider(names_from = categ_veg, values_from = area) |> 
      rename(ID = `id.pq`)

# conferir se existem ids faltantes porque pensavamos que estavam sem dados (mas não existem tais ids)
which(!1:501 %in% area_clas_veg$ID)
