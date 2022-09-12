## Detalhe dos data frames

### Tabelas auxiliares

Permitem identificar os parques por nome e outros atributos administrativos assim como as classes vegetais por nome e código

`0_parques.csv`: informaçãs dos parques associados ao ID único (nome, tipo de parque, etc). Parques com mais de uma fase são mantidos em linhas diferentes mas com o mesmo ID.

`0_classes_vegetais.csv`: conecta o código da classe (`classe_1 a classe_17`) com o nome e descrição. Necessária para conferir a que tipo de vegetação corresponde cada código.

### Tabelas de análise

Tabelas para serem usadas nos modelos de regressão

`1_id_area_class_veg.csv`: área de cada classe de vegetação por parque (ID do parque).

`1_id_area_coord_parque.csv`: coordenadas e área do parque por ID único.

`1_id_riqueza_aves.csv`: Riqueza de aves por ID do parque.

### Tabelas iniciais

`tab_1_areaveg.csv` e `tab_1.csv`: as tabelas preenchidas na mão das quais sairam as tabelas de análise e auxiliares. **Não mexer**
