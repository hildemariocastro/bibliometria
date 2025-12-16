library(tidyverse)
library(bibliometrix)
library(readxl)
library(openxlsx)

biblioshiny()

## biblometria_ hildemario castro ######
### 15/12/2025 ### termos de busca
### camarões, trato digestório, tecnicas 1988 - 2025

##### importando banco de dados #####
M <- convert2df("base_dados_bibliometria_intestino_15_12_2025.csv", 
                dbsource = "scopus", format = "csv")


##### removendo artigos duplicados #####
M <- M[!duplicated(M$DI), ]
# Ou usar a função dedicada do pacote, se aplicável
# M <- unique(M)



#### exportando banco de dados em exel ####
write.xlsx(M, file = "M.xlsx", rowNames = FALSE)