source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)

dados <- ("relatorio_old_town_road.xlsx")
relatorio <- read_xlsx(dados, sheet = "relatorio_vendas")
vendas <- read_xlsx(dados, sheet = "infos_vendas")
produtos <- read_xlsx(dados, sheet = "infos_produtos")
funcionarios <- read_xlsx(dados, sheet = "infos_funcionarios")
cidades <-  read_xlsx(dados, sheet = "infos_cidades")
clientes <-  read_xlsx(dados, sheet = "infos_clientes")
lojas <-  read_xlsx(dados, sheet = "infos_lojas")

vendas <- vendas %>%
  rename(SaleID = Sal3ID)

produtos <- produtos %>%
  rename(ItemID = Ite3ID )

novo_banco <- left_join(produtos, vendas, by= "ItemID")
novo_banco2 <- left_join(relatorio, novo_banco, by= "SaleID")

novo_banco2<- novo_banco2 %>%
  mutate(valor_real= UnityPrice*5.31) 

novo_banco2 <- novo_banco2 %>%
  mutate(ano= year(Date))
view(novo_banco2)

#analise 1:

library(dplyr)  
analise1 <- novo_banco2 %>%
  group_by(ano, StoreID) %>%
  summarise(receita_total = sum(valor_real), .groups = "drop") %>%
  group_by(ano) %>%
  summarise(receita_media = mean(receita_total))
view(analise1)

library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

analise1 <- novo_banco2 %>%
  group_by(ano, StoreID) %>%
  summarise(receita_total = sum(valor_real), .groups = "drop") %>%
  group_by(ano) %>%
  summarise(receita_media = mean(receita_total)) %>%
  mutate(
    freq = receita_media,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq_label = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c("R$ ", round(freq, 0), " (", freq_label, ")") %>% str_squish()
  )
histograma<- ggplot(analise1, aes(x = factor(ano), y = receita_media, label = label)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    x = "Ano",
    y = "Receita média (R$)",
    title = "Receita Média das Lojas (1880–1889)"
  ) +
  theme_estat() 
ggsave("receita_media_1880_1889.pdf", width = 158, height = 93, units = "mm")
histograma

