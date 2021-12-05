# Preparação do ambiente
#@title Atividade 1 PIC
#install.packages("tidyverse")
#install.packages("rmarkdown")
library(tidyverse)
library(rmarkdown)
# Ler arquivo e analisar campos
pacientes <- read_csv('pacientes.csv')
glimpse(pacientes)
pacientes$ID <- as.factor(pacientes$ID)
pacientes$`Código do Procedimento Principal` <- as.factor(pacientes$`Código do Procedimento Principal`)
# Estatísticas e Gráficos
pacientes %>% ggplot(aes(`Idade do Segurado`)) + 
  geom_density(fill = "blue")
summary(pacientes)
pacientes %>% summarize(
  min = min(`Idade do Segurado`), 
  max = max(`Idade do Segurado`),
  mean = mean(`Idade do Segurado`),
  median = median(`Idade do Segurado`), 
  var = var(`Idade do Segurado`),
  sd = var(`Idade do Segurado`) ^ (1/2))
pacientes %>% ggplot(aes(`Valor Total Liberado`)) + 
  geom_density(fill = "blue")
range(pacientes$`Valor Total Liberado`)
pacientes <- drop_na(pacientes)
pacientes %>% summarize(
  min = min(`Valor Total Liberado`), 
  max = max(`Valor Total Liberado`),
  mean = mean(`Valor Total Liberado`),
  median = median(`Valor Total Liberado`), 
  var = var(`Valor Total Liberado`),
  sd = var(`Valor Total Liberado`) ^ (1/2))
pacientes %>% filter(`Valor Total Liberado` == max(`Valor Total Liberado`))
pacientes %>% filter(`Idade do Segurado` == max(`Idade do Segurado`))
pacientes %>% ggplot(aes(`Valor Total Liberado`)) + 
  geom_density(fill = "blue")
pacientes %>% ggplot(aes(`Valor Total Liberado`)) + 
  geom_histogram(fill = "blue")
valor_proced <- pacientes %>% group_by(`Código do Procedimento Principal`) %>% summarise(valor_total = sum(`Valor Total Liberado`))
glimpse(valor_proced)
valor_proced %>% summarize(
  min = min(valor_total), 
  max = max(valor_total),
  mean = mean(valor_total),
  median = median(valor_total), 
  var = var(valor_total),
  sd = var(valor_total) ^ (1/2))
valor_proced %>% filter(valor_total == max(valor_total))

