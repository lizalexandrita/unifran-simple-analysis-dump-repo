## Preparação do Ambiente
library('readtext')
library('tidyverse')

## Leitura do Arquivo
idades_text <- readtext(file = 'idades.docx')
glimpse(idades_text)

## Preparação do texto para dataframe numérico para análise
tmp <- strsplit(x=idades_text$text, split=", ")
glimpse(tmp)
tmp <- as.data.frame(tmp)
glimpse(tmp)
names(tmp) <- 'idades'
tmp$idades <- as.numeric(tmp$idades)
glimpse(tmp)

## Análise - Estatística Descritiva
summary(tmp)
tmp %>% summarize(
  min = min(idades), 
  max = max(idades),
  mean = mean(idades),
  median = median(idades), 
  var = var(idades),
  sd = var(idades) ^ (1/2))

## Análise - Gráfica
tmp %>% boxplot()

tmp %>% ggplot(aes(idades)) + 
  geom_density(fill = "blue")

tmp %>% ggplot(aes(idades)) + 
  geom_histogram(fill = "blue")
