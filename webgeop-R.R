# install.packages("nome") das bibliotecas antes
library("jsonlite")
library(dplyr)
library(lubridate)
library(ggplot2)
library("gridExtra")
library(ggthemes)

json_file <- "C:/Users/Elsio Macedo/Downloads/export2.json"
json_data <- fromJSON(json_file)

#Substitui os acentos no arquivo json

names(json_data)<- gsub(" ", "_", names(json_data))                      
names(json_data) <- gsub("ã", "a", names(json_data))
names(json_data) <- gsub("á", "a", names(json_data))
names(json_data) <- gsub("ç", "c", names(json_data))



Tipo <- json_data %>%
  filter(Sistematica %in% c("Não")) %>%    
  count(Tipo_de_Solicitacao) %>%
  arrange(desc(n)) %>%
  mutate(proporcao = 100 * n/sum(n))


g1 <- ggplot(Tipo, aes(x = reorder(Tipo_de_Solicitacao, -n), y = proporcao)) + 
  geom_bar(stat = "identity", fill = "#01a2d9") + 
  geom_label(aes(label = n)) + 
  theme_economist()

Setor <- json_data %>%
  filter(!Setor %in% c("")) %>%
  count(Setor) %>%
  arrange(desc(n)) %>%
  mutate(proporcao = 100 * n/sum(n))


g2 <- ggplot(Setor, aes(x = reorder(Setor, -n), y = proporcao)) + 
  geom_bar(stat = "identity", fill = "#01a2d9") + 
  geom_label(aes(label = n)) + 
  theme_economist()

Serv <- json_data %>%
  filter(Sistematica %in% c("Não")) %>%  
  count(Tipo_de_Servico) %>%
  arrange(desc(n)) %>%
  mutate(proporcao = 100 * n/sum(n))


g3 <- ggplot(Serv, aes(x = reorder(Tipo_de_Servico, -n), y = proporcao)) + 
  geom_bar(stat = "identity", fill = "#01a2d9") + 
  geom_label(aes(label = n)) + 
  theme_economist()  

grid.arrange(g1 , g2 ,
             g3,
             ncol=1, nrow=3)
