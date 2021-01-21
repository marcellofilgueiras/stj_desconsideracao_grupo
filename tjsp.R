devtools::install_github("jjesusfilho/tjsp")
devtools::install_github("jjesusfilho/tjmg")

library(tjsp)
library(tjmg)
library(tidyverse)
library(stringr)

#Baixando Processos do TJ
tjsp::baixar_cjsg(livre="desconsideração personalidade jurídica grupo econômico", tipo = "A", diretorio = "dados_tjsp2")
baixar_cs

#Lendo os processos e Criando a tabela
desconsideração_tjsp <- tjsp::ler_cjsg(arquivos = NULL, diretorio = "dados_tjsp2")
#Limitando a Agravos de Instrumento
agravos <- filter(desconsideração_tjsp, classe == "Agravo de Instrumento")

#filtrando a Incidente de Desconsideração da Personalidade Jurídica,
#Sem questões Processuais, e sem casos reguladosd pelo CDC

incidente <- agravos %>% 
  filter(str_detect(ementa,"incidente"))
inc_desvio <- incidente %>% 
  filter(str_detect(ementa,"desvio de finalidade|confusão patrimonial"))

exclusoes <- inc_desvio %>% 
  filter(str_detect(ementa,"nulidade|nulo")) %>% 
  pull("processo")
inc_desvio_sem_nulidade <- filter(inc_desvio,!processo %in% exclusoes)

exclusoes2 <- inc_desvio_sem_nulidade %>% 
  filter(str_detect(ementa,"cdc|CDC|28")) %>% 
  pull("processo")
inc_desvio_sem_nulidade_cdc <- filter(inc_desvio_sem_nulidade,!processo %in% exclusoes2)

#marco temporal
inc_desvio_sem_nulidade_cdc_2016 <- inc_desvio_sem_nulidade_cdc%>%
  filter(ano_julgamento>=2016)

#Processos antes de depois liberdade econômica
liberdadeeconomica <- inc_desvio_sem_nulidade_cdc_2016 %>% 
   mutate(antes_depois_LLE = ifelse(data_julgamento < as.Date("2019-04-30"),"anterior_LLE","posterior_LLE"))


#Algumas estatisticas
agravosporano = liberdadeeconomica%>%
  group_by(ano_julgamento)%>%
  count(antes_depois_LLE)
  
media_agravos_mes<-liberdadeeconomica%>%
  count(antes_depois_LLE)%>%
  mutate(meses = c(40,12))%>%
  summarize(media = n/meses)


#graficos
#numero de processos por mes
library(ggplot2)

liberdadeeconomicapormes<- liberdadeeconomica%>% mutate(anoMes= paste(ano_julgamento, mes_julgamento))

liberdadeeconomicapormes%>%
group_by(anoMes)%>%summarize(anoMes)

ggplot(agravosporano, aes(x=ano_julgamento, y=n, fill=antes_depois_LLE)) + geom_col()

#numero de processos por corte

liberdadeeconomica_corte <- liberdadeeconomica%>%
  group_by(ano_julgamento) %>%
  count(orgao_julgador)%>% arrange(desc(orgao_julgador))

ggplot(liberdadeeconomica_corte, aes(x=n, y=orgao_julgador, fill=ano_julgamento)) + geom_col()
