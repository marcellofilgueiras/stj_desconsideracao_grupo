devtools::install_github("jjesusfilho/stj")

library(ggplot2)
library(stj)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)


#Código Para Baixar os Julgados do Site
baixar_julgados_stj(livre="desconsideração personalidade jurídica grupo econômico",
                    operador = "e", repo = "ACOR",
                    data_inicial = "01/02/2002", 
                    data_final = "01/08/2020",diretorio="julgados")

#Organiza os julgados em tabelas
desconsideração_stj <- ler_julgados_stj(diretorio = "julgados")

#transformando e arrumando a tabela, por ano de julgamento, definindo marco temporal
desconsideração_stj_com_ano <-desconsideração_stj %>%
    mutate(ano = year(ymd(data_julgamento)), mes= month(ymd(data_julgamento)))

desconsideração_pos_CPC15 <- desconsideração_stj_com_ano %>%
  filter(ano>2016)

#analisando os dados
count(desconsideração_stj_com_ano, classe)
count(desconsideração_stj_com_ano, relator)
count(desconsideração_stj_com_ano, orgao_julgador)

classeporano <- count(desconsideração_pos_CPC15, ano,classe)
count(desconsideração_pos_CPC15, ano)
count(desconsideração_pos_CPC15, classe)
count(desconsideração_pos_CPC15, relator)
count(desconsideração_pos_CPC15, orgao_julgador)



#Função de ctrl + F nos julgados, buscando padroes

negaram <- regex('negara?m|nega|nega', ignore_case = TRUE)
parcial <- regex('parcial', ignore_case = TRUE)
deram <- regex('deram|mantiv|dá-se', ignore_case = TRUE)
extinto <- regex('extin', ignore_case = TRUE)
nulo <- regex('nul', ignore_case = TRUE)
nconhec <- regex('não conhec|deixo de conhec', ignore_case = TRUE)

tipos_decisao <- function(decisoes) {
  case_when(
    str_detect(decisoes, negaram) ~ 'negado',
    str_detect(decisoes, parcial) ~ 'parcial',
    str_detect(decisoes, deram) ~ 'provido',
    str_detect(decisoes, extinto) ~ 'extinto',
    str_detect(decisoes, nulo) ~ 'nulo',
    str_detect(decisoes, nconhec) ~ 'não conhecido',
    TRUE ~ "outros")
}

resultados <- tipos_decisao(desconsideração_stj$dispositivo)
resultados

table(resultados)

