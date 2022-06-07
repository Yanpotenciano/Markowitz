#stdlib
rm(list=ls(all=TRUE))
library(plotly)
library(fPortfolio)
library(BatchGetSymbols)
# todas as fun??es que subscrevem tidyverse devem vir antes
library(tidyverse)
library(readxl)
library(lubridate)
library(mondate)
library(gdata)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)
library(openxlsx)
library(tseries)
require(lubridate)
library(forecast)
setwd("Z:/GERENCIA/INVESTIMENTOS/MARKOWITZ-MAIN")

Ano_base = 2022
Mes_base = 5 
Meses_calculados = 6
Data_base = dmy("01/05/2022")
fund_register = "Fronteira_Markowitz_v14.xlsm"
cvm_link = "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_"
cvm_link2 = "inf_diario_fi_"





suffix_cvm = function(Meses_calculados,Data_base)
{
  Anos=0
  Meses=0
  #Starting dates
  Meses[1] = format(mondate(Data_base, "%m"))
  Anos[1] = format(mondate(Data_base, "%Y"))
  
  #Creating suffix to be used in cvm link
  suffix = rep(0, Meses_calculados)
  suffix[1] = paste0(Anos[1], Meses[1])
  
  for (i in 1:(Meses_calculados-1))
  {
    Meses[i+1] = format(mondate(Data_base- months(i), "%m"))
    Anos[i+1] = format(mondate(Data_base- months(i), "%Y"))
    suffix[i+1] = paste0(Anos[i+1], Meses[i+1])
  }
  
  return(suffix)
}


Download_CVM = function(n, suffix)
{
  
  #Temporary file to be created
  tmp = 'tmp.zip'
  
  lista = list()
  myurl = paste0(cvm_link, suffix,".zip")
  mydta = paste0(cvm_link2, suffix,".csv")
  
  for (i in 1:n)
  {
    download.file(url = myurl[i], destfile = tmp)
    unzip(tmp)
    
    # lista[[i]] = read_delim(mydta[i],
    #                         delim = ";",
    #                         escape_double = FALSE,
    #                         col_types = cols(DT_COMPTC = col_character()),
    #                         trim_ws = TRUE)
  }
  
  
  lista = do.call(rbind, lapply(list.files(path = ".", pattern = "csv"),
                                read.csv2))
  
  return(lista)
}


Read_CVM = function(n, suffix)
{
  lista = list()
  mydta = vector()
  
  lista = do.call(rbind.data.frame, lapply(list.files(path = ".", pattern = "csv"),
                                           read.csv, sep = ";"))
  
  return(lista)
}





read_funds = function(fund_register)
{
  Fundos <- read_excel(fund_register, sheet = "CADASTRO")
  Fundos <- Fundos %>%
    select(`Nome do fundo`,
           CNPJ,
           Bench,
           Resgate,
           `Taxa Administrativa`,
           `Grau de Risco`,
           `Enquadramento Legal`,
           `Limite do Enquadramento Legal`,
           Segmento
    ) %>%
    rename.vars(from = "CNPJ",
                to = "CNPJ_FUNDO")
  Fundos$Restricao_segmento= Fundos$`Limite do Enquadramento Legal`
  Fundos$Data_analise = paste0("0",Mes_base,"/",Ano_base)
  
  #Blacklist
  Fundos = filter(Fundos, Fundos$CNPJ_FUNDO != "35.292.597/0001-70")
  
  return(Fundos)
}



Analise_CNPJ = function(my_dt,Fundos)
{
  my_dt = inner_join(my_dt, Fundos, by= ("CNPJ_FUNDO"="CNPJ_FUNDO"))
  CNPJS = unique(my_dt$CNPJ_FUNDO)
  my_dt = my_dt[(order(as.Date(my_dt$DT_COMPTC, format="%Y/%m/%d"))),]
  
  my_dt2 = list()
  for (i in 1:length(CNPJS))
    my_dt2[[i]] = filter(my_dt , my_dt$CNPJ_FUNDO == CNPJS[i])
  
   for (j in 1:length(my_dt2))
   {
     for (i in 1:length(my_dt2[[j]]$CNPJ_FUNDO)) 
     {
       if(length(my_dt2[[j]]$CNPJ_FUNDO) == i)
         my_dt2[[j]]$rentabilidade[i] = 0
       else
         my_dt2[[j]]$rentabilidade[i] = (my_dt2[[j]]$VL_QUOTA[i]-my_dt2[[j]]$VL_QUOTA[i+1])/my_dt2[[j]]$VL_QUOTA[i+1]
     }
   }
  return(my_dt2)
}



















# Download dos arquivos do site da cvm 
suffix = suffix_cvm(Meses_calculados, Data_base)
#my_dt = Download_CVM(Meses_calculados, suffix)
my_dt = Read_CVM(Meses_calculados, suffix)

# Step ?
Fundos = read_funds(fund_register)

my_dt = Analise_CNPJ(my_dt,Fundos)








test_data_idx <- function(date)
  {
  y <- year(as.Date.character(date)) == 2022
  m <- month(as.Date.character(date)) == 2
  
  return(m & y)
  }



training_data_idx <- function(date)
{
  y <- year(as.Date.character(date)) == 2022
  m <- month(as.Date.character(date)) == 2
  
  return(!(m & y))
}  



serie= list()
teste = list()
decisao = 0
decisao2 = 0
decisao3 = 0
decisao_final = 0
normalidade = 0
normalidade2 = 0
s=0
#intervalo = length(my_dt) -1
dados_treino = list()
dados_teste = list()
acuracia = list()
previsao = list()


for (i in 1:length(my_dt)) {
  teste_idx = which(test_data_idx(my_dt[[i]]$DT_COMPTC))
  treino_idx = which(training_data_idx(my_dt[[i]]$DT_COMPTC))
  dados_treino[[i]] = ts(my_dt[[i]]$VL_QUOTA, start = treino_idx[1] , end= tail(treino_idx,1))
  dados_teste[[i]] = ts(my_dt[[i]]$VL_QUOTA, start = teste_idx[1] , end=tail(teste_idx,1) )
  serie[[i]] = auto.arima(dados_treino[[i]], allowdrift = TRUE,stepwise=TRUE)
  teste[[i]] = Box.test(residuals(arima(dados_treino[[i]], order =arimaorder(serie[[i]]) )),
                        type="Ljung-Box")
  s[i] = arimaorder(serie[[1]])[2]
  teste[[i]]$dickey_fuller = adf.test(diff(dados_treino[[i]], differences = s[i]))
  teste[[i]]$pptest = pp.test(diff(dados_treino[[i]],differences = s[i]))
  teste[[i]]$normalidade = jarque.bera.test(residuals(serie[[i]]))
  teste[[i]]$normalidade2 = shapiro.test(residuals(serie[[i]]))
  decisao[i] = if_else(teste[[i]]$p.value > 0.05 , "Não Rejeita independencia", "Rejeita independencia")
  #decisao2[i] = if_else(teste[[i]]$dickey_fuller$p.value > 0.05 , "Não Rejeita estacionariedade", "Rejeita estacionariedade")
  decisao3[i] = if_else(teste[[i]]$pptest$p.value > 0.05 , " Rejeita estacionariedade", "Rejeita não estacionariedade")
  normalidade[i] = if_else(teste[[i]]$normalidade$p.value > 0.05 , "Não Rejeita normalidade dos erros", "Rejeita normalidade dos erros")
  normalidade2[i] = if_else(teste[[i]]$normalidade2$p.value > 0.05 , "Não Rejeita normalidade dos erros", "Rejeita normalidade dos erros")
  
  previsao[[i]] = forecast(serie[[i]],h=147, level=95)
  acuracia[[i]]=  accuracy(forecast(serie[[i]],h=30, level=95),dados_teste[[i]])
  
  
  
}


table(decisao)
#table(decisao2)
table(decisao3)
8/52
51/52


#15,3% das series foi rejeitada a independencia
#98,1% das series foi rejeitada a não estacionariedade, então 1,9% são não estacionarios, 98,1% são estacionarios.
 

table(normalidade)
table(normalidade2)

47/52 
#90,3% das series foi rejeitada a hipotese de normalidade dos erros

fstat= list()
homoc= 0
i=1
t= length(serie[[1]]$residuals)
for (i in 1:t) {
  
  m = length(serie[[i]]$residuals)/2
  a1 = serie[[i]]$residuals[1:m]
  a2 = serie[[i]]$residuals[(m+1):length(serie[[i]]$residuals)]
  
  
  
  f= sum(a1^2/m)/(sum(a2^2/(m)))
  
  
  fstat[[i]] =qf(0.95,m,m)
  
  homoc[i] = if_else(fstat[[i]] < f , " Rejeita homocedasticidade", "Não rejeita homocedasticidade")

  }


10/42
#23,8% das series não sao homocedasticas



Restricoes = function(f,condicao)
{
  # f corresponde a coluna do enquadramento ou ao segmento
  # condição corresponde ao enquadramento do fundo
  vetor = which(f == condicao)
  
  return(vetor)
}







otimizacao = function(my_dt, Fundos)
{
  
  
  
  
  Desvio = 0
  dt = 0
  rentabilidademedia=0
  my_data = 0
  for (i in 1:length(my_dt))
  {
    rentabilidademedia[i] = mean(my_dt[[i]]$rentabilidade)
    Desvio[i] = sd(my_dt[[i]]$rentabilidade) 
    dt = filter(my_dt[[i]], my_dt[[i]]$DT_COMPTC == max(my_dt[[i]]$DT_COMPTC))
    my_data[i]= dt$VL_PATRIM_LIQ[1]
  }
  
  for (i in 1:length(my_dt))
    my_dt[[i]] = my_dt[[i]] %>% select(rentabilidade)
  
  teste = my_dt[[1]]
  for (j in 2:length(my_dt))
    teste = cbind(teste,my_dt[[j]])
  
  
  for (k in 1:length(Fundos$`Nome do fundo`)) 
    names(teste)[k]<- unique(Fundos$`Nome do fundo`)[k] 
  
  
  
  rentabilidade <- as.timeSeries(teste) #tangecyPortfolio() only accepts time.series object.
  
  
  constraints <- c('maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, IV, a")]=0.39',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, I, a")]=0.29',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, II, a")]=0.19',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, III")]=0.09',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 9º-A, III")]=0.09',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, VII, b")]=0.04',
                   'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, III")]=0.59',
                   'maxsumW[Restricoes(Fundos$Segmento,"Renda variável")]=0.29',
                   'maxsumW[Restricoes(Fundos$Segmento,"Exterior")]=0.09')
  
  
  
  portfolio.eficiente <- tangencyPortfolio(rentabilidade, spec = portfolioSpec(), constraints)
  
  Markowitz = data.frame(Fundos = Fundos$`Nome do fundo`,
                         Bench = Fundos$Bench,
                         Resgate = Fundos$Resgate,
                         Taxa_Administrativa =Fundos$`Taxa Administrativa`,
                         CNPJ = Fundos$CNPJ_FUNDO,
                         Grau_de_Risco = Fundos$`Grau de Risco`,
                         Enquadramento_legal = Fundos$`Enquadramento Legal`,
                         Limite_do_Enquadramento_Legal = Fundos$`Limite do Enquadramento Legal`,
                         Segmento = Fundos$Segmento,
                         Restricao_segmento = Fundos$Restricao_segmento,
                         Max_recursos = 0.14*my_data,
                         Rentabilidade_Media = rentabilidademedia,
                         Desvio = Desvio,
                         Recursosporcentagem = portfolio.eficiente@portfolio@portfolio$weights,
                         Data_analise = Fundos$Data_analise)
  
  Markowitz$Recursos = Markowitz$Max_recursos*Markowitz$Recursosporcentagem
  
  return(Markowitz)
}





Markowitz = otimizacao(my_dt,Fundos)

# Os indices de fundos que possuem alguma alocação

idx = which(Markowitz$Recursos!=0) 
Fundos_alocados = filter(Markowitz , Markowitz$Recursos !=0)



i=1
perda = list()
for (i in 1:length(idx)) {
  
  
  perda[[i]] = (previsao[[i]]$mean-previsao[[i]]$lower)*Fundos_alocados$Recursos[i] 
  
}




#perda = rbind(perda[[idx[1]]],perda[[idx[2]]],perda[[idx[3]]],perda[[idx[4]]],perda[[idx[5]]],perda[[idx[6]]],perda[[idx[7]]])

#perdas = data.frame(Fundos_alocados$Fundos, Fundos_alocados$CNPJ, Valor_perda = perda)
 


