# Created by Yan - 2022-04-12
setwd("Z:/GERENCIA/INVESTIMENTOS/MARKOWITZ-MAIN")
rm(list=ls(all=TRUE))

# Importing configuration variables
source("Config.R")

# Importing everything else
source("download_CVM.R")
source("Analise_Fundos.R")

# Download dos arquivos do site da cvm 
suffix = suffix_cvm(Meses_calculados, Data_base)
#my_dt = Download_CVM(Meses_calculados, suffix)
my_dt = Read_CVM(Meses_calculados, suffix)

# Step ?
Fundos = read_funds(fund_register)
my_dt = Analise_CNPJ(my_dt,Fundos)




otimizacao = function(my_dt, Fundos)
{
  
  
  
  
  Desvio = 0
  dt = 0
  
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