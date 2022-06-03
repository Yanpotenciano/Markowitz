# Obter Fundos Cadastrados 

source("stdlib.R")


# Obtendo os fundos de analise e retirando o fundo que ta apresentando problemas
# Apenas 1 fundo dentre os cadastrados apresenta problema, por isso foi retirado.
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


# Selecionando apenas os cnpjs dos fundos cadastrados 
# e ordenando por data da mais recente até a mais distante.
Analise_CNPJ = function(my_dt,Fundos)
  {
  my_dt = inner_join(my_dt, Fundos, by= ("CNPJ_FUNDO"="CNPJ_FUNDO"))
  CNPJS = unique(my_dt$CNPJ_FUNDO)
  my_dt = my_dt[rev(order(as.Date(my_dt$DT_COMPTC, format="%Y/%m/%d"))),]
  
  
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


Restricoes = function(f,condicao)
{
  # f corresponde a coluna do enquadramento ou ao segmento
  # condição corresponde ao enquadramento do fundo
  vetor = which(f == condicao)
  
  return(vetor)
}



# 
# 
# otimizacao = function(my_dt, Fundos)
#   {
# 
# 
#   
#     
#   Desvio = 0
#   dt = 0
#   
#   for (i in 1:length(my_dt))
#     {
#     rentabilidademedia[i] = mean(my_dt[[i]]$rentabilidade)
#     Desvio[i] = sd(my_dt[[i]]$rentabilidade) 
#     dt = filter(my_dt[[i]], my_dt[[i]]$DT_COMPTC == max(my_dt[[i]]$DT_COMPTC))
#     my_data[i]= dt$VL_PATRIM_LIQ[1]
#     }
#   
#   for (i in 1:length(my_dt))
#     my_dt[[i]] = my_dt[[i]] %>% select(rentabilidade)
#   
#   teste = my_dt[[1]]
#   for (j in 2:length(my_dt))
#     teste = cbind(teste,my_dt[[j]])
#   
#   
#   for (k in 1:length(Fundos$`Nome do fundo`)) 
#     names(teste)[k]<- unique(Fundos$`Nome do fundo`)[k] 
#   
#   
#   
#   rentabilidade <- as.timeSeries(teste) #tangecyPortfolio() only accepts time.series object.
#   
#   
#   constraints <- c('maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, IV, a")]=0.39',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, I, a")]=0.29',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, II, a")]=0.19',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 8º, III")]=0.09',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 9º-A, III")]=0.09',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, VII, b")]=0.04',
#                    'maxsumW[Restricoes(Fundos$`Enquadramento Legal`,"Art. 7º, III")]=0.59',
#                    'maxsumW[Restricoes(Fundos$Segmento,"Renda variável")]=0.29',
#                    'maxsumW[Restricoes(Fundos$Segmento,"Exterior")]=0.09')
#   
# 
#   
#   portfolio.eficiente <- tangencyPortfolio(rentabilidade, spec = portfolioSpec(), constraints)
# 
#   Markowitz = data.frame(Fundos = Fundos$`Nome do fundo`,
#                          Bench = Fundos$Bench,
#                          Resgate = Fundos$Resgate,
#                          Taxa_Administrativa =Fundos$`Taxa Administrativa`,
#                          CNPJ = Fundos$CNPJ_FUNDO,
#                          Grau_de_Risco = Fundos$`Grau de Risco`,
#                          Enquadramento_legal = Fundos$`Enquadramento Legal`,
#                          Limite_do_Enquadramento_Legal = Fundos$`Limite do Enquadramento Legal`,
#                          Segmento = Fundos$Segmento,
#                          Restricao_segmento = Fundos$Restricao_segmento,
#                          Max_recursos = 0.14*my_data,
#                          Rentabilidade_Media = rentabilidademedia,
#                          Desvio = Desvio,
#                          Recursosporcentagem = portfolio.eficiente@portfolio@portfolio$weights,
#                          Data_analise = Fundos$Data_analise)
# 
#   Markowitz$Recursos = Markowitz$Max_recursos*Markowitz$Recursosporcentagem
# 
#   return(Markowitz)
#   }

