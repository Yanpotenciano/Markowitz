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










Analise_Markowitz= function(my_dt,fund_register){
  
  
  
  
  for(i in 1:length(Fundos)){
    if(Fundos$`Enquadramento Legal`[i]== "Art. 7º, I, b"){
      Fundos$Restricao_segmento[i]== 1.00
      
    } else if(Fundos$`Enquadramento Legal`[i]== "Art. 7º, IV, a"){
      Fundos$Restricao_segmento[i]== 0.39
    } else if(Fundos$`Enquadramento Legal`[i]== "Art. 8º, I, a" ){
      Fundos$Restricao_segmento[i]== 0.29
      
    }
    else if(Fundos$`Enquadramento Legal`[i]== "Art. 8º, II, a" ){
      Fundos$Restricao_segmento[i]== 0.19
      
    }
    else if(Fundos$`Enquadramento Legal`[i]== "Art. 8º, III" ){
      Fundos$`Limite do Enquadramento Legal`[i]== 0.09
      
    }
    else{
      Fundos$`Limite do Enquadramento Legal`[i]== 0.09
    }
  }  
  
  
  for (i in 1:length(Fundos$Restricao_segmento)) {
    
    if(Fundos$Restricao_segmento[i]== 1.00){
      Fundos$Restricao_segmento[i] = 1.00
      
    }  else{
      Fundos$Restricao_segmento[i]= Fundos$Restricao_segmento[i]-0.01
      
    }
  }
  
  
  return(Fundos)
  
  
  
}




otimizacao = function(my_dt,my_dt2)
{
  Desvio = 0
  dt= 0
  for (i in 1:length(my_dt)) {
    
    rentabilidademedia[i] = mean(my_dt[[i]]$rentabilidade)
    Desvio[i] = sd(my_dt[[i]]$rentabilidade) 
    dt= filter(my_dt[[i]],  my_dt[[i]]$DT_COMPTC == max(my_dt[[i]]$DT_COMPTC ) )
    my_data[i]= dt$VL_PATRIM_LIQ[1]
    
  }
  
  
  for (i in 1:length(my_dt))
    my_dt[[i]] = my_dt[[i]] %>% select(rentabilidade)
  
  teste = my_dt[[1]]
  for (j in 2:length(my_dt))
    teste = cbind(teste,my_dt[[j]])
  
  
  for (k in 1:length(my_dt2$`Nome do fundo`)) 
    names(teste)[k]<- unique(my_dt2$`Nome do fundo`)[k] 
  
  
  rentabilidade <- as.timeSeries(teste)
  #constraints <- c('minW[1:length(my_dt)]=0', 'maxW[1:length(my_dt)]=my_dt2$Restricao_segmento')
  g1 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 7º, I, b"))]=1')
  g2 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 7º, IV, a"))]=0.39')
  g3 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 8º, I, a"))]=0.29')
  g4 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 8º, II, a"))]=0.19')
  g5 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 8º, III"))]=0.09')
  g6 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 9º-A, III"))]=0.09')
  g7 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 7º, VII, b"))]=0.04')
  g8 = c('summaxW[c(which(my_dt2$`Enquadramento Legal` == "Art. 7º, III"))]=0.59')
  g9 = c('summaxW[c(which(my_dt2$Segmento == "Renda Fixa"))]=1')
  g10 = c('summaxW[c(which(my_dt2$Segmento == "Renda Variável"))]=0.29')
  g11 = c('summaxW[c(which(my_dt2$Segmento == "Exterior"))]=0.09')
  constraints <- c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11)
  portfolio.eficiente <- tangencyPortfolio(rentabilidade, spec = portfolioSpec(), constraints)
  Markowitz=0
  Markowitz = data.frame(Fundos=my_dt2$`Nome do fundo` ,Bench =my_dt2$Bench, Resgate =my_dt2$Resgate,Taxa_Administrativa =my_dt2$`Taxa Administrativa`, CNPJ = my_dt2$CNPJ_FUNDO,Grau_de_Risco =my_dt2$`Grau de Risco`,Enquadramento_legal = my_dt2$`Enquadramento Legal`,Limite_do_Enquadramento_Legal =my_dt2$`Limite do Enquadramento Legal`,Segmento =my_dt2$Segmento,Restricao_segmento =my_dt2$Restricao_segmento,Max_recursos = 0.14*my_data  , Rentabilidade_Media = rentabilidademedia, Desvio = Desvio, Recursosporcentagem =portfolio.eficiente@portfolio@portfolio$weights, Data_analise= my_dt2$Data_analise)
  Markowitz$Recursos = Markowitz$Max_recursos*Markowitz$Recursosporcentagem 
  #write.xlsx(Markowitz, "Markowitz.xlsx",overwrite = FALSE)
  return(Markowitz)
  
  
  
}


#a = portfolioConstraints(rentabilidade, spec= portfolioSpec(), constraints)

