# Obter Fundos Cadastrados 


Fronteira_Markowitz_v14 <- read_excel("Z:/GERENCIA/INVESTIMENTOS/MARKOWITZ/Fronteira_Markowitz_v14.xlsm", sheet = "CADASTRO")


Fundos = Fronteira_Markowitz_v14 %>% 
  select(`Nome do fundo`,CNPJ)

Fundos =rename.vars(Fundos, from= "CNPJ", to = "CNPJ_FUNDO")

# obtendo apenas as datas finais de cada mes para os 13 meses

#for (i in 1:13) {
# Lista[[i]] = filter(Lista[[i]],  Lista[[i]]$DT_COMPTC == max(Lista[[i]]$DT_COMPTC ) )
#  
#}


