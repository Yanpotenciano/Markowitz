#Donwload_CVM

cvm_link = "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_"

my_data=0
Anos= 0
Meses = 0


Anos[1] = format(mondate(Data_base, "%Y"))
Meses[1] = format(mondate(Data_base, "%m"))

for (i in 1:12) {
  
  Anos[i+1] = format(mondate(Data_base- months(i), "%Y"))
  Meses[i+1] = format(mondate(Data_base- months(i), "%m"))
}


myurl=list()

data_link = 
  
  for (i in 1:13) {
    myurl[[i]] = paste(cvm_link,Anos[i],Meses[i],".csv", sep="")
  }


myurl  
#X_CVM =
#X_CVM[i] <- read_delim(url[i], delim = ";", escape_double = FALSE, col_types = cols(DT_COMPTC = col_character()), trim_ws = TRUE)

Lista= list()
#Alternativamente pode ser usado o link abaixo
#read.csv("Z:/GERENCIA/INVESTIMENTOS/MARKOWITZ/202202-CVM.csv", sep=";", dec=",")

for (i in 1:13) {
  


Lista[[i]] = list (read_delim(myurl[[i]], delim = ";", escape_double = FALSE, col_types = cols(DT_COMPTC = col_character()), trim_ws = TRUE)

)

}


