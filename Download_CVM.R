#Donwload_CVM



#Anos[1] = format(mondate(Data_base, "%Y"))
#Meses[1] = format(mondate(Data_base, "%m"))

Download_CVM1 = function(Meses_calculados){

for (i in 1:(Meses_calculados-1)) {
  
  Anos[i+1] = format(mondate(Data_base- months(i), "%Y"))
  Meses[i+1] = format(mondate(Data_base- months(i), "%m"))
}
}


Download_CVM2 = function(Meses_calculados){


  for (i in 1:Meses_calculados) {
    myurl[[i]] = paste(cvm_link,Anos[i],Meses[i],".csv", sep="")
  }

}
myurl  
#X_CVM =
#X_CVM[i] <- read_delim(url[i], delim = ";", escape_double = FALSE, col_types = cols(DT_COMPTC = col_character()), trim_ws = TRUE)

#Lista= list()
#Alternativamente pode ser usado o link abaixo
#read.csv("Z:/GERENCIA/INVESTIMENTOS/MARKOWITZ/202202-CVM.csv", sep=";", dec=",")


Download_CVM3 = function(){
for (i in 1:13) {
  


Lista[[i]] = list (read_delim(myurl[[i]], delim = ";", escape_double = FALSE, col_types = cols(DT_COMPTC = col_character()), trim_ws = TRUE)

)

}

}
