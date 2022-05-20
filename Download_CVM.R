#Download_CVM

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



