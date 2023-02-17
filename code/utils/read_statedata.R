#READ state_data
#' Questa funzione restituisce un dataframe che contiene tutte le trittiche
#' (anno - trimestre - stato) per gli US con attributi (temperatura, pioggia, 
#' inquinamento, ecc..) salvati in csv nelle cartelle per ogni stato 
#' (si possono salvare quanti attributi si vogliono
#' per ogni stato, l'importante è che siano gli stessi in ogni stato (cartella)!).
#' In particolare, per il formato del csv scarcato si compie la media nel trimestre
#' per ogni attributo - e se nella colonna è presente il termine "max" o "min"
#' compie le rispettive operazioni.

read_statedata <- function(ita_convert = T) {

  #' per ogni cartella che trovo in state_data, corrispondente ad ogni stato, devo
  #' listare tutti i csv presenti che corrispondono a una colonna del "nuovo" dataset.

  # per come sono fatti i nostri csv importati devo skippare le prime 4 linee
  # myfiles = lapply(filenames, read.delim, skip=4)
  
  #nota: il [-1] serve per escludere la prima directory state/data "vuota"
  state <- sub("data/state_data/","",list.dirs("data/state_data") [-1]) 
  number <- length(state)
  cols_name <- c("year","months")
  result <- c()
  #namedir <- "data/state_data/Alabama"
  for (j in 1:number) 
  {
    # per ogni stato estraggo il nome della directory
    namedir <- paste("data/state_data",state[j],sep="/")
    filenames <- list.files(path=namedir, pattern="\\.csv$",full.names = T)
    myfiles = lapply(filenames, read.csv, skip=4) #i csv hanno le prime 4 righe di intestazione
    #_ magari da METTERE CHECK SU STATO j == intestazione file ->fatto
    intest <- lapply(filenames, read.csv,nrows=1, header=F)
    
    
    col <- length(filenames)
    #estrarre info file
    parzial_result <- c()
    for(i in 1:col)
    {
      #n <- sub(".csv","",sub(paste(namedir,"/",sep=''),"",filenames[i]))
      if(intest[[i]]$V1 != state[j])
        warning(paste("IMPORTANTE: Lo Stato del csv non coincide con lo Stato della cartella:",state[j],filenames[i],sep= " "))
      n <- gsub("[[:space:]]|\\(|\\)", "", intest[[i]]$V2) #gsub rimuove whitespaces oppure parentesi
      if(j==1)
        cols_name <- c(cols_name,n)
      else if(!(n %in% cols_name))
        warning("Errore colonne, stato state[j]")
      
      if(intest[[i]]$V2 != cols_name[2+i])
        warning(paste("IMPORTANTE: Errore intestazione Attributo:", state[j], sep= " "))
      
   
      #devo processare ogni riga per renderla anno - trimestre - valore
      a <- myfiles[[i]]
      
      a <- cbind(a, year=substr(a$Date,1,4), months=substr(a$Date,5,6))
      b <- strtoi(a$month, base = 10L)
      j_m <- 1<=b & b<=3
      a_j <- 4<=b & b<=6
      j_s <- 7 <= b & b <= 9
      o_d <- 10 <= b & b <= 12
      b[j_m] <- "January-March"
      b[a_j] <- "April-June"
      b[j_s] <- "July-September"
      b[o_d] <- "October-December"
      a$months <- b
      a$months <- factor(a$months, levels = c("January-March",
                                              "April-June",
                                              "July-September",
                                              "October-December"))
      
      # fine preprocessing, posso mediare (o onsderare min e max) per ogni trimestre
      if(grepl("max", tolower(n)))
      {
        a <- a %>%
          group_by(year,months) %>%
          summarise(max(Value))
      } else if(grepl("min", tolower(n)))
      {
        a <- a %>%
          group_by(year,months) %>%
          summarise(min(Value))
      } else
      {
        a <- a %>%
          group_by(year,months) %>%
          summarise(mean(Value))
      }
      
      
      if(dim(a)[1]!=30)
        warning(paste(state[j], n,"has more row than expected: ", dim(a)[1], sep=" "))
      #da cambiare quel 1:30 perchè nei primi data_file abbiamo più dati
      if(i==1)
        parzial_result <- cbind(parzial_result,a[1:30,])
      else
        parzial_result <- cbind(parzial_result, a[1:30,3])
    }
    
    colnames(parzial_result) <- cols_name
    #aggiungo colonna relativa a stato
    parzial_result <- cbind(parzial_result, state = rep(state[j],dim(parzial_result)[1]))
    result <- rbind(result,parzial_result)
  
    
  }
  
  #conversione temperature se flagita = T
  col_to_convert <- which(grepl("temp", tolower(cols_name)))
  result[,col_to_convert] <- fahrenheit.to.celsius(result[,col_to_convert], round = 2)
  
  return(result)
}
