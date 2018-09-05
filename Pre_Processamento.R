setwd("D:/Mestrado/Códigos/Dados")
transacoes = read.csv('Transferencias.csv',encoding='UTF-8',stringsAsFactors=FALSE,sep=";")

colnames(transacoes)[1]="Data"

transacoes$Data <- as.POSIXct(transacoes$Data,format="%Y-%m-%d %H:%M:%OS",tz=Sys.timezone())



transacoes$HoraDia<-as.factor(transacoes$HoraDia)
transacoes$Categoria<-as.factor(transacoes$Categoria)
transacoes$Transacao<-as.factor(transacoes$Transacao)
transacoes$Categoria <- 'T'
#transacoes$BancoDestino = as.integer(transacoes$BancoDestino)
#transacoes$AgenciaDestino = as.integer(transacoes$AgenciaDestino)
#transacoes$ContaDestino = as.integer(transacoes$ContaDestino)


num.col <- c("HoraDia","TipoConta","Categoria")
#transacoes<-cbind(transacoes,with(transacoes, model.matrix(~ Categoria + 0)),
                  #with(transacoes, model.matrix(~ HoraDia + 0)))
transacoes<-cbind(transacoes,with(transacoes, with(transacoes, model.matrix(~ HoraDia + 0))))
                  
transacoes$intHoraDia = factor(transacoes$HoraDia,
                               levels = c('Madrugada','Manh?','Noite','Tarde'),
                               labels = c(0, 1, 2, 3))
transacoes$intHoraDia = as.integer(as.character(transacoes$intHoraDia))
transacoes$intCategoria = factor(transacoes$Categoria,
                                 levels = c('E','P','T'),
                                 labels = c(0, 1, 2))
transacoes$intCategoria = as.integer(as.character(transacoes$intCategoria))

transacoes$FaixaValor <- cut(transacoes$Valor,
                             breaks = c(-Inf,26, 51, 101, 151, 201, 251, 501, 751, 1001,1501, 2001,5001,10001,20001,50001,Inf), 
                             labels = c("0-25", "26-50", "51-100", "101-150", "151-200", "201-250", "251-500","501-750","751-1000","1001-1500","1501-2000","2001-5000","5001-10000","10001-20000","20000-50000",">50000"), 
                             right = FALSE)
transacoes$intFaixaValor <- cut(transacoes$Valor,
                                breaks = c(-Inf,26, 51, 101, 151, 201, 251, 501, 751, 1001,1501, 2001,5001,10001,20001,50001,Inf), 
                                labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                                right = FALSE)
transacoes$intFaixaValor = as.integer(as.character(transacoes$intFaixaValor))
transacoes<-cbind(transacoes,with(transacoes, model.matrix(~ FaixaValor + 0)))
