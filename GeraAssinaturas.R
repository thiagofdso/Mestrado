load("D:/Mestrado/Códigos/Dados/dados.RData")

library(foreach)
library(doParallel)

# inicializa tabelas de destinatarios e assinaturas
destinatarios <- data.frame(Cliente=integer(),BancoDestino=integer(),AgenciaDestino=integer() ,ContaDestino=numeric(),Frequencia=integer())
assinaturas <- data.frame(Cliente=integer(),TipoConta=integer(),Ano=integer(),Mes=integer(),TotalTransferencias=integer(),ValorMaximo=numeric(),MediaValores=numeric(),DesvioValores=numeric(),TotalTransferenciasMadrugada=integer(),MediaValoresMadrugada=numeric(),DesvioValoresMadrugada=numeric(),NovosDestinos=integer(),MediaTransferenciaSessao=numeric(),QuantidadeRemetentes=integer())


for(ano in 2016:2017) {
  for(mes in 7:12){
    assinaturas <- data.frame(Cliente=integer(),TipoConta=integer(),Ano=integer(),Mes=integer(),TotalTransferencias=integer(),ValorMaximo=numeric(),MediaValores=numeric(),DesvioValores=numeric(),TotalTransferenciasMadrugada=integer(),MediaValoresMadrugada=numeric(),DesvioValoresMadrugada=numeric(),NovosDestinos=integer(),MediaTransferenciaSessao=numeric(),QuantidadeRemetentes=integer())
    dadosMes <- transacoes %>%
      filter(Ano == 2016 & Mes == 1)
    clientes <- unique(dadosMes$Cliente)
    destinatariosMes <- transacoes %>% 
      filter(Ano == ano & Mes == mes) %>% 
      group_by(Cliente,BancoDestino,AgenciaDestino,ContaDestino) %>% summarise(Frequencia=n()) %>% data.frame()
    for(c in clientes){
      dadosCliente <-transacoes %>%
        filter(Cliente == c & Ano == ano & Mes == mes)
      assinaturas <- rbind(assinaturas,criarAssinatura(dadosCliente,destinatariosMes))
    }
    destinatarios <- atualizarTabelaDestinos(destinatariosMes)
    saveRDS(assinaturas,file=paste0('D:/Mestrado/Códigos/Dados/assinaturas',mes+12,'.rds'))
    saveRDS(destinatarios,file=paste0('D:/Mestrado/Códigos/Dados/destinatarios',mes+12,'.rds'))
  }
}



# comb <- function(x, ...) {  
#   mapply(rbind,x,...,SIMPLIFY=FALSE)
# }

# assinaturas <- readRDS('D:/Mestrado/Códigos/Dados/assinaturas1.rds')
# destinatarios <- readRDS('D:/Mestrado/Códigos/Dados/destinatarios1.rds')
# 
# cores=detectCores()
# cl <- makeCluster(cores[1]-4) #not to overload your computer
# registerDoParallel(cl)
# ano=2016  
# mes=1
# destinatarios <- data.frame(Cliente=integer(),BancoDestino=integer(),AgenciaDestino=integer() ,ContaDestino=numeric(),Frequencia=integer())
# assinaturas <- data.frame(Cliente=integer(),TipoConta=integer(),Ano=integer(),Mes=integer(),TotalTransferencias=integer(),ValorMaximo=numeric(),MediaValores=numeric(),DesvioValores=numeric(),TotalTransferenciasMadrugada=integer(),MediaValoresMadrugada=numeric(),DesvioValoresMadrugada=numeric(),NovosDestinos=integer(),MediaTransferenciaSessao=numeric(),QuantidadeRemetentes=integer())
#   clientes <- transacoes %>% 
#     filter(Ano == ano & Mes == mes) %>%
#     pull(unique(Cliente))
# #,.init=list(assinaturas,destinatarios)  
#   assinaturas <- foreach(c = clientes, .combine=rbind, .packages="dplyr",.init=assinaturas) %dopar% {
#     dadosCliente <-transacoes %>% 
#       filter(Cliente == c & Ano == ano & Mes == mes)
#     assinaturastemp <- criarAssinatura(dadosCliente)
#     destinatarios <- atualizarTabelaDestinos(dadosCliente)
#     list(assinaturastemp,destinatarios)
#   }
#   saveRDS(assinaturas,file='assinaturas1.rds')
#   saveRDS(destinatarios,file='destinatarios1.rds')
#   # destinatarios <- foreach(c = clientes, .combine=rbind, .packages="dplyr") %dopar% {
#   #   dadosCliente <- transacoes %>% 
#   #     filter(Cliente == c & Ano == ano & Mes == mes)
#   #   destinos <- atualizarTabelaDestinos(dadosCliente)
#   #   destinos
#   # }
# 
# stopCluster(cl)
# 
# rm(list=setdiff(ls(), c("assinaturas","destinatarios","transacoes")))

# assinaturas <- readRDS('D:/Mestrado/Códigos/Dados/assinaturas1.rds')
# for(i in 2:24)
#   assinaturas <- rbind(assinaturas,readRDS(paste0('D:/Mestrado/Códigos/Dados/assinaturas',i,'.rds')))
# write.csv(assinaturas,file='D:/Mestrado/Códigos/Dados/assinaturas.csv',row.names = FALSE)
