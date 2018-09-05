#assinaturas <- read.csv('D:/Mestrado/Códigos/Dados/assinaturas.csv')
library("dplyr")
load("D:/Mestrado/Códigos/Dados/assinaturasInicial.RData")
clientes <- (assinaturas %>% group_by(Cliente) %>% summarize(Quantia=n())%>% filter(Quantia>1))$Cliente
setwd("D:/Mestrado/Códigos/Dados")

for(beta in 1:9/10){
  print(paste0('beta=',beta))
  assinaturasAtualizadas <- data.frame(Cliente=integer(),TipoConta=integer(),Ano=integer(),Mes=integer(),TotalTransferencias=integer(),ValorMaximo=numeric(),MediaValores=numeric(),DesvioValores=numeric(),TotalTransferenciasMadrugada=integer(),MediaValoresMadrugada=numeric(),DesvioValoresMadrugada=numeric(),NovosDestinos=integer(),MediaTransferenciaSessao=numeric(),QuantidadeRemetentes=integer())
  for(c in clientes){
    assinaturasCliente = assinaturas %>% filter(Cliente==c)
    assinaturaAtual <- assinaturasCliente[1,]
    assinaturasAtualizadas = rbind(assinaturasAtualizadas,assinaturaAtual)
    for(j in 2:nrow(assinaturasCliente)){
        assinaturaAtual <-atualizaAssinatura(assinaturaAtual,
                                              assinaturasCliente[j,],
                                              beta)
        assinaturasAtualizadas = rbind(assinaturasAtualizadas,assinaturaAtual)
    }
  }
  write.csv(assinaturasAtualizadas,file=paste0('assinaturasAtualizadas_beta',beta,'.csv'),row.names = FALSE)
}