


# funcao para atualizar destinatarios
atualizarTabelaDestinos <- function(dados){
 destinatarios <- rbind(destinatarios,dados) %>% group_by(Cliente,BancoDestino,AgenciaDestino,ContaDestino) %>% summarise(Frequencia=sum(Frequencia)) %>% data.frame()
 destinatarios
}

# funcao para contar novos destinatarios
novosDestinos <- function(dados){
  dados %>% 
  anti_join(destinatarios,by=c('Cliente','BancoDestino','AgenciaDestino','ContaDestino')) %>%
  distinct(BancoDestino,AgenciaDestino,ContaDestino) %>%
  summarise(n()) %>% pull()
}

clientesDistintos <- function(cliente,destinatariosMes){
  inner_join(destinatariosMes %>% filter(Cliente == cliente) %>% 
               select(BancoDestino,AgenciaDestino,ContaDestino),
             destinatariosMes,
             by=c(c("BancoDestino", "AgenciaDestino", "ContaDestino"))) %>% 
    summarize(Quantidade=n_distinct(Cliente))%>% pull(Quantidade)
}

# funcao para criar assinatura

criarAssinatura <- function(dados,destinatariosMes){
  assinatura <- 
    dados %>% 
    left_join(destinatarios,by=c('Cliente','BancoDestino','AgenciaDestino','ContaDestino')) %>%
    group_by(Cliente,TipoConta,Ano,Mes) %>%
    summarise(TotalTransferencias = sum(Categoria=='T'),
              ValorMaximo = max(Valor[Categoria=='T']),
              MediaValores = mean(Valor[Categoria=='T']),
              DesvioValores = sd(Valor[Categoria=='T']),
              TotalTransferenciasMadrugada = sum(Categoria=='T'&HoraDiaMadrugada==1),
              MediaValoresMadrugada = mean(Valor[Categoria=='T'&HoraDiaMadrugada==1]),
              DesvioValoresMadrugada = sd(Valor[Categoria=='T'&HoraDiaMadrugada==1]),
              NovosDestinos = novosDestinos(dados),
              MediaTransferenciaSessao = sum(Categoria=='T')/n_distinct(Sessao[Categoria=='T']),
              QuantidadeRemetentes = clientesDistintos(dados$Cliente[1],destinatariosMes)) %>%
    as.data.frame()
  
  # tratamento para valores NA
  assinatura$MediaValores = ifelse(is.na(assinatura$MediaValores),0,assinatura$MediaValores)
  assinatura$DesvioValores = ifelse(is.na(assinatura$DesvioValores),0,assinatura$DesvioValores)
  assinatura$MediaValoresMadrugada = ifelse(is.na(assinatura$MediaValoresMadrugada),0,assinatura$MediaValoresMadrugada)
  assinatura$DesvioValoresMadrugada = ifelse(is.na(assinatura$DesvioValoresMadrugada),0,assinatura$DesvioValoresMadrugada)
  assinatura
}
calcAmplitudeVariavel <- function(v){
  amp = max(v)-min(v)
}
calcAmplitude <- function(dadosAtualizados,dados,assinatura1,assinatura2){
  amp <- dadosAtualizados %>% 
#    filter((Ano==assinatura1$Ano&Mes==assinatura1$Mes)|(Ano==assinatura2$Ano&Mes==assinatura2$Mes))%>%
    filter(Ano==assinatura1$Ano&Mes==assinatura1$Mes) %>%
    rbind(dados %>%filter(Ano==assinatura2$Ano&Mes==assinatura2$Mes)) %>%
    summarise_at(c("TotalTransferencias","ValorMaximo","MediaValores",
                   "TotalTransferenciasMadrugada","MediaValoresMadrugada",
                   "NovosDestinos","MediaTransferenciaSessao","QuantidadeRemetentes"),calcAmplitudeVariavel)
  amp
}
calcAmplitudeJanela <- function(dados){
  amp <- dados %>% 
    summarise_at(c("TotalTransferencias","ValorMaximo","MediaValores",
                   "TotalTransferenciasMadrugada","MediaValoresMadrugada",
                   "NovosDestinos","MediaTransferenciaSessao","QuantidadeRemetentes"),calcAmplitudeVariavel)
}
distanciaSimples <- function(v1,v2,B,amp){
  if(amp==0)
    0
  else
    exp(-(((v1-v2)*B)/amp))   
}
modulo_intersessao <- function(I1,I2){
  if ( I2[1]>I1[2] | I1[1]>I2[2] ) { 
    0
  }
  else {
    os=max(I1[1],I2[1])
    oe=min(I1[2],I2[2])
    oe-os
  }
}
modulo_uniao <- function(I1,I2){
  os=min(I1[1],I2[1])
  oe=max(I1[2],I2[2])
  oe-os
}

distanciaComplexa <- function(M1,d1,M2,d2,B,amp){
  mi <- modulo_intersessao(c(M1-d1,M1+d1),c(M2-d2,M2+d2))
  ratio <- ifelse(mi==0,0,mi/modulo_uniao(c(M1-d1,M1+d1),c(M2-d2,M2+d2)))
  distanciaSimples(M1,M2,B,amp)*ratio
}

distanciaBase  <- function(assinatura1,assinatura2,B,amp){
  distancias <- data.frame(Cliente1 = assinatura1$Cliente,
                           TipoConta = assinatura1$TipoConta,
                           Ano1 = assinatura1$Ano,
                           Mes1 = assinatura1$Mes,
                           Cliente2 = assinatura2$Cliente,
                           Ano2 = assinatura2$Ano,
                           Mes2 = assinatura2$Mes,
                           distanciaTotalTransferencias =
                             distanciaSimples(assinatura1$TotalTransferencias,
                                              assinatura2$TotalTransferencias,
                                              B,amp$TotalTransferencias)^2,
                           distanciaValorMaximo =
                             distanciaSimples(assinatura1$ValorMaximo,
                                              assinatura2$ValorMaximo,
                                              B,amp$ValorMaximo)^2,
                           distanciaValores =
                             distanciaComplexa(assinatura1$MediaValores,
                                               assinatura1$DesvioValores,
                                               assinatura2$MediaValores,
                                               assinatura2$DesvioValores,
                                               B,amp$MediaValores)^2,
                           distancaiTotalTransferenciasMadrugada = 
                           distanciaSimples(assinatura1$TotalTransferenciasMadrugada,
                                             assinatura2$TotalTransferenciasMadrugada,
                                             B,amp$TotalTransferenciasMadrugada)^2,
                           distanciaValoresMadrugada = 
                             distanciaComplexa(assinatura1$MediaValoresMadrugada,
                                               assinatura1$DesvioValoresMadrugada,
                                               assinatura2$MediaValoresMadrugada,
                                               assinatura2$DesvioValoresMadrugada,
                                               B,amp$MediaValoresMadrugada)^2,
                           distanciaNovosDestinos = 
                             distanciaSimples(assinatura1$NovosDestinos,
                                              assinatura2$NovosDestinos,
                                              B,amp$NovosDestinos)^2,
                           distanciaMediaTransferenciaSessao = 
                             distanciaSimples(assinatura1$MediaTransferenciaSessao,
                                              assinatura2$MediaTransferenciaSessao,
                                              B,amp$MediaTransferenciaSessao)^2,
                           distanciaQuantidadeRemetentes = 
                             distanciaSimples(assinatura1$QuantidadeRemetentes,
                                              assinatura2$QuantidadeRemetentes,
                                              B,amp$QuantidadeRemetentes)^2
  )
  distancias
}


# distanciaAssinaturas  <- function(assinatura1,assinatura2,B,pesos){
#   distancias <- data.frame(Cliente = assinatura1$Cliente,
#                            TipoConta = assinatura1$TipoConta,
#                            Ano1 = assinatura1$Ano,
#                            Mes1 = assinatura1$Mes,
#                            Ano2 = assinatura2$Ano,
#                            Mes2 = assinatura2$Mes,
#     distanciaTotalTransferencias =
#     pesos$pesoTotalTransferencias*
#       distanciaSimples(assinatura1$TotalTransferencias,
#                        assinatura2$TotalTransferencias,
#                        B)^2,
#     distanciaValorMaximo =
#     pesos$pesoValorMaximo*
#       distanciaSimples(assinatura1$ValorMaximo,
#                        assinatura2$ValorMaximo,
#                        B)^2,
#     distanciaValores =
#     pesos$pesoValores*
#       distanciaComplexa(assinatura1$MediaValores,
#                         assinatura1$DesvioValores,
#                         assinatura2$MediaValores,
#                         assinatura2$DesvioValores,
#                         B)^2,
#     distancaiTotalTransferenciasMadrugada = 
#     pesos$pesoTotalTransferenciasMadrugada
#     *distanciaSimples(assinatura1$TotalTransferenciasMadrugada,
#                       assinatura2$TotalTransferenciasMadrugada,
#                       B)^2,
#     distanciaValoresMadrugada = 
#     pesos$pesoValoresMadrugada*
#       distanciaComplexa(assinatura1$MediaValoresMadrugada,
#                         assinatura1$DesvioValoresMadrugada,
#                         assinatura2$MediaValoresMadrugada,
#                         assinatura2$DesvioValoresMadrugada,
#                         B)^2,
#     distanciaNovosDestinos = 
#     pesos$pesoNovosDestinos*
#       distanciaSimples(assinatura1$NovosDestinos,
#                        assinatura2$NovosDestinos,
#                        B)^2,
#     distanciaMediaTransferenciaSessao = 
#     pesos$pesoMediaTransferenciaSessao*
#       distanciaSimples(assinatura1$MediaTransferenciaSessao,
#                        assinatura2$MediaTransferenciaSessao,
#                        B)^2,
#     distanciaQuantidadeRemetentes = 
#     pesos$pesoQuantidadeRemetentes*
#       distanciaSimples(assinatura1$QuantidadeRemetentes,
#                        assinatura2$QuantidadeRemetentes,
#                        B)^2
#     )
#   distancias$distanciaTotal <- sqrt(sum(distancias[c(7:14)]))
#   distancias
# }

atualizaAssinatura <- function(assinatura1,assinatura2,beta){
  assinatura2[5:14] = assinatura1[5:14]*beta + (1-beta)*assinatura2[5:14]
  assinatura2
}
#########################################
#                 TESTES                #
#########################################

# destinatarios <- data.frame(Cliente=integer(),BancoDestino=integer(),AgenciaDestino=integer() ,ContaDestino=numeric(),Frequencia=integer())
# dadosTeste=transacoes %>% filter(Cliente==28036&Ano==2016&Mes==6)
# assinatura1 = criarAssinatura(dadosTeste,destinatarios)
# destinatarios <- atualizarTabelaDestinos(dadosTeste)
# 
# dadosTeste <- transacoes %>% filter(Cliente==28036&Ano==2016&Mes==10)
# assinatura2 = criarAssinatura(dadosTeste,destinatarios)
# destinatarios <- atualizarTabelaDestinos(dadosTeste)
# 
# pesos = data.frame(pesoTotalTransferencias= 0.1,pesoValorMaximo= 0.1,pesoValores= 0.1,pesotalTransferenciasMadrugada= 0.1,pesoValoresMadrugada= 0.1,pesoNovosDestinos= 0.1,pesoMediaTransferenciaSessao= 0.1,pesoQuantidadeRemetentes = 0.1, B = 0.1, beta = 0.6)
#d=distanciaAssinaturas(assinaturas[1,],assinaturas[2,],pesos$B,pesos[1:8])
# 
# 
#assinatura1
#assinatura2
# assinatura = atualizaAssinatura(assinatura1,assinatura2,pesos$beta)
