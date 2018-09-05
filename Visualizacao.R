library("dplyr")
library(stringr)
library("lattice")
library("latticeExtra")
library("ggplot2")
load("D:/Mestrado/Códigos/Dados/Transferencias.RData")
dim(transacoes)
n_distinct(transacoes$Cliente)
clientes=intersect(transacoes$Cliente[transacoes$Ano==2016],transacoes$Cliente[transacoes$Ano==2017])
n_distinct(clientes)
transacoes %>% filter(Ano==2016) %>% summarise(n_distinct(Cliente))
transacoes %>% filter(Ano==2017) %>% summarise(n_distinct(Cliente))
transacoes %>% filter(Ano==2016) %>% dim()
transacoes %>% filter(Ano==2017) %>% dim()
head(transacoes$Data)
transacoes$Dia = as.integer(format(transacoes$Data,format = "%d"))

transacoes %>% filter() %>% barchart(~table(FaixaValor),data=.,col="darkblue",cex.names=0.8,ylab="Valores de Transferências",xlab="Quantidade de Transferências")

transacoes  %>% barchart(~table(HoraDia)|factor(flag_fraude),data=.,col="darkblue",cex.names=0.8,ylab="Valores de Transferências",xlab="Quantidade de Transferências")

transacoes %>% filter(flag_fraude==0) %>% barchart(~table(HoraDia),data=.,col="darkblue",cex.names=0.8,ylab="Valores de Transferências",xlab="Quantidade de Transferências")
transacoes %>% filter(flag_fraude==1) %>% barchart(~table(HoraDia),data=.,col="darkblue",cex.names=0.8,ylab="Valores de Transferências",xlab="Quantidade de Transferências",main='Fraudes')
xyplot(Valor~Mes|Ano,data = transacoes,subset = flag_fraude==1,type='o' )

barplot(table(transacoes$Transacao,transacoes$FaixaValor),
        col=c("darkblue", "red","yellow","orange","purple","green"),
        horiz=TRUE,
        las=2,
        cex.names=0.8,
        ylab="Valores de Transferências",
        xlab="Transações")
legend(x="top",
       legend=levels(transacoes$Transacao),fill = c("darkblue", "red","yellow","orange","purple","green","pink"))

par(mar=c(5.1,4.1,4.1,2.1),mgp=c(3,1,0))
par(mar=c(6.1,7.1,4.1,2.1),mgp=c(5,1,0))




transferenciaMes <- transacoes %>% group_by(Cliente,Ano,Mes) %>% 
 summarise(quantidade = sum(Categoria=='T')) %>% group_by(Ano,Mes) %>%
  summarise(Maximo=max(quantidade),Minimo=min(quantidade),Media=mean(quantidade),`Media Truncada 1%`=mean(quantidade, trim=0.01))
#colnames(transferenciaMes)[6]<-"Media Truncada 1%"
transferenciaPeriodo <- transacoes  %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(Transferencias = sum(Categoria=='T')) %>% group_by(Ano,Mes) 
transferenciaClientes <- transacoes %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(Transferencias = sum(Categoria=='T')) %>% group_by(Cliente) %>% summarise(MediaTransferencias=as.integer(mean(Transferencias)))
group_by(MediaTransferencias) %>% summarise(Clientes=n())

transferenciaValor <- transacoes %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(Transferencias = sum(Valor[Categoria=='T'])/n()) %>% group_by(Ano,Mes) %>% 
  summarise(Maximo=max(Transferencias),Minimo=min(Transferencias),Media=mean(Transferencias),`Media Truncada 1%`=mean(Transferencias, trim=0.01))
transferenciaValor2 <- transacoes %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(Transferencias = sum(Valor[Categoria=='T'])/n()) %>% group_by(Ano,Mes)
  

diasTransferencia <- transacoes %>% filter(Categoria=='T') %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(quantidade = n_distinct(Dia)) %>% group_by(Ano,Mes) %>%
summarise(Maximo=max(quantidade),Minimo=min(quantidade),Media=mean(quantidade),`Media Truncada 1%`=mean(quantidade, trim=0.01))
diasCliente <- transacoes %>% filter(Categoria=='T') %>% group_by(Cliente,Ano,Mes) %>% 
  summarise(`Dias com Transferência` = n_distinct(Dia))  %>% group_by(Ano,Mes) 

summary(transferenciaClientes$MediaTransferencias)
quantile(transferenciaPeriodo$Transferencias, probs = seq(0.99, 1, by= 0.001)) # decile
table(transferenciaClientes$MediaTransferencias)

quantile(transferenciaValor2$Transferencias, probs = seq(0.99, 1, by= 0.001)) # decile

transferenciaPeriodo %>% bwplot(Mes~Transferencias|factor(Ano), data=.,xlab="Quantidade mensal de Transferências",main="Quantidade mensal de Transferências x Mês X Ano")
transferenciaValor2 %>% bwplot(Mes~Transferencias|factor(Ano), data=.,xlab="Valor mensal de Transferências",main="Valor mensal de Transferências x Mês X Ano")

summary(diasCliente$`Dias com Transferência`)
quantile(diasCliente$`Dias com Transferência`, probs = seq(0.99, 1, by= 0.001)) # decile

transferenciaClientes  %>% barchart(~table(MediaTransferencias),data=.,xlab="Clientes",ylab="Transferências",main="Quantidade de Clientes x Quantidade Mensal de Transferências",col="darkblue")
diasCliente  %>% barchart(~table(`Dias com Transferência`),data=.,xlab="Clientes",ylab="Dias com Transferência",main="Quantidade Média de Dias com Transferências",col="darkblue")
quantile(diasTransferencia$Media, probs = seq(0.9, 1, by= 0.01)) # decile
#diasCliente %>%  xyplot(`Dias com Transferência`~Mes|Ano,data=.,ylab="Quantidade de dias",main="Dias com transferência X Mês X Ano",auto.key = list(columns = 2))

transferenciaMes %>% sunflowerplot(`Media Truncada 1%`~Mes,data=.,xlab="Mês",ylab="Valor Médio",main="Dias com transferência X Mês")
diasCliente %>% sunflowerplot(`Dias com Transferência`~Mes,data=.,xlab="Mês",ylab="Quantidade de dias",main="Dias com transferência X Mês")
sunflowerplot(transferenciaPeriodo$Mes,transferenciaPeriodo$Transferencias,xlab="Mês",ylab="Quantidade Mensal de Transferências",main="Quantidade de Clientes x Quantidade Mensal de Transferências")

# GRAFICOS DE LINHA
transferenciaMes %>%  xyplot(`Media Truncada 1%`~Mes,data=.,type=c("l","p"),groups = Ano,ylab="Quantidade mensal de Transferências",main="Quantidade mensal de Transferências X Mês X Ano",auto.key = list(columns = 2))
diasTransferencia %>%  xyplot(`Media Truncada 1%`~Mes,data=.,type=c("l","p"),groups = Ano,ylab="Quantidade de dias",main="Dias com transferência X Mês X Ano",auto.key = list(columns = 2))
transferenciaValor %>%  xyplot(`Media Truncada 1%`~Mes,data=.,type=c("l","p"),groups = Ano,ylab="Valor Médio de Transferências",main="Valor mensal de Transferências X Mês X Ano",auto.key = list(columns = 2))

transacoes %>% filter(Categoria=='T'&Valor>10000&TipoConta==0) %>% select(Cliente,Transacao,Valor,Data)



#transferenciaMes%>% filter(Ano==2017)%>%
#ggplot(., aes(x = Mes, y = Media)) +
#  geom_point(size = 4) +
# geom_errorbar(aes(ymax = Maximo, ymin = Minimo))

plot(transferenciaMes$Mes[transferenciaMes$Ano==2017],transferenciaMes$Media[transferenciaMes$Ano==2017], type="l",col="blue")
lines(transferenciaMes$Mes[transferenciaMes$Ano==2017],transferenciaMes$Maximo[transferenciaMes$Ano==2017],col="red",lty = 'dashed')
lines(transferenciaMes$Mes[transferenciaMes$Ano==2017],transferenciaMes$Media[transferenciaMes$Ano==2017],col="blue")
lines(transferenciaMes$Mes[transferenciaMes$Ano==2016],transferenciaMes$Media[transferenciaMes$Ano==2016],col="blue")
transferenciaMes%>% filter(Ano==2017) %>%  with(plot(Mes,Media, type="l",col="blue"))
transferenciaMes%>% filter(Ano==2016) %>%  with(lines(Mes,Media,col="red"))
transferenciaMes%>% filter(Ano==2017) %>%  with(points(Mes,Maximo,col="blue"))
transferenciaMes%>% filter(Ano==2016) %>%  with(points(Mes,Maximo,col="red"))
transferenciaMes%>% filter(Ano==2017) %>%  with(points(Mes,Minimo,col="blue"))
transferenciaMes%>% filter(Ano==2016) %>%  with(points(Mes,Minimo,col="red"))

transacoes %>% summarise(min(Idade),max(Idade),sum(Ano==2016),sum(Ano==2017))
histogram(~factor(Ano),
          col = "#bdbdbd",
          data=transacoes, 
          xlab = "",
          ylab = "",
          main = "Transações por Ano",
          labels = TRUE,
          type="count",
          scales = list(tck = c(0,0), y = list(draw = FALSE)),
          par.settings = list(axis.line = list(col = 0)))
indexEmprestimo <- which(transacoes$Categoria == "E")
indexTransferencia <- which(transacoes$Categoria == "T")
# Transacoes por Ano
hist(transacoes$Ano,labels = TRUE,axes=FALSE,xlab="",ylab="",main="Transações por Ano",breaks = c(2015.0,2016.0,2017.0),col=c("darkblue","red"),w=3)
axis(1, at=c(2015.5,2016.5), labels=c("2016","2017"),tick = FALSE)
legend(x="topleft",legend=c(2016,2017),fill = c("darkblue", "red"))

transacoes %>% group_by(Ano) %>% 
  summarise(Pagamentos=sum(Categoria=='P'),
            Emprestimo=sum(Categoria=='E'),
            Transferencia=sum(Categoria=='T'))%>%
            select(Pagamentos,Emprestimo,Transferencia)%>% t() %>% 
          barplot(col=c("darkblue","yellow","red"),names.arg=c('2016','2017'),main="Transações por Ano")
legend(x="topleft",legend=c("P","E","T"),fill = c("darkblue","yellow","red"))


cliente_transacaco <- as_data_frame(matrix(c(2016,2017,
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='P'&transacoes$Ano==2016])),
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='P'&transacoes$Ano==2017])),
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='E'&transacoes$Ano==2016])),
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='E'&transacoes$Ano==2017])),
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='T'&transacoes$Ano==2016])),
n_distinct(as.character(transacoes$Cliente[transacoes$Categoria=='T'&transacoes$Ano==2017]))),byrow = FALSE,nrow =2))
colnames(cliente_transacaco) <- c('Ano','Pagamento','Empréstimo','Transferência')
cliente_transacaco[,2:4] %>% t() %>% 
  barplot(col=c("darkblue","yellow", "red"),names.arg=c('2016','2017'),main="Clientes por Tipo de Transação")
legend(x="topleft",legend=c("P","E","T"),fill = c("darkblue","yellow", "red"))

barchart(Ano~Categoria,data = transacoes)

barplot(transacoes$Ano)
colnames(transacoes)
histogram(~Transacao,data = transacoes,subset = transacoes$flag_fraude==1, horizontal=TRUE)
perfilFraude=transacoes$flag_fraude==1 & transacoes$Transacao!='Pagamento de Boleto de Consumo'& transacoes$Transacao!='Pagamento de GPS'&transacoes$Transacao!='Compra de Credito Celular'
clientes <- transacoes$Cliente[perfilFraude]

barchart(as.character(transacoes$Transacao[perfilFraude]))
barchart(as.character(transacoes$Categoria[perfilFraude]))
barchart(as.character(transacoes$FaixaValor[perfilFraude])|transacoes$Categoria[perfilFraude])

par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(5.1,7,4.1,2.1))
sum(perfilFraude)-2
barplot(table(transacoes$Categoria[perfilFraude],transacoes$FaixaValor[perfilFraude])[,c(2:13)],
        col=c("darkblue","red","yellow"),
        horiz=TRUE,
        las=2,
        cex.names=0.8,
        main="Valores de Fraudes",
        xlab="Transações")
legend(x="bottomright",legend=c("E","P","T"),fill = c("darkblue", "red","yellow"))

barplot(table(transacoes$Transacao[perfilFraude],transacoes$FaixaValor[perfilFraude])[c(4,12,14,15,17),c(2:13)],
        horiz=TRUE,
        col=c("darkblue","red","yellow","black","white"),
        las=2,
        cex.names=0.8,
        main="Valores de Fraudes",
        xlab="Transações")
legend(x="bottomright",legend=c("Pagamento de Boleto de Cobrança","RDC - Confirmacao do Empréstimo Parcelado","Transferência de Conta Corrente para Conta Corrente","Transferência de Conta Corrente para Poupança","Transferência de DOC E - Diferente Titularidade"),fill = c("darkblue", "red","yellow","black","white"))

barchart(transacoes$Transacao[perfilFraude]~transacoes$FaixaValor[perfilFraude],col=c('green','blue','yellow'),groups=transacoes$Categoria[perfilFraude] ,auto.key = list(columns = 3))
#barchart(transacoes$HoraDia[perfilFraude])
#barchart(factor(transacoes$nm_bai[perfilFraude]))
#xtabs(~Transacao,data = transacoes,subset = transacoes$flag_fraude==1)
#teste<-cbind(with(transacoes, model.matrix(~ FaixaEtaria:Sexo + 0)))
#head(teste)
#Analfabeto
dd=densityplot(~ intFaixaValor|factor(FaixaEtaria):factor(EstCivil) ,groups=factor(Sexo):factor(Categoria), data = transacoes, 
               plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa Etária X Estado Civil X Analfabeto",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='A', par.settings = list(superpose.line = list(lwd=3)))
print(dd)
#Fundamental
dd=densityplot(~ intFaixaValor|factor(FaixaEtaria):factor(EstCivil) ,groups=factor(Sexo):factor(Categoria), data = transacoes, 
               plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa Etária X Estado Civil X Fundamental",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='F', par.settings = list(superpose.line = list(lwd=3)))
print(dd[c(2,18,19,25)]) 
#Medio
dd=densityplot(~ intFaixaValor|factor(FaixaEtaria):factor(EstCivil) ,groups=factor(Sexo):factor(Categoria), data = transacoes, 
               plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa Etária X Estado Civil X Medio",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='M', par.settings = list(superpose.line = list(lwd=3)))
print(dd[c(2,5,7,18,22)])
#Incompleto
dd=densityplot(~ intFaixaValor|factor(FaixaEtaria):factor(EstCivil) ,groups=factor(Sexo):factor(Categoria), data = transacoes, 
               plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa Etária X Estado Civil X Incompleto",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='I', par.settings = list(superpose.line = list(lwd=3)))
print(dd[24])
#Superior
dd=densityplot(~ intFaixaValor|factor(FaixaEtaria):factor(EstCivil) ,groups=factor(Sexo):factor(Categoria), data = transacoes, 
               plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa Etária X Estado Civil X Superior",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='C', par.settings = list(superpose.line = list(lwd=3)))
print(dd[11,28,31,33])

#xyplot(Cliente~Data|factor(FaixaValor),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='A',auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#xyplot(Valor~Data|factor(FaixaEtaria):factor(EstCivil),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='A',main = "Valor X Faixa EtÃ¡ria X Estado Civil X Analfabeto",auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#xy=xyplot(Valor~Data|factor(FaixaEtaria):factor(EstCivil),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='F',main = "Valor X Faixa EtÃ¡ria X Estado Civil X Fundamental",auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#print(xy[1:6])
#print(xy[7:12])
#xy=xyplot(Valor~Data|factor(FaixaEtaria):factor(EstCivil),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='M',main = "Valor X Faixa EtÃ¡ria X Estado Civil X MÃ©dio",auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#xy=xyplot(Valor~Data|factor(FaixaEtaria):factor(EstCivil),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='I',main = "Valor X Faixa EtÃ¡ria X Estado Civil X Sup. Incompleto",auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#xy=xyplot(intFaixaValor~Data|factor(FaixaEtaria):factor(EstCivil),groups=factor(Categoria),data=transacoes,subset=Escolaridade=='C',main = "Valor X Faixa EtÃ¡ria X Estado Civil X Superior",auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#xyplot(vr_sal_men~FaixaValor|Mes,groups=factor(Categoria),data=transacoes,auto.key = list(columns = 3),par.settings = list(superpose.symbol = list(pch = 19, cex = 1)))
#densityplot(~ vr_sal_men, data = transacoes)

dpAnalfabeto=densityplot(~ intFaixaValor|factor(FaixaEtaria)+factor(EstCivil) ,groups=Ano, data = transacoes, 
                         plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa EtÃ¡ria X Estado Civil X Analfabeto",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='A')
dpFundamental=densityplot(~ intFaixaValor|factor(FaixaEtaria)+factor(EstCivil) ,groups=Ano, data = transacoes, 
                          plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa EtÃ¡ria X Estado Civil X Fundamental",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='F')
dpMedio=densityplot(~ intFaixaValor|factor(FaixaEtaria)+factor(EstCivil) ,groups=Ano, data = transacoes, 
                    plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa EtÃ¡ria X Estado Civil X MÃ©dio",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='M')
dpIncompleto=densityplot(~ intFaixaValor|factor(FaixaEtaria)+factor(EstCivil) ,groups=Ano, data = transacoes, 
                         plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa EtÃ¡ria X Estado Civil X Sup. Incompleto",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='I')
dpSuperior=densityplot(~ intFaixaValor|factor(FaixaEtaria)+factor(EstCivil) ,groups=Ano, data = transacoes, 
                       plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X Faixa EtÃ¡ria X Estado Civil X Superior",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='C')

dpFundamental2=densityplot(~ intFaixaValor|factor(Categoria) ,groups=Ano, data = transacoes, 
                           plot.points = FALSE, ref = TRUE,auto.key = list(columns = 2),main = "Valor X 80-99 anos X Casado Parcial X Fundamental",xlab = "Faixa de Valor",ylab="Densidade",subset = Escolaridade=='F'&EstCivil=='CP'&FaixaEtaria=='80-99 anos')


print(dpAnalfabeto)
print(dpFundamental[,1:3])
#print(dpFundamental[,4:6])
#print(dpFundamental[,7:8])
#dim(dpMedio)
#print(dpMedio[,1:3])
#print(dpMedio[,4:6])
print(dpMedio[,7:8])
#print(dpIncompleto[,1:3])
#print(dpIncompleto[,4:6])
print(dpIncompleto[,7:9])
#print(dpSuperior[,1:3])
print(dpSuperior[,4:6])
#print(dpSuperior[,7:9])
print(dpFundamental2)

spAnalfabeto=xyplot(Valor~ Data|factor(FaixaEtaria)+factor(EstCivil) ,groups=Categoria, data = transacoes, ref = TRUE,auto.key = list(columns = 2),main = "66-79 anos X ViÃºvo X Analfabeto",xlab = "Data",ylab="Valor",subset = Escolaridade=='A'&EstCivil=='VI'&FaixaEtaria=='66-79 anos')
spFundamental=xyplot(Valor~ Data|factor(FaixaEtaria)+factor(EstCivil) ,groups=Categoria, data = transacoes, ref = TRUE,auto.key = list(columns = 2),main = "80-99 anos X Casado Parcial X Fundamental",xlab = "Data",ylab="Valor",subset = Escolaridade=='F'&EstCivil=='CP'&FaixaEtaria=='80-99 anos')
spMedio=xyplot(Valor~ Data|factor(FaixaEtaria)+factor(EstCivil) ,groups=Categoria, data = transacoes, ref = TRUE,auto.key = list(columns = 2),main = "18-29 anos X ViÃºvo X MÃ©dio",xlab = "Data",ylab="Valor",subset = Escolaridade=='M'&EstCivil=='VI'&FaixaEtaria=='18-29 anos')
spIncompleto=xyplot(Valor~ Data|factor(FaixaEtaria)+factor(EstCivil) ,groups=Categoria, data = transacoes, ref = TRUE,auto.key = list(columns = 2),main = "80-99 anos X ViÃºvo X Sup. Incompleto",xlab = "Data",ylab="Valor",subset = Escolaridade=='I'&EstCivil=='VI'&FaixaEtaria=='80-99 anos')
spSuperior=xyplot(Valor~ Data|factor(FaixaEtaria)+factor(EstCivil) ,groups=Categoria, data = transacoes, ref = TRUE,auto.key = list(columns = 2),main = "18-29 anos X Separado X Superior",xlab = "Data",ylab="Valor",subset = Escolaridade=='C'&EstCivil=='SE'&FaixaEtaria=='18-29 anos')

print(spAnalfabeto)
print(spFundamental)
print(spMedio)
print(spIncompleto)
print(spSuperior)

num.col <- c("Ano","Mes","Valor","Idade","intFaixaEtaria")
transacoesScaled=scale(transacoes[,num.col])
