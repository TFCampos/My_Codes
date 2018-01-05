##############################################
##################Preambulo###################
##############################################


rm(list=ls())
gc()
###############################################
############Preparação dos pacotes#############
###############################################
# Selecioanar uma CRAN mirror
 local({r <- getOption("repos")
       r["CRAN"] <- "http://camoruco.ing.uc.edu.ve/cran/"
       options(repos=r)})
#Função que carrega e requere os pacotes em silêncio 
Install_And_Load <- function(Required_Packages)
{
    Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];

    if(length(Remaining_Packages)) 
    {
        install.packages(Remaining_Packages, dependencies = TRUE);
    }

    for(package_name in Required_Packages)
    {
        library(package_name,character.only=TRUE,quietly=TRUE);

    }
}
Required_Packages =c("ggplot2","dplyr","xtable",'MicrosoftML','gridExtra','rmarkdown','tree','gclus');

Install_And_Load(Required_Packages)

options(scipen = 9999)
#######################
####Leitura de dados###
#######################
dados<-read.table('D:/Disco\ C/Documentos/Trabalho\ de\ Geologia\ forence/Dados_V2.csv',header=T,sep=';',dec=',',strip.white=F)
head(dados)
summary(dados)

#########################
###Explorando os dados###
#########################
names(dados)[names(dados)=='Point']<-'Sitio'
dados$Sitio<-gsub('point','Sitio',dados$Sitio)
dados$Categoria<-gsub('RAIZ','Raiz',dados$Categoria)
dados$Categoria<-factor(dados$Categoria,levels=c('Parte aérea','Raiz','Solo','Solo rizosferico'))

xtable(t(table(dados$Sitio)))
xtable(t(table(dados$Categoria)))
##########################
###Frequencia dos dados###
##########################
PA<-dados%>%filter(Categoria=='Parte aérea')%>%group_by(Sitio)%>%summarise(PA=n())
Raiz<-dados%>%filter(Categoria=='Raiz')%>%group_by(Sitio)%>%summarise(Raiz=n())
Solo<-dados%>%filter(Categoria=='Solo')%>%group_by(Sitio)%>%summarise(Solo=n())
SR<-dados%>%filter(Categoria=='Solo rizosferico')%>%group_by(Sitio)%>%summarise(SR=n())
dados%>%select(Sitio)%>%unique()%>%left_join(PA)%>%left_join(Raiz)%>%left_join(Solo)%>%left_join(SR)%>%
  xtable(caption = 'Contagem de missing values na amostra',align=rep('c',6),label='tab:NA')%>%
  print(include.rownames=FALSE,NA.string='0')


###################
###Tabela de Nas###
###################

PA<-dados%>%filter(is.na(P)&Categoria=='Parte aérea')%>%group_by(Sitio)%>%summarise(PA=n())
Raiz<-dados%>%filter(is.na(P)&Categoria=='Raiz')%>%group_by(Sitio)%>%summarise(Raiz=n())
Solo<-dados%>%filter(is.na(P)&Categoria=='Solo')%>%group_by(Sitio)%>%summarise(Solo=n())
SR<-dados%>%filter(is.na(P)&Categoria=='Solo rizosferico')%>%group_by(Sitio)%>%summarise(SR=n())
dados%>%select(Sitio)%>%unique()%>%left_join(PA)%>%left_join(Raiz)%>%left_join(Solo)%>%left_join(SR)%>%
  xtable(caption = 'Contagem de missing values na amostra',align=rep('c',6),label='tab:NA')%>%
  print(include.rownames=FALSE,NA.string='0')


DadosValidos<-filter(dados,!is.na(P))
head(DadosValidos)
xtable(t(table(DadosValidos$Sitio)))
xtable(t(table(DadosValidos$Categoria)))
PA<-DadosValidos%>%filter(Categoria=='Parte aérea')%>%group_by(Sitio)%>%summarise(PA=n())
Raiz<-DadosValidos%>%filter(Categoria=='Raiz')%>%group_by(Sitio)%>%summarise(Raiz=n())
Solo<-DadosValidos%>%filter(Categoria=='Solo')%>%group_by(Sitio)%>%summarise(Solo=n())
SR<-DadosValidos%>%filter(Categoria=='Solo rizosferico')%>%group_by(Sitio)%>%summarise(SR=n())
DadosValidos%>%select(Sitio)%>%unique()%>%left_join(PA)%>%left_join(Raiz)%>%left_join(Solo)%>%left_join(SR)%>%
  xtable(caption = 'Tamanho da Amostra',align=rep('c',6),label='tab:Tamanho')%>%
  print(include.rownames=FALSE,NA.string='0')

##################################################################################
###Tratando as variaveis numericas que foram carregadas como fatores/caracteres###
##################################################################################
DadosValidos$Cd<-as.character(DadosValidos$Cd)
DadosValidos$Cd<-gsub(",",".",DadosValidos$Cd)
DadosValidos$Cd<-gsub("<0","< 0",DadosValidos$Cd)
DadosValidos$Cd<-gsub("< 0.2","0",DadosValidos$Cd)
DadosValidos$Cd<-as.numeric(DadosValidos$Cd)

DadosValidos$Cr<-as.character(DadosValidos$Cr)
DadosValidos$Cr<-gsub("< 0,4","0",DadosValidos$Cr)
DadosValidos$Cr<-gsub(",",".",DadosValidos$Cr)
DadosValidos$Cr<-as.numeric(DadosValidos$Cr)

DadosValidos$Pb<-as.character(DadosValidos$Pb)
DadosValidos$Pb<-gsub(",",".",DadosValidos$Pb)
DadosValidos$Pb<-gsub("< 2","0",DadosValidos$Pb)
DadosValidos$Pb<-as.numeric(DadosValidos$Pb)

DadosValidos$As<-as.character(DadosValidos$As)
DadosValidos$As<-gsub(",",".",DadosValidos$As)
DadosValidos$As<-gsub("< 2","0",DadosValidos$As)
DadosValidos$As<-as.numeric(DadosValidos$As)

head(DadosValidos)
tail(DadosValidos)
summary(DadosValidos)

DadosValidos<-DadosValidos%>%select(-c(K,Ca,Al))

#########################
###Explorando os dados###
#########################


ggp1<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
       ggplot(aes(x=SitioNUM,y=P))+
       geom_boxplot()+
       facet_grid(.~Cat)+
       xlab('Sitio')+
       ylab('%')+
       ggtitle('P')

ggp2<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Mg))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('%')+
  ggtitle('MG')

ggp3<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=S))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('%')+
  ggtitle('S')

pdf('C:/Users/sebas/Dropbox/Projetos\ de\ artigos/Geologia\ Forence/Boxplot_P_Mg_S.PDF')
grid.arrange(ggp1,ggp2,ggp3,ncol=2,nrow=2)
dev.off()

ggp4<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Cu))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Cu')

ggp5<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Zn))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Zn')


ggp6<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Mn))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('mn')

ggp7<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Na))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Na')

ggp8<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Cd))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Cd')

ggp9<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Cr))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Cr')

ggp10<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Ni))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Ni')

ggp11<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Pb))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Pb')

ggp12<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=As))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('As')

ggp13<-DadosValidos%>%mutate(SitioNUM=gsub('Sitio ',"",Sitio),Cat=gsub('Solo rizosferico','Rizo',
                                                                      gsub('Parte aérea','Aérea',Categoria)))%>%
  ggplot(aes(x=SitioNUM,y=Ba))+
  geom_boxplot()+
  facet_grid(.~Cat)+
  xlab('Sitio')+
  ylab('mg/Kg')+
  ggtitle('Ba')

grid.arrange(ggp4,ggp5,ggp6,ggp7,ggp8,ggp9,ggp10,ggp11,ggp12,ggp13,ncol=2,nrow=10)

#########################################
#Comparação de médias via Kruskal-Wallis#
#########################################
elementos<-names(DadosValidos)[3:15]
cats<-unique(DadosValidos$Categoria)
formulas<-paste(elementos,names(DadosValidos)[2],sep='~')
Valor.P<-matrix(0,nrow = length(elementos),ncol=length(cats))

for(j in 1:4)
{
  aux<-DadosValidos%>%filter(Categoria==cats[j])
  aux$Sitio<-as.factor(aux$Sitio)
  for(i in 1:length(elementos))
    Valor.P[i,j]<-kruskal.test(as.formula(formulas[i]),data = aux)$p.value
}

colnames(Valor.P)<-cats
row.names(Valor.P)<-elementos

Valor.P<-round(Valor.P*100,2)

xtable(Valor.P,align = rep('c',5),caption = 'P-value dos testes de Kruskal-Wallis')



############################
###Estudando a Correlação###
############################

aux<-DadosValidos%>%select(-c(Id,Sitio,Categoria))
aux.r<-abs(cor(aux))
aux.col<-dmat.color(aux.r)
aux.o <- order.single(aux.r) 
pdf('C:/Users/sebas/Dropbox/Projetos\ de\ artigos/Geologia\ Forence/Correlacao_ElementosQuimicos.PDF')
cpairs(aux, aux.o, panel.colors=aux.col, gap=.5,
       main="Correlação entre os elementos químicos presente nas amostras",
       cex.main=1.25,pch=20)
dev.off()
sitios<-unique(DadosValidos$Sitio)

for(i in 1:8)
  for(j in 1:4)
  {
    nome.arquivo<-paste(sitios[i],cats[j],sep='_')
    nome.arquivo<-gsub(' ','.',nome.arquivo)
    nome.arquivo<-paste(nome.arquivo,'PDF',sep='.')
    link.arquivo<-paste('C:/Users/sebas/Dropbox/Projetos\ de\ artigos/Geologia\ Forence',nome.arquivo,sep='/')
    titulo.graf<-paste('Correlação entre os elementos químicos presente nas amostras de ', as.character(cats[j]),' do ',sitios[i],sep='')
    aux<-DadosValidos%>%subset(Sitio==sitios[i] & Categoria==cats[j],select=-c(Id,Sitio,Categoria))
    aux.r<-abs(cor(aux))
    aux.col<-dmat.color(aux.r)
    #aux.o <- order.single(aux.r) 
    pdf(link.arquivo)
    cpairs(aux, panel.colors=aux.col, gap=.5,
           main=titulo.graf,
           cex.main=.75)
    dev.off()
  }

#####################################################
###Matriz com os p-values dos testes de correlação###
#####################################################
elementos<-names(DadosValidos)[3:15]
cor.matrix<-matrix(0,length(elementos),length(elementos))
colnames(cor.matrix)<-elementos
row.names(cor.matrix)<-elementos
aux<-as.matrix(DadosValidos[,3:15])
for(i in 1:nrow(cor.matrix))
 for(j in 1:nrow(cor.matrix))
  cor.matrix[i,j]<-cor.test(aux[,i],aux[,j])$p.value
cor.matrix<-round(100*cor.matrix,2)
xtable(cor.matrix)
cor.matrix<-as.character(cor.matrix)

for(i in 1:nrow(cor.matrix))
  for(j in 1:nrow(cor.matrix))
    cor.matrix[i,j]<-paste(cor.matrix[i,j],'%')

###########################
###Abordagem de C.A.R.T.###
###########################

####################################################
##Tentando umaarvore treinada com os dados de solo##
##e testada com os dados das plantas################
####################################################

###################################
###Definindo a semente aleatoria###
###################################

set.seed(6669)
####################################################################################################
###Para a função tree, a variável resposta precisa ser, obrigatoriamente, tipo numérica ou fator.###
####################################################################################################
DadosValidos$Sitio<-factor(DadosValidos$Sitio,levels =unique(DadosValidos$Sitio))


Dados.Treino<-DadosValidos%>%group_by(Sitio)%>%sample_frac(.8)
Dados.Teste<-filter(DadosValidos,!Id%in%Dados.Treino$Id)


Arvore.Conjunta.Treino<-tree(Sitio~P+Mg+S+Cu+Zn+Mn+Na+Cd+Cr+Ni+Pb+As+Ba+Categoria,data=Dados.Treino)
plot(Arvore.Conjunta.Treino)
text(Arvore.Conjunta.Treino)
summary(Arvore.Conjunta.Treino)$misclass
title(main="Grupo de treino: 80% Amostras de Solo + 80% Amostras Vegetais \n Grupo de validação: 20% Amostras de Solo + 20% Amostras Vegetais",sub="Assertividade: 72,85%")

pdf('D:/Disco C/Documentos/Trabalho\ de\ Geologia\ forence/ClassificadorConjunto.PDF')
plot(Arvore.Conjunta.Treino)
text(Arvore.Conjunta.Treino)
title(main="Grupo de treino: 80% Amostras de Solo + 80% Amostras Vegetais \n Grupo de validação: 20% Amostras de Solo + 20% Amostras Vegetais",sub="Assertividade: 72,85%")
dev.off()



previsão.Conjunta.Teste<-predict(Arvore.Conjunta.Treino,Dados.Teste)
head(previsão.Conjunta.Teste)

maxidx.1 <- function(arr) {
Maximo<-which(arr == max(arr))
if(length(Maximo)==1) return(Maximo) else return(sample(Maximo,size=1))
}

idx <- apply(previsão.Conjunta.Teste, 1, maxidx.1)

Previsão.Conjunta<-c('Sitio 1','Sitio 2','Sitio 3','Sitio 4','Sitio 5','Sitio 6','Sitio 7','Sitio 8')[idx]

table(Previsão.Conjunta, Dados.Teste$Sitio)%>%xtable()
9/sum(table(Previsão.Conjunta, Dados.Teste$Sitio))


###########################################
###Tentando uma abordagem de rede neural###
###########################################

Rede<-rxNeuralNet(Sitio~P+Mg+S+Cu+Zn+Mn+Na+Cd+Cr+Ni+Pb+As+Ba+Categoria,data=Dados.Treino,type = "multiClass")
summary(Rede)

previsaoREDE<-rxPredict(Rede,data =Dados.Teste, extraVarsToWrite='Sitio')
previsaoREDE
Erro<-previsaoREDE$Sitio!=previsaoREDE$PredictedLabel

sum(Erro)*100/nrow(previsaoREDE)
################################################
###Com a nova extração dos dados, ##############
#a assertividade da arvore aumentou para 53,46%#
################################################

############################
###Tentando Random Forest###
############################

RFSolo<-randomForest(Sitio~S+K+P+Mg+Ca+Cu+Zn+Mn+Na+Cd+Cr+Ni+Pb+Al+As+Ba
,data=DadosValidos.Solo,importance=TRUE, do.trace=100000, ntree=100000)
print(RFSolo)
RFPrevisãoPlanta<-predict(RFSolo,select(DadosValidos.Planta,S,K,P,Mg,Ca,Cu,Zn,Mn,Na,Cd,Cr,Ni,Pb,Al,As,Ba))
table(RFPrevisãoPlanta,DadosPlanta$Sitio)
table(DadosPlanta$Sitio,RFPrevisãoPlanta)

rfcv(select(DadosSolo,S,K,P,Mg,Ca),DadosSolo$Sitio)
table(DadosPlanta$Sitio,rfcv(select(DadosSolo,S,K,P,Mg,Ca),DadosSolo$Sitio)$predicted$`5`)
table(rfcv(select(DadosSolo,S,K,P,Mg,Ca),DadosSolo$Sitio)$predicted$`5`,DadosPlanta$Sitio)

#########################################
###Tentando Conditional Inference Tree###
#########################################

CTSolo<-ctree(Sitio~.,data=select(DadosSolo,-ID,-Categoria))
plot(CTSolo)
print(CTSolo)
CTPlanta<-predict(CTSolo,select(DadosPlanta,S,K,P,Mg,Ca,Cu,Zn,Fe,Mn,Na,Cd,Cr,Ni,Pb,Categoria))
table(CTPlanta,DadosPlanta$Sitio)
table(DadosPlanta$Sitio,CTPlanta)
