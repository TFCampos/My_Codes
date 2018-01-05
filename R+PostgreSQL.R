###############################################
##################Preambulo####################
###############################################
rm(list=ls())
gc()
###############################################
############Preparação dos pacotes#############
###############################################
# Selecioanar uma CRAN mirror
local({r <- getOption("repos")
r["CRAN"] <- "https://vps.fmvz.usp.br/CRAN/"
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
Required_Packages =c("RODBC","ggplot2","dplyr","stringr",
                     "AUC","lubridate","plotROC","CHAID",
                     "RPostgreSQL","xlsx","MicrosoftML");

Install_And_Load(Required_Packages)

options(scipen = 9999,digits = 10)
##############################################################################################
##############################################################################################
##############################################################################################

#########################################
###Definindo os paramentros da conexão###
#########################################

Usuario<-'postgres'
Senha<-'iron6669'
Host<-'localhost'
NomeDB<-'NYC_Taxi_Trip'

#######################################
###Carregando o driver do POstgreSQL###
#######################################
drv <- dbDriver("PostgreSQL")

#######################################
###Estabelecendo coneção com o banco###
#######################################
con <- dbConnect(drv, dbname = NomeDB,
                 host = Host, port = 5432,
                 user = Usuario, password = Senha)
###############################
##Informações sobre a conexão##
###############################
dbGetInfo(con)

###################
###Lendo tabelas###
###################

#######################################################
#Aqui leio apenas os nomes de tabelas do schema public#
#######################################################
(Table_Name<-dbGetQuery(con,"SELECT table_name FROM information_schema.tables WHERE table_schema='public'"))
paste('public',Table_Name$table_name,sep='.')

Teste_DB<-dbGetQuery(con, 'SELECT * FROM public."Trip_Data_Fare" limit 10')
head(Teste_DB)
##################################
#Lendo a primeira tabela do banco#
##################################
Base_1<-dbGetQuery(con, 'SELECT * FROM public."Trip_Data_Fare_1"')
head(Base_1)
tail(Base_1)
str(Base_1)


###################################################
###Fechando a conexão e descarregando os drivers###
###################################################

dbDisconnect(con)
dbUnloadDriver(drv)