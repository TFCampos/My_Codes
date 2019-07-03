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
Required_Packages <- c('tidyverse',"mlr","data.table",
                       "AUC","plotROC",'RPostgreSQL',
                       'beepr','tictoc','parallelMap');

Install_And_Load(Required_Packages)

options(scipen = 9999,digits = 10)
##############################################################################################
##############################################################################################
##############################################################################################

# dados <- fread('./GitHub/Amostra.csv')
# glimpse(dados)




#########################################
###Definindo os paramentros da conexÃ£o###
#########################################

Usuario<-'postgres'
Senha<-'senha'
Host<-'localhost'
NomeDB<-'NYC_Taxi_Trip'

#######################################
###Carregando o driver do POstgreSQL###
#######################################
drv <- dbDriver("PostgreSQL")

#######################################
###Estabelecendo coneÃ§Ã£o com o banco###
#######################################
con <- dbConnect(drv, dbname = NomeDB,
                 host = Host, port = 5432,
                 user = Usuario, password = Senha)
###############################
##InformaÃ§Ãµes sobre a conexÃ£o##
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
dados<-dbGetQuery(con, 'SELECT * FROM public."Trip_Data_Fare_Amostra"')
head(dados)
tail(dados)
glimpse(dados)

dados$store_and_fwd_flag %>% table


dados %>% mutate(vendor_id=factor(vendor_id,levels = unique(vendor_id)),
                 store_and_fwd_flag=case_when(is.na(store_and_fwd_flag) ~ '-',
                                              store_and_fwd_flag=='N' ~ 'N',
                                              store_and_fwd_flag=='Y' ~ 'Y'),
                 payment_type=factor(payment_type,levels = unique(payment_type)),
                 TARGET=if_else(tip_amount>0,1,0)) %>%
  mutate(store_and_fwd_flag=factor(store_and_fwd_flag,levels = c('-','N','Y')),
         TARGET=factor(TARGET,0:1)) %>% 
  select(-c(medallion,hack_license,pickup_datetime,dropoff_datetime,tip_amount)) %>%
  filter(!is.na(dropoff_longitude)) -> dados_aux

set.seed(666)
train_index <- sample(1:nrow(dados_aux), 0.7 * nrow(dados_aux))
test_index <- setdiff(1:nrow(dados_aux), train_index)

treino <- dados_aux[train_index,]
teste <- dados_aux[test_index,]

tsk_treino <- makeClassifTask(id='NYC_Train',data = treino ,target = 'TARGET',positive = '1')
tsk_teste <- makeClassifTask(id='NYC_Test',data = teste,target = 'TARGET',positive = '1')



learner <- makeLearner('classif.ranger',fix.factors.prediction = TRUE,
                       predict.type = "prob",
                       mtry=getTaskFeatureNames(tsk_treino) %>% length() %>% sqrt)


ps=makeParamSet(makeIntegerParam(id='num.trees',lower = 10,upper = 1000),
                makeIntegerParam(id='min.node.size',lower = 10,upper = 10000),
                makeDiscreteParam(id='importance',values = c('none','impurity','permutation')),
                makeDiscreteParam(id='splitrule',values = c('gini','extratrees')))

inner = makeResampleDesc("CV", iters = 10)
set.seed(666)
ctrl <-  makeTuneControlRandom(100)
lrn <- makeTuneWrapper(learner = learner,
                       resampling = inner,
                       measures = auc,
                       par.set = ps,
                       control = crtl)
outer <- makeResampleDesc("CV", iters = 5)
tic('Neted Cross Validation')
r <- resample(lrn, tsk_treino, resampling = outer, extract = getTuneResult, show.info = T)
toc()
beep(4)