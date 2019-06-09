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
                       "AUC","plotROC");

Install_And_Load(Required_Packages)

options(scipen = 9999,digits = 10)
##############################################################################################
##############################################################################################
##############################################################################################

dados <- fread('./GitHub/Amostra.csv')
glimpse(dados)

dados %>% mutate(id=paste(medallion,hack_license),
                 vendor_id=factor(vendor_id,levels = unique(vendor_id)),
                 store_and_fwd_flag=case_when(is.na(store_and_fwd_flag) ~ '-'),
                 pickup_datetime=as.POSIXct(pickup_datetime,format='%Y-%m-%d %H:%M:%S'),
                 dropoff_datetime=as.POSIXct(dropoff_datetime,format='%Y-%m-%d %H:%M:%S'),
                 payment_type=factor(payment_type,levels = unique(payment_type))) %>% 
  select(-c(medallion,hack_license)) %$% store_and_fwd_flag %>% table
  
  