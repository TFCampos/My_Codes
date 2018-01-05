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
Required_Packages =c("RODBC","ggplot2","dplyr","xtable","ggdendro",
                     "tree","stringr","AUC","lubridate","rpart","nnet",
                     "mgcv","randomForest","party","plotROC","CHAID","RPostgreSQL",
                     "ReporteRs","xlsx","MicrosoftML");

Install_And_Load(Required_Packages)

options(scipen = 9999,digits = 10)
##############################################################################################
##############################################################################################
##############################################################################################