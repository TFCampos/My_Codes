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
Required_Packages =c('rpart','rattle','rpart.plot','rpart.plot','RColorBrewer','party','partykit','caret');
Install_And_Load(Required_Packages)

# Plotting Classification Trees with the plot.rpart and rattle pckages

library(rpart)           		      # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				      # Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
						      # but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
 
# Make big tree
form <- as.formula(Class ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=3)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
#-------------------------------------------------------------------
# Plot a tree built with RevoScaleR
# Construct a model formula
sdNames <- names(segmentationData)
X <- as.vector(sdNames[-c(1,2,3)])
form <- as.formula(paste("Class","~", paste(X,collapse="+")))
# Run the model
rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
                   minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
# Plot the tree						
prp(rxAddInheritance(rx.tree))
fancyRpartPlot(rxAddInheritance(rx.tree))