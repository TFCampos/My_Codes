rm(list=ls())
gc()
library(mlr)
library(caret)
library(ranger)
####Testando ranger com o ranger+mlr####

ptm <- proc.time()
# Define task and learner
task <- makeClassifTask(id = "iris",
                        data = iris,
                        target = "Species")
learner <- makeLearner("classif.ranger",num.threads = 2)
# Choose resampling strategy and define grid
rdesc <- makeResampleDesc("CV", iters = 5)
ps <- makeParamSet(makeIntegerParam("mtry", 3, 4),
                   makeDiscreteParam("num.trees", 200))
# Tune
res = tuneParams(learner, task, rdesc, par.set = ps,
                 control = makeTuneControlGrid())
# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(makeLearner("classif.ranger",num.threads = 2), par.vals = res$x)
m = mlr::train(lrn, iris.task)
print(m)
print(proc.time() - ptm) # ~6 seconds
####Testando o ranger+caret
ptm <- proc.time()
data(iris)
grid <-  expand.grid(mtry = c(3,4),splitrule='gini',min.node.size=1)
fitControl <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE)
fit = caret::train(
  x = iris[ , names(iris) != 'Species'],
  y = iris[ , names(iris) == 'Species'],
  method = 'ranger',
  num.threads = 2,
  num.trees = 200,
  tuneGrid = grid,
  trControl = fitControl
)
print(fit)
print(proc.time() - ptm)
####Testando um grid saerch na mão
hyper_grid <- expand.grid(
  mtry       = 3:4,
  node_size  = 1,
  num.trees = 200,
  OOB_RMSE   = 0
)
system.time(
  for(i in 1:nrow(hyper_grid)) {
    # train model
    rf <- ranger(
      formula        = Species ~ .,
      data           = iris,
      num.trees      = hyper_grid$num.trees[i],
      mtry           = hyper_grid$mtry[i],
      min.node.size  = hyper_grid$node_size[i],
      importance = 'impurity',
      num.threads = 2)
    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(rf$prediction.error)
  })
