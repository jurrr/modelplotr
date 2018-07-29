###################################################################################
# GENERAL CHECKS
###################################################################################
# install.packages('devtools')
# install.packages('roxygen2')
# install.packages('testthat')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('magrittr')
# install.packages('tidyverse')
# install.packages('RColorBrewer')
# install.packages('mlr')
# install.packages('randomForest')
# install.packages('knitr')
# install.packages('rmarkdown')
# install.packages("ggfittext")
??ggfittext
devtools::install_github("r-lib/devtools")
library(devtools)
library(roxygen2)

library(grid)

R.home()
file.edit(file.path("~", ".Rprofile")) # edit .Rprofile in HOME



devtools::install_github('jurrr/modelplotr')
packageVersion("randomForest")
devtools::load_all('C:/TEMP/modelplotr')
library(roxygen2)
#library(modelplotr)

??modelplotr
?dataprep_modevalplots
?input_modevalplots
?scope_modevalplots

?modelplotr

devtools::use_vignette("modelplotr")
devtools::use_testthat()
usethis::use_testthat()


# install package modelplotr from Github
devtools::install_github("jurrr/modelplotr")
library(modelplotr)
chooseCRANmirror()

chooseCRANmirror(graphics=FALSE, ind=1)



###################################################################################
# PACHAGE EXAMPLE
###################################################################################



###################################################################################
# TEST WITH IRIS
###################################################################################

library(modelplotr)

# prepare iris dataset
data(iris)
# add some noise to iris to prevent perfect models

iris_addnoise = data.frame(
  Sepal.Length=round(rnorm(n=100,mean=mean(iris$Sepal.Length),sd=sd(iris$Sepal.Length)),1),
  Sepal.Width=round(rnorm(n=100,mean=mean(iris$Sepal.Width),sd=sd(iris$Sepal.Width)),1),
  Petal.Length=round(rnorm(n=100,mean=mean(iris$Petal.Length),sd=sd(iris$Petal.Length)),1),
  Petal.Width=round(rnorm(n=100,mean=mean(iris$Petal.Width),sd=sd(iris$Petal.Width)),1),
  Species=sample(unique(iris$Species),100,replace=TRUE))

iris <- rbind(iris,iris_addnoise)
test_size = 0.3
train_index =  sample(seq(1, nrow(iris)),size = (1 - test_size)*nrow(iris) ,replace = F )
train = iris[train_index,]
test = iris[-train_index,]
# estimate models with mlr
# this line is needed when usin mlr without loading it (mlr::)
mlr::configureMlr()
#estimate models
task = mlr::makeClassifTask(data = train, target = "Species")
lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
rf = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
mnl = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.xgboost", predict.type = "prob")
xgb = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.lda", predict.type = "prob")
lda = mlr::train(lrn, task)


# apply modelplotr functions
dataprep_modevalplots(datasets=list("train","test"),
                      datasetlabels = list("train data","test data"),
                      models = list("rf","mnl","xgb","lda"),
                      modellabels = list("random forest","multinomial logit","XGBoost","Discriminant"),
                      targetname="Species")
#head(eval_tot)
input_modevalplots()
scope_modevalplots(eval_type = "CompareTargetValues")
cumgains()
cumgains(highlight_decile = 3)
lift()
lift(highlight_decile = 2)
response()
response(highlight_decile = 2)

input_modevalplots(prepared_input = eval_tot)
#head(eval_t_tot)

scope_modevalplots(eval_type="CompareDatasets",select_model = "random forest")
scope_modevalplots(eval_type="CompareModels")
scope_modevalplots(eval_type="CompareTargetValues")
scope_modevalplots(eval_type="CompareDatasets",select_model = "multinomial logit")
scope_modevalplots(eval_type = "CompareDatasets",select_targetvalue = NA)
scope_modevalplots(eval_type = "NoComparison",select_smallesttargetvalue = FALSE)
scope_modevalplots(eval_type = "CompareModels",select_smallesttargetvalue = FALSE)
scope_modevalplots(eval_type = "CompareDatasets",select_dataset = "train data")
scope_modevalplots(eval_type = "CompareTargetValues",select_dataset = list("train data","test data"))
scope_modevalplots(eval_type = "CompareTargetValues",select_targetvalue = "setosa")
scope_modevalplots(eval_type = "CompareTargetValues", select_targetvalue = list("setosa", "virginica"))
scope_modevalplots(eval_type = "CompareModels",select_model = list("multinomial logit", "XGBoost"))
scope_modevalplots(eval_type = "CompareModels",select_dataset = "train data" )
scope_modevalplots(eval_type = "CompareModels", select_targetvalue = list("setosa", "virginica"))
scope_modevalplots()
fourevalplots()

cumgains()

cumgains(highlight_decile = 2)

?modelplotr
?dataprep_modevalplots
?input_modevalplots
?scope_modevalplots
#head(eval_t_type)

cumgains(explainAtDecile = 3)

#`%>%` <- magrittr::`%>%`

#eval_t_type %>% dplyr::group_by(legend) %>% dplyr::summarize(n=n())
#test %>% dplyr::group_by(Species) %>% dplyr::summarize(n=n())

cumgains1 <- cumgains()
cumgains1
lift1 <- lift()
lift1
response1 <- response()
response1
cumresponse1 <- cumresponse()
cumresponse1
multiplot(cumgains(),lift(),response(),cumresponse(),cols=2)
str(fourevalplots())

# save plots
savemodelplots(c("cumgains1","lift1","response1","cumresponse1"))




###################################################################################
# test with bankingdata
###################################################################################

zipname = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip'
csvname = 'bank-additional/bank-additional.csv'

temp <- tempfile()
download.file(zipname,temp, mode="wb")
bank <- read.table(unzip(temp,csvname),sep=";", stringsAsFactors=FALSE,header = T)
bank <- bank[,c('duration','campaign','pdays','previous','euribor3m','y')]
summary(bank)
unlink(temp)

data = iris
task = mlr::makeClassifTask(data = bank, target = "y")
availableAlgorithms = mlr::listLearners(task, check.packages = FALSE)$name
print(paste0('Number of alorithms: ',length(availableAlgorithms)))
print(paste0('Algorithms (first 20): ',paste(availableAlgorithms[1:20],collapse = ', '),' ,...'))
?mlr::listLearners
bank$y = as.factor(bank$y)


test_size = 0.3
train_index =  sample(seq(1, nrow(bank)),size = (1 - test_size)*nrow(bank) ,replace = F )
train = bank[train_index,]
test = bank[-train_index,]

# subset with 4 columns
train <- train[,c('duration','campaign','pdays','previous','euribor3m','y')]
test <- test[,c('duration','campaign','pdays','previous','euribor3m','y')]

install.packages('xgboost')
# prepare data for training and train models
bank$y = as.factor(bank$y)
test_size = 0.3
train_index =  sample(seq(1, nrow(bank)),size = (1 - test_size)*nrow(bank) ,replace = F )
train = bank[train_index,]
test = bank[-train_index,]

# estimate models with mlr
library(mlr)
#models
task = makeClassifTask(data = train, target = "y")
lrn = makeLearner("classif.randomForest", predict.type = "prob")
rf = train(lrn, task)
lrn = makeLearner("classif.multinom", predict.type = "prob")
mnl = train(lrn, task)
lrn = makeLearner("classif.xgboost", predict.type = "prob")
xgb = train(lrn, task)
lrn = makeLearner("classif.lda", predict.type = "prob")
lda = train(lrn, task)


# estimate models with mlr
library(mlr)
#models
task = makeClassifTask(data = train, target = "y")
lrn = makeLearner("classif.randomForest", predict.type = "prob")
rf = train(lrn, task)
lrn = makeLearner("classif.multinom", predict.type = "prob")
mnl = train(lrn, task)
lrn = makeLearner("classif.xgboost", predict.type = "prob")
xgb = train(lrn, task)
lrn = makeLearner("classif.lda", predict.type = "prob")
lda = train(lrn, task)

# apply modelplotr
library(modelplotr)
dataprep_modevalplots(datasets=list("train","test"),
  datasetlabels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  modellabels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  targetname="y")
head(eval_tot)

# estimate models with mlr
# this line is needed when usin mlr without loading it (mlr::)
mlr::configureMlr()
#models
task = mlr::makeClassifTask(data = train, target = "y")
lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
rf = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
mnl = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.xgboost", predict.type = "prob")
xgb = mlr::train(lrn, task)
lrn = mlr::makeLearner("classif.lda", predict.type = "prob")
lda = mlr::train(lrn, task)

# apply modelplotr
dataprep_modevalplots(datasets=list("train","test"),
  datasetlabels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  modellabels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  targetname="y")
head(eval_tot)
tail(eval_tot)
dim(eval_tot)

# apply modelplotr
library(modelplotr)
dataprep_modevalplots(datasets=list("train","test"),
  datasetlabels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  modellabels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  targetname="y")
head(eval_tot)
input_modevalplots()
scope_modevalplots()

input_modevalplots()
tail(eval_t_tot)

scope_modevalplots(eval_type="CompareDatasets")
#scope_modevalplots(eval_type="CompareModels")
#scope_modevalplots(eval_type="TargetValues",select_model="Discriminant")
head(eval_t_type)
tail(eval_t_type)

scope_modevalplots(eval_type="CompareDatasets")
scope_modevalplots(eval_type="CompareModels")
scope_modevalplots(eval_type="CompareTargetValues")
scope_modevalplots()

scope_modevalplots(select_model = "random forest",select_targetvalue = "no",select_dataset = "train data")

cumgains <- cumgains(customlinecolors=c("green"))
cumgains + ggplot2::ggtitle("Gains plot") + ggplot2::theme(legend.position=c(0.975,0.025),legend.justification=c(1, 0))
lift <- lift()
lift
response <- response()
response
cumresponse <- cumresponse()
cumresponse
multiplot(cumgains,lift,response,cumresponse,cols=2)

cumresponse <- cumresponse()
cumresponse

# save plots
savemodelplots()

codetools::findGlobals(response)

codetools::findGlobals(dataprep_modevalplots)
codetools::findGlobals(input_modevalplots)
codetools::findGlobals(scope_modevalplots)

###################################################################################
# tutorial mdl
###################################################################################
# https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
library(mlr)

summarizeColumns(train)



trainTask <- makeClassifTask(data = cd_train,target = "y")
testTask <- makeClassifTask(data = cd_test, target = "y")

getParamSet("classif.randomForest")

rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 4),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

#cv accuracy
rf_tune$y

#best parameters
rf_tune$x

#using hyperparameters for modeling
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(rf.tree, trainTask)
getLearnerModel(rforest)

#make predictions
rfmodel <- predict(rforest, testTask)
predprob <- getPredictionProbabilities(rfmodel)
rfmodel$data

??mlr::predict


task = makeClassifTask(data = cd_train, target = "y")
getTaskDesc(task)$positive

lrn = makeLearner("classif.randomForest", predict.type = "prob")
rf = train(lrn, task)
# predict probabilities
pred = predict(rf, newdata = cd_test)

# Get probabilities for all classes
head(getPredictionProbabilities(pred))

