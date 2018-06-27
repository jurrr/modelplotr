###################################################################################
# GENERAL CHECKS
###################################################################################

packageVersion("mlr")
devtools::load_all('C:/TEMP/modelplotr')
library(roxygen2)
#library(modelplotr)

??modelplotr
?dataprep_modevalplots
?input_modevalplots
?scope_modevalplots


devtools::use_vignette("modelplotr")
devtools::use_testthat()
usethis::use_testthat()


###################################################################################
# TEST WITH IRIS
###################################################################################

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
head(eval_tot)
tail(eval_tot)
dim(eval_tot)

input_modevalplots()
head(eval_t_tot[eval_t_tot$modelname=="random forest"&
                eval_t_tot$dataset=="train data"&
                eval_t_tot$category=="setosa",],11)
tail(eval_t_tot)
scope_modevalplots(eval_type="CompareDatasets",
                   select_model = "random forest")
scope_modevalplots(eval_type="CompareModels")
scope_modevalplots(eval_type="CompareTargetValues")



#scope_modevalplots(eval_type="TargetValues",select_model="Discriminant")
head(eval_t_type)
tail(eval_t_type)

#`%>%` <- magrittr::`%>%`

eval_t_type %>% dplyr::group_by(legend) %>% dplyr::summarize(n=n())
test %>% dplyr::group_by(Species) %>% dplyr::summarize(n=n())

cumgains <- cumgains()
cumgains
lift <- lift()
lift
response <- response()
response
cumresponse <- cumresponse()
multiplot(cumgains,lift,response,cumresponse,cols=2)

# save plots
savemodelplots()





###################################################################################
# test with bankingdata
###################################################################################

zipname = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip'
csvname = 'bank-additional/bank-additional.csv'

temp <- tempfile()
download.file(zipname,temp, mode="wb")
bank <- read.table(unzip(temp,csvname),sep=";", stringsAsFactors=FALSE,header = T)
summary(bank)
unlink(temp)

bank$y = as.factor(bank$y)


test_size = 0.3
train_index =  sample(seq(1, nrow(bank)),size = (1 - test_size)*nrow(bank) ,replace = F )
train = bank[train_index,]
test = bank[-train_index,]

# subset with 4 columns
train <- train[,c('duration','campaign','pdays','previous','euribor3m','y')]
test <- test[,c('duration','campaign','pdays','previous','euribor3m','y')]



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

input_modevalplots()
tail(eval_t_tot)

scope_modevalplots(eval_type="CompareDatasets")
#scope_modevalplots(eval_type="CompareModels")
#scope_modevalplots(eval_type="TargetValues",select_model="Discriminant")
head(eval_t_type)
tail(eval_t_type)

scope_modevalplots(eval_type="CompareDatasets",
  select_model = "random forest")
scope_modevalplots(eval_type="CompareModels")
scope_modevalplots(eval_type="CompareTargetValues")

cumgains <- cumgains()
cumgains
lift <- lift()
lift
response <- response()
response
cumresponse <- cumresponse()
cumresponse
multiplot(cumgains,lift,response,cumresponse,cols=2)

# save plots
savemodelplots()



###################################################################################
# tutorial mdl
###################################################################################
# https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
install.packages("mlr")
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

