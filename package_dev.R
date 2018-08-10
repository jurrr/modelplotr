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



RColorBrewer::display.brewer.all(n=4)
devtools::install_github('jurrr/modelplotr')
packageVersion("randomForest")
devtools::load_all('C:/TEMP/modelplotr')
library(roxygen2)
#library(modelplotr)

??modelplotr
?prepare_scores_and_deciles
?aggregate_over_deciles
?plotting_scope

?modelplotr

devtools::use_vignette("modelplotr")
devtools::use_testthat()
usethis::use_testthat()


# install package modelplotr from Github
devtools::install_github("jurrr/modelplotr")
library(modelplotr)
chooseCRANmirror()

chooseCRANmirror(graphics=FALSE, ind=1)
?modelplotr

?savemodelplots
savemodelplots("plot_cumgains")
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
addNoise <- function(x) round(rnorm(n=100,mean=mean(x),sd=sd(x)),1)
iris_addnoise <- as.data.frame(lapply(iris[1:4], addNoise))
iris_addnoise$Species <- sample(unique(iris$Species),100,replace=TRUE)
iris <- rbind(iris,iris_addnoise)


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
prepare_scores_and_deciles(datasets=list("train","test"),
                      dataset_labels = list("train data","test data"),
                      models = list("rf","mnl","xgb","lda"),
                      model_labels = list("random forest","multinomial logit","XGBoost","Discriminant"),
                      target_column="Species")
#head(scores_and_deciles)
aggregate_over_deciles()
plotting_scope(scope = "compare_targetclasses")
plot_cumgains()
plot_cumgains(highlight_decile = 3)
plot_cumlift()
plot_cumlift(highlight_decile = 2)
plot_response()
plot_response(highlight_decile = 2)

aggregate_over_deciles(prepared_input = scores_and_deciles)
#head(deciles_aggregate)

plotting_scope(scope="compare_datasets",select_model_label = "random forest")
plotting_scope(scope="compare_models")
plotting_scope(scope="compare_targetclasses")
plotting_scope(scope="compare_datasets",select_model_label = "multinomial logit")
plotting_scope(scope = "compare_datasets",select_targetclass = NA)
plotting_scope(scope = "no_comparison",select_smallest_targetclass = FALSE)
plotting_scope(scope = "compare_models",select_smallest_targetclass = FALSE)
plotting_scope(scope = "compare_datasets",select_dataset_label = "train data")
plotting_scope(scope = "compare_targetclasses",select_dataset_label = list("train data","test data"))
plotting_scope(scope = "compare_targetclasses",select_targetclass = "setosa")
plotting_scope(scope = "compare_targetclasses", select_targetclass = list("setosa", "virginica"))
plotting_scope(scope = "compare_models",select_model_label = list("multinomial logit", "XGBoost"))
plotting_scope(scope = "compare_models",select_dataset_label = "train data" )
plotting_scope(scope = "compare_models", select_targetclass = list("setosa", "virginica"))
plotting_scope()
fourevalplots()

plot_cumgains()

plot_cumgains(highlight_decile = 2)

?modelplotr
?prepare_scores_and_deciles
?aggregate_over_deciles
?plotting_scope
#head(plot_input)

plot_cumgains(explainAtDecile = 3)

#`%>%` <- magrittr::`%>%`

#plot_input %>% dplyr::group_by(legend) %>% dplyr::summarize(n=n())
#test %>% dplyr::group_by(Species) %>% dplyr::summarize(n=n())

plot_cumgains1 <- plot_cumgains()
plot_cumgains1
plot_cumlift1 <- plot_cumlift()
plot_cumlift1
plot_response1 <- plot_response()
plot_response1
plot_cumresponse1 <- plot_cumresponse()
plot_cumresponse1
multiplot(plot_cumgains(),plot_cumlift(),plot_response(),plot_cumresponse(),cols=2)
str(fourevalplots())

# save plots
savemodelplots(c("plot_cumgains1","plot_cumlift1","plot_response1","plot_cumresponse1"))




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
prepare_scores_and_deciles(datasets=list("train","test"),
  dataset_labels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  model_labels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  target_column="y")
head(scores_and_deciles)

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
prepare_scores_and_deciles(datasets=list("train","test"),
  dataset_labels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  model_labels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  target_column="y")
head(scores_and_deciles)
tail(scores_and_deciles)
dim(scores_and_deciles)

# apply modelplotr
library(modelplotr)
prepare_scores_and_deciles(datasets=list("train","test"),
  dataset_labels = list("train data","test data"),
  models = list("rf","mnl","xgb","lda"),
  model_labels = list("random forest","multinomial logit","XGBoost","Discriminant"),
  target_column="y")
head(scores_and_deciles)
aggregate_over_deciles()
plotting_scope()

aggregate_over_deciles()
tail(deciles_aggregate)

plotting_scope(scope="compare_datasets")
#plotting_scope(scope="compare_models")
#plotting_scope(scope="TargetValues",select_model_label="Discriminant")
head(plot_input)
tail(plot_input)

plotting_scope(scope="compare_datasets")
plotting_scope(scope="compare_models")
plotting_scope(scope="compare_targetclasses")
plotting_scope()

plotting_scope(select_model_label = "random forest",select_targetclass = "no",select_dataset_label = "train data")

plot_cumgains <- plot_cumgains(custom_line_colors=c("green"))
plot_cumgains + ggplot2::ggtitle("Gains plot") + ggplot2::theme(legend.position=c(0.975,0.025),legend.justification=c(1, 0))
plot_cumlift <- plot_cumlift()
plot_cumlift
plot_response <- plot_response()
plot_response
plot_cumresponse <- plot_cumresponse()
plot_cumresponse
multiplot(plot_cumgains,plot_cumlift,plot_response,plot_cumresponse,cols=2)

plot_cumresponse <- plot_cumresponse()
plot_cumresponse

# save plots
savemodelplots()

codetools::findGlobals(plot_response)

codetools::findGlobals(prepare_scores_and_deciles)
codetools::findGlobals(aggregate_over_deciles)
codetools::findGlobals(plotting_scope)

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



??modelplotr

?plot_cumgains()
