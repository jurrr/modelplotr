library(roxygen2)



# prepare iris dataset
data(iris)
colnames(iris) = c('sepal_length', 'sepal_width', 'petal_length', 'petal_width', 'species')
test_size = 0.3
train_index =  sample(seq(1, nrow(iris)),size = (1 - test_size)*nrow(iris) ,replace = F )
train = iris[train_index,]
test = iris[-train_index,]
# estimate Random Forest
clf <- randomForest::randomForest(species ~ ., data=train, importance = T)

install.packages("xgboost")
install.packages("nnet")

library(xgboost)
??xgboost


clf <- xgboost::xgboost(data=as.matrix(train[,1:4]),label=train$species,nrounds = 20)
clf <- nnet::multinom(species ~ ., data = train)

clf <- randomForest::randomForest(species ~ ., data=train, importance = T)


library(modelplotr)
# plot

dataprep_modevalplots(targetname="species")

train1=as.matrix(train[,1:4])
test1=as.matrix(test[,1:4])

dataprep_modevalplots(datasets=list("train1","test1"),targetname="species")
dataprep_modevalplots(targetname="species")
input_modevalplots()
cumgains <- cumgains()
lift <- lift()
response <- response()
cumresponse <- cumresponse()
multiplot(cumgains,lift,response,cumresponse,cols=2)

devtools::install_github('jurrr/modelplotr')
devtools::document()

# Predict the labels of the test data: y_pred
y_pred <- predict(clf, test)

clf$importance

confusionMatrix(y_pred, test$species)

devtools::load_all('C:/TEMP/modelplotr')

dataprep_modevalplots(depvar = 'species')

eval_tot

library(roxygen2)

devtools::document()

?dataprep_modevalplots
?input_modevalplots


devtools::use_vignette("modelplotr")

devtools::use_testthat()
usethis::use_testthat()


file.exists("~/.shh/id_rsa.pub")


list.files(
  path=c("c:/program files", "c:/program files (x86)"),
  pattern="git.exe",
  full.names=TRUE,
  recursive=TRUE
)

list.files(
  path=c("c:/"),
  pattern="svn.exe",
  full.names=TRUE,
  recursive=TRUE
)


# test with bankingdata

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

summary(bank)
# estimate Random Forest
clf <- randomForest::randomForest(y ~ duration+campaign, data=train, importance = T)

levels(bank$y)
library(modelplotr)
dataprep_modevalplots(targetname="y")
input_modevalplots()
cumgains <- cumgains(targetcat='yes')
lift <- lift(targetcat='yes')
response <- response(targetcat='yes')
cumresponse <- cumresponse(targetcat='yes')
multiplot(cumgains,lift,response,cumresponse,cols=2)
