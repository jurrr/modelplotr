
# prepare iris dataset
data(iris)
colnames(iris) = c('sepal_length', 'sepal_width', 'petal_length', 'petal_width', 'species')
test_size = 0.3
train_index =  sample(seq(1, nrow(iris)),size = (1 - test_size)*nrow(iris) ,replace = F )
train = iris[train_index,]
test = iris[-train_index,]
# estimate Random Forest
clf <- randomForest::randomForest(species ~ ., data=train, importance = T)

# plot
dataprep_modevalplots(targetname="species")
input_modevalplots()
cumgains <- cumgains()
lift <- lift()
response <- response()
cumresponse <- cumresponse()
multiplot(cumgains,lift,response,cumresponse,cols=2)



?filter_

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


devtools::use_vignette("modelplotr")µ

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
