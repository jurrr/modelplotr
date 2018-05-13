
iris_multiplotter <- function(){
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
cumgains()
lift()
response()
cumresponse()
return(multiplot(cumgains,lift,response,cumresponse,cols=2))
}


#iris_multiplotter()
