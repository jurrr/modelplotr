context("prepare_scores_and_deciles")

library(magrittr)
library(testthat)
# Generate some sample data -----------------------------------------------

data(iris)

# add some noise to iris to prevent perfect models
addNoise <- function(x) round(rnorm(n=100,mean=mean(x),sd=sd(x)),1)

iris_addnoise <- as.data.frame(lapply(iris[1:4], addNoise))

iris_addnoise$Species <- sample(unique(iris$Species),100,replace=TRUE)

iris <- rbind(iris,iris_addnoise)

train_index =  sample(seq(1, nrow(iris)),size = 0.7*nrow(iris), replace = F )
train = iris[train_index,]

test = iris[-train_index,]


# Expected answers --------------------------------------------------------

expected_cols <- c("model_label", "dataset_label", "y_true",
                   "prob_setosa", "prob_versicolor", "prob_virginica",
                   "dcl_setosa", "dcl_versicolor", "dcl_virginica")

expected_col_types <- c("factor", "factor", "factor", "numeric",
                        "numeric", "numeric", "numeric", "numeric",
                        "numeric")




# H2o tests ---------------------------------------------------------------

# Load H2o and initialise -----------------------------------------------
library(h2o)

h2o.init()
h2o.no_progress()

h2o_train <- train %>%
  as.h2o()

# Train a GBM -------------------------------------------------------------

h2o_model <- h2o.gbm(y = "Species",
                     x = setdiff(colnames(train), "Species"),
                     training_frame = h2o_train,
                     nfolds = 5)


df <- prepare_scores_and_deciles(datasets=list("train","test"),
                                 dataset_labels = list("train data","test data"),
                                 models = list("h2o_model"),
                                 model_labels = list("h2o gbm"),
                                 target_column="Species")

test_that("h2o models are properly formatted for aggregate_over_deciles", {
  df <- get("scores_and_deciles")
  col_names <- colnames(df)
  col_types <- map_chr(df, ~ class(.))

  expect_equal(colnames(df),  expected_cols)
  expect_equal(unname(col_types), expected_col_types)
})


# Test MLR support
trainTask <- mlr::makeClassifTask(data = train, target = "Species")
testTask <- mlr::makeClassifTask(data = test, target = "Species")

# Test mlr support --------------------------------------------------------

mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)

task <- mlr::makeClassifTask(data = train, target = "Species")
lrn <- mlr::makeLearner("classif.randomForest", predict.type = "prob")
rf <- mlr::train(lrn, task)


prepare_scores_and_deciles(datasets=list("train","test"),
                           dataset_labels = list("train data","test data"),
                           models = list("rf"),
                           model_labels = list("random forest"),
                           target_column="Species")

test_that("mlr models are properly formatted for aggregate_over_deciles", {

  df <- get("scores_and_deciles")
  col_names <- colnames(df)
  col_types <- map_chr(df, ~ class(.))

  expect_equal(colnames(df),  expected_cols)
  expect_equal(unname(col_types), expected_col_types)
})



# Test caret support ------------------------------------------------------

rf <- caret::train(Species ~.,data = train, method = "rf")

prepare_scores_and_deciles(datasets=list("train","test"),
                           dataset_labels = list("train data","test data"),
                           models = list("rf"),
                           model_labels = list("random forest"),
                           target_column="Species")

test_that("caret models are properly formatted for aggregate_over_deciles", {

  df <- get("scores_and_deciles")
  col_names <- colnames(df)
  col_types <- map_chr(df, ~ class(.))

  expect_equal(colnames(df),  expected_cols)
  expect_equal(unname(col_types), expected_col_types)
})

