.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package modelplotr loaded! Happy model plotting!")
}


#' modelplotr: Plots to Evaluate the Business Performance of Predictive Models.
#'
#' The modelplotr package provides two categories of important functions:
#' datapreparation and plotting.
#'
#' @author Jurriaan Nagelkerke <jurriaan.nagelkerke@@gmail.com> [aut, cre]
#' @author Pieter Marcus <pieter.marcus@@persgroep.net> [aut]
#'
#' @section Datapreparation functions:
#'  The datapreparation functions are:
#' \describe{
#'   \item{\code{\link{dataprep_modevalplots}}}{a
#'   function that builds a dataframe object \code{'eval_tot'} that contains
#'   actuals and predictions on dependent variable for each dataset in datasets.}
#'   \item{\code{\link{input_modevalplots}}}{a function that creates a dataframe \code{'eval_t_tot'} with aggregated
#'     actuals and predictions. A record in 'eval_t_tot' is unique on the combination
#'     of datasets-decile.}
#'   \item{\code{\link{scope_modevalplots}}}{a function that creates a dataframe \code{'eval_t_type'}  with a subset
#'     of 'eval_t_tot', relevant to the selected scope of evaluation. }}
#' @section Plotting functions:
#'   The plotting functions are:
#' \describe{
#'   \item{\code{\link{cumgains}}}{Generates the cumulative gains plot. This plot, often referred to as the gains chart,
#'     helps answering the question: When we apply the model and select the best X deciles,
#'     what percentage of the actual target class observations can we expect to target? }
#'     \item{\code{\link{cumlift}}}{Generates the cumulative lift plot, often referred to as lift plot or index plot,
#'     helps you answer the question: When we apply the model and select the best X deciles,
#'     how many times better is that than using no model at all?}
#'     \item{\code{\link{response}}}{Generates the response plot. It plots the percentage of target class observations
#'     per decile. It can be used to answer the following business question: When we apply
#'     the model and select decile X, what is the expected percentage of target class observations
#'     in that decile?}
#'     \item{\code{\link{cumresponse}}}{Generates the cumulative response plot. It plots the cumulative percentage of
#'      target class observations up until that decile. It helps answering the question:
#'      When we apply the model and select up until decile X, what is the expected percentage of
#'      target class observations in the selection? }
#'     \item{\code{\link{fourevalplots}}}{Generates a canvas with all four evaluation plots combined}}
#'
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog posts on using modelplotr
#' @examples
#' data(iris)
#' # add some noise to iris to prevent perfect models
#' addNoise <- function(x) round(rnorm(n=100,mean=mean(x),sd=sd(x)),1)
#' iris_addnoise <- as.data.frame(lapply(iris[1:4], addNoise))
#' iris_addnoise$Species <- sample(unique(iris$Species),100,replace=TRUE)
#' iris <- rbind(iris,iris_addnoise)
#' train_index =  sample(seq(1, nrow(iris)),size = 0.7*nrow(iris), replace = F )
#' train = iris[train_index,]
#' test = iris[-train_index,]
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' dataprep_modevalplots(datasets=list("train","test"),
#'                       datasetlabels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       modellabels = list("random forest","multinomial logit"),
#'                       targetname="Species")
#' head(eval_tot)
#' input_modevalplots()
#' scope_modevalplots()
#' cumgains()
#' cumlift()
#' response()
#' cumresponse()
#' fourevalplots()
#'
#' @docType package
#' @name modelplotr
NULL
