.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package modelplotr loaded! Happy model plotting!")
}


#' modelplotr: Plots to Evaluate the Business Performance of Predictive Models.
#'
#' Plots to evaluate the business performance of predictive models in R.
#' A number of widely used plots to assess the quality of a predictive model from a business perspective
#' can easily be created. Using these plots, it can be shown how implementation of the model will impact
#' business targets like response on a campaign or return on investment. It's very easy to apply modelplotr
#' to predictive models that are developed in caret, mlr, h2o or keras. For other models, even those built
#' outside of R, an instruction is included.
#' The modelplotr package provides three categories of important functions:
#' datapreparation, plot parameterization and plotting.
#'
#' @author Jurriaan Nagelkerke <jurriaan.nagelkerke@@gmail.com> [aut, cre]
#' @author Pieter Marcus <pieter.marcus@@persgroep.net> [aut]
#'
#' @section Datapreparation functions:
#'  The datapreparation functions are:
#' \describe{
#'   \item{\code{\link{prepare_scores_and_ntiles}}}{Function that builds a dataframe
#'   that contains actuals and predictions on the target variable for each dataset in \code{datasets} and each model in \code{models}.
#'   As inputs, it takes dataframes to score and model objects created with \strong{caret}, \strong{mlr}, \strong{h2o} or \strong{keras}.
#'   To use modelplotr on top of models created otherwise, even models built outside r, see \code{\link{aggregate_over_ntiles}}}
#'   \item{\code{\link{plotting_scope}}}{Function that creates a dataframe in the required format for all
#'   modelplotr plots, relevant to the selected scope of evaluation. Each record in this dataframe represents
#'   a unique combination of datasets, models, target classes and ntiles. As an input, plotting_scope can handle
#'   both a dataframe created with \code{aggregate_over_ntiles} as well as a dataframe created with
#'   \code{prepare_scores_and_ntiles} (or created otherwise, with similar layout). }
#'   \item{\code{\link{aggregate_over_ntiles}}}{Function that aggregates the output of \code{prepare_scores_and_ntiles}
#'   to create a dataframe with aggregated actuals and predictions. Each record in this dataframe represents
#'   a unique combination of datasets, models, target classes and ntiles. In most cases, you do not need to use function
#'   since the \code{plotting_scope} function will call this function automatically. }}
#' @section Parameterization functions:
#'  Most parameterization functions are internal functions. However, one is available for customization:
#' \describe{
#'   \item{\code{\link{customize_plot_text}}}{Function that returns a list that contains all textual elements for
#'   all plots that modelplotr can create. By changing the elements in this list - simply by overwriting values -
#'   and then including this list with the \code{custom_plot_text} parameter in plot functions, plot texts can easily be customized
#'   to meet your (language) preferences}}
#' @section Plotting functions:
#'   The plotting functions are:
#' \describe{
#'   \item{\code{\link{plot_cumgains}}}{Generates the cumulative gains plot. This plot, often referred to as the gains chart,
#'     helps answering the question: \strong{\emph{When we apply the model and select the best X ntiles,
#'     what percentage of the actual target class observations can we expect to target?}} }
#'     \item{\code{\link{plot_cumlift}}}{Generates the cumulative lift plot, often referred to as lift plot or index plot,
#'     helps you answer the question: \strong{\emph{When we apply the model and select the best X ntiles,
#'     how many times better is that than using no model at all?}}}
#'     \item{\code{\link{plot_response}}}{Generates the response plot. It plots the percentage of target class observations
#'     per ntile. It can be used to answer the following business question: \strong{\emph{When we apply
#'     the model and select ntile X, what is the expected percentage of target class observations
#'     in that ntile?}}}
#'     \item{\code{\link{plot_cumresponse}}}{Generates the cumulative response plot. It plots the cumulative percentage of
#'      target class observations up until that ntile. It helps answering the question:
#'      \strong{\emph{When we apply the model and select up until ntile X, what is the expected percentage of
#'      target class observations in the selection? }}}
#'     \item{\code{\link{plot_multiplot}}}{Generates a canvas with all four evaluation plots - cumulative gains, cumulative lift,
#'     response and cumulative response - combined on one canvas}
#'     \item{\code{\link{plot_costsrevs}}}{It plots the cumulative costs and revenues up until that ntile when the model
#'     is used for campaign selection. It can be used to answer the following business question:
#'      \strong{\emph{When we apply the model and select up until ntile X, what are the expected costs and
#'      revenues of the campaign?}}}
#'     \item{\code{\link{plot_profit}}}{Generates the Profit plot. It plots the cumulative profit up until that ntile when the
#'     model is used for campaign selection. It can be used to answer the following business question:
#'      \strong{\emph{When we apply the model and select up until ntile X, what is the expected profit of the campaign?}}}
#'     \item{\code{\link{plot_roi}}}{Generates the Return on Investment plot. It plots the cumulative revenues as a percentage
#'     of investments up until that ntile when the model is used for campaign selection. It can be used to answer the following
#'     business question: \strong{\emph{When we apply the model and select up until ntile X, what is the expected % return on
#'     investment of the campaign?}}}
#'     }
#'
#' @seealso \code{vignette('modelplotr')}
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog posts on using modelplotr
#' @examples
#' # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
#' data("bank_td")
#'
#' # prepare data for training model for binomial target has_td and train models
#' train_index =  sample(seq(1, nrow(bank_td)),size = 0.5*nrow(bank_td) ,replace = FALSE)
#' train = bank_td[train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#' test = bank_td[-train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#'
#' #train models using caret... (or use mlr or H2o or keras ... see ?prepare_scores_and_ntiles)
#' # setting caret cross validation, here tuned for speed (not accuracy!)
#' fitControl <- caret::trainControl(method = "cv",number = 2,classProbs=TRUE)
#' # random forest using ranger package, here tuned for speed (not accuracy!)
#' rf = caret::train(has_td ~.,data = train, method = "ranger",trControl = fitControl,
#'                   tuneGrid = expand.grid(.mtry = 2,.splitrule = "gini",.min.node.size=10))
#' # mnl model using glmnet package
#' mnl = caret::train(has_td ~.,data = train, method = "glmnet",trControl = fitControl)
#'
#' # load modelplotr
#' library(modelplotr)
#'
#' # transform datasets and model objects to input for modelplotr
#' scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train","test"),
#'                          dataset_labels = list("train data","test data"),
#'                          models = list("rf","mnl"),
#'                          model_labels = list("random forest","multinomial logit"),
#'                          target_column="has_td",
#'                          ntiles=100)
#'
#' # set scope for analysis (default: no comparison)
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles)
#' head(plot_input)
#'
#' # ALL PLOTS, with defaults
#' plot_cumgains(data=plot_input)
#' plot_cumlift(data=plot_input)
#' plot_response(data=plot_input)
#' plot_cumresponse(data=plot_input)
#' plot_multiplot(data=plot_input)
#' # financial plots - these need some financial parameters
#' plot_costsrevs(data=plot_input,fixed_costs=15000,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_profit(data=plot_input,fixed_costs=15000,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_roi(data=plot_input,fixed_costs=15000,variable_costs_per_unit=10,profit_per_unit=50)
#'
#' # CHANGING THE SCOPE OF ANALYSIS
#' # changing the scope - compare models:
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_models")
#' plot_cumgains(data=plot_input)
#' # changing the scope - compare datasets:
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_datasets")
#' plot_roi(data = plot_input,fixed_costs=15000,variable_costs_per_unit=10,profit_per_unit=50)
#' # changing the scope - compare target classes:
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope="compare_targetclasses")
#' plot_response(data=plot_input)
# '
#' # HIGHLIGHTING OPTIONS
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,
#'                              scope = 'compare_datasets',select_model_label = 'random forest')
#' plot_cumgains(data=plot_input,highlight_ntile=20)
#' plot_cumlift(data=plot_input,highlight_ntile=20,highlight_how = 'plot')
#' plot_response(data=plot_input,highlight_ntile=20,highlight_how = 'text')
#' plot_cumresponse(data=plot_input,highlight_ntile=20,highlight_how = 'plot_text')
#' plot_costsrevs(data=plot_input,fixed_costs = 15000,variable_costs_per_unit = 10,
#'                profit_per_unit = 50,highlight_ntile='max_roi')
#' plot_profit(data=plot_input,fixed_costs = 15000,variable_costs_per_unit = 10,profit_per_unit = 50)
#' plot_roi(data=plot_input,fixed_costs = 15000,variable_costs_per_unit = 10,profit_per_unit = 50)
#'
#' # OTHER PLOT CUSTOMIZATIONS
#' # customize line colors
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope = 'compare_models')
#' plot_cumgains(data=plot_input,custom_line_colors = c('pink','navyblue'))
#' # customize all textual elements of plots
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles)
#' mytexts <- customize_plot_text(plot_input = plot_input)
#' mytexts$cumresponse$plottitle <- 'Expected conversion rate for Campaign XYZ'
#' mytexts$cumresponse$plotsubtitle <- 'proposed selection: best 15 percentiles according to our model'
#' mytexts$cumresponse$y_axis_label <- '% Conversion'
#' mytexts$cumresponse$x_axis_label <- 'percentiles (percentile = 1% of customers)'
#' mytexts$cumresponse$annotationtext <-
#' "Selecting up until the &NTL percentile with model &MDL has an expected conversion rate of &VALUE"
#' plot_cumresponse(data=plot_input,custom_plot_text = mytexts,highlight_ntile = 15)
#' @docType package
#' @name modelplotr
NULL
