##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### prepare_scores_and_ntiles()      ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


#' Build 'scores_and_ntiles' containing Actuals, Probabilities and Ntiles
#'
#' Build dataframe object 'scores_and_ntiles' that contains actuals and predictions on
#' the target variable for each dataset in datasets and each model in models
#'
#' @section When you build scores_and_ntiles yourself:
#' To make plots with modelplotr, is not required to use this function to generate scores_and_ntiles.
#' You can create your own dataframe containing actuals and predictions and ntiles,
#' Please do check the required input for the \code{\link{aggregate_over_ntiles}} function if you
#' want to use that function to aggregate actuals and predictions
#' @param datasets List of Strings. A list of the names of the dataframe
#'   objects to include in model evaluation. All dataframes need to contain
#'   target variable and feature variables.
#' @param dataset_labels List of Strings. A list of labels for the datasets, user.
#'   When dataset_labels is not specified, the names from \code{datasets} are used.
#' @param models List of Strings. Names of the model objects containing parameters to
#'   apply models to data. To use this function, model objects need to be generated
#'   by the mlr package or by the caret package or by the h20 package.
#'   Modelplotr automatically detects whether the model is built using mlr or caret or h2o.
#' @param model_labels List of Strings. Labels for the models to use in plots.
#'   When model_labels is not specified, the names from \code{moddels} are used.
#' @param target_column String. Name of the target variable in datasets. Target
#'   can be either binary or multinomial. Continuous targets are not supported.
#' @param ntiles Integer. Number of ntiles. The ntile parameter represents the specified number
#'   of equally sized buckets the observations in each dataset are grouped into.
#'   By default, observations are grouped in 10 equally sized buckets, often referred to as deciles.
#' @return Dataframe. Dataframe \code{scores_and_ntiles} is built, based on the \code{datasets}
#'   and \code{models} specified. It contains the dataset name, actuals on the \code{target} ,
#'   the predicted probabilities for each class of the target and attribution to
#'   ntiles in the dataset for each class of the target.
#'
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' aggregates the output to the input for the plots.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
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
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' rf = caret::train(Species ~.,data = train, method = "rf")
#' mnl = caret::train(Species ~.,data = train, method = "multinom",trace = FALSE)
#' #.. or train models using h2o
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "Species",
#'                           x = setdiff(colnames(train), "Species"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' # preparation steps
#' prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm"),
#'                       model_labels = list("random forest","multinomial logit", "gradient boosting machine"),
#'                       target_column="Species")
#' head(scores_and_ntiles)
#' aggregate_over_ntiles()
#' plotting_scope()
#' plot_cumgains()
#' plot_cumlift()
#' plot_response()
#' plot_cumresponse()
#' plot_all()
#' @export
#' @importFrom magrittr %>%
prepare_scores_and_ntiles <- function(datasets,
                                  dataset_labels,
                                  models,
                                  model_labels ,
                                  target_column,
                                  ntiles=10){
  if((typeof(datasets)!='character'&typeof(datasets)!="list")|typeof(datasets[[1]])!='character') {
    stop('"datasets" should a be list with dataset names as strings! (e.g. "list("train","test")")')}
  if(missing(dataset_labels)) {
    dataset_labels = datasets
  } else if((typeof(dataset_labels)!='character'&typeof(dataset_labels)!="list")|typeof(dataset_labels[[1]])!='character') {
    stop('dataset_labels should be list with desctiption strings! (e.g. "list("train set","test set")")')}
  if((typeof(models)!='character'&typeof(models)!="list")|typeof(models[[1]])!='character') {
    stop('"models" should a be list with model object names as string!.
      \n model objects need to be generated with mlr package!')}
  if(!(ntiles%%1==0&ntiles>=4&ntiles<=100)) {
    stop('"ntiles should be an integer value between 4 and 100.')}
  if(missing(model_labels)) model_labels = models
  # create per dataset (train, test,...) a set with y, y_pred, p_y and ntl_y
  scores_and_ntiles = data.frame()
  if(typeof(target_column)!='character') {
    stop('"target_column" needs to a be a string with the name of the target variable in all datasets!')}

  for (dataset in datasets) {
    for (mdl in models) {

      # 1.0 check if specified model object exists
      if(exists(mdl)){

        # if(max(class(try((mlr::getTaskDesc(get(mdl))),TRUE)))== "try-error") {
        #   stop('model objects need to be generated with mlr package')}
        #
        # 1.1. get target class prediction from model (NOT YET DYNAMIC!) and prepare
        actuals = get(dataset) %>% dplyr::select_(y_true=target_column)
        # check if target is factor, otherwise make it a factor
        if(typeof(actuals$y_true)!='factor') actuals$y_true <- as.factor(actuals$y_true)
        #print(typeof(actuals$y_true))

        # 1.2. get probabilities per target class from model and prepare

        # 1.2.1. mlr models
        if(ifelse(is.null(class(get(mdl))), "", class(get(mdl))) == "WrappedModel") {
          cat(paste0('... scoring mlr model "',mdl,'" on dataset "',dataset,'".\n'))
          if (!requireNamespace("mlr", quietly = TRUE)) {
            stop("Package \"mlr\" needed for this function to work, but it's not installed. Please install it.",
              call. = FALSE)
          }
          mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
          # for binary targets
          if (!is.na(mlr::getTaskDesc(get(mdl))$positive)) {
              y_values <- c(mlr::getTaskDesc(get(mdl))$positive,mlr::getTaskDesc(get(mdl))$negative)
              prob_pos <- mlr::getPredictionProbabilities(predict(get(mdl),newdata=get(dataset)))
              probabilities <- data.frame(pos=prob_pos,neg=1-prob_pos)
          }
          # for multiclass targets
          else {
            probabilities <- as.data.frame(mlr::getPredictionProbabilities(predict(get(mdl),newdata=get(dataset))))
            y_values <- colnames(probabilities)
          }
        # 1.2.2. h2o models
        } else if (ifelse(is.null(attr(class(get(mdl)), "package")), "", attr(class(get(mdl)), "package")) == "h2o") {
          cat(paste0('... scoring h2o model "',mdl,'" on dataset "',dataset,'".\n'))
          if (!requireNamespace("h2o", quietly = TRUE)) {
            stop("Package \"h2o\" needed for this function to work, but it's not installed. Please install it.",
                 call. = FALSE)
          }else{
            # for binary and multiclass targets
            probabilities <- as.data.frame(h2o::h2o.predict(get(mdl),
                                                            h2o::as.h2o(get(dataset))
                                                            )
                                           )[, -1]
            y_values <- colnames(probabilities)
          }
        }
        # 1.2.3. caret models
        else {
          cat(paste0('... scoring caret model "',mdl,'" on dataset "',dataset,'".\n'))
          if (!requireNamespace("caret", quietly = TRUE)) {
            stop("Package \"caret\" needed for this function to work, but it's not installed. Please install it.",
              call. = FALSE)
          }
            probabilities <- predict(get(mdl),newdata=get(dataset),type='prob')
            y_values <- colnames(probabilities)
          }

          #name probability per target class
          colnames(probabilities) = paste0('prob_',y_values)
          y_probvars = colnames(probabilities)

          probabilities = cbind(model_label=unlist(model_labels[match(mdl,models)]),
                                dataset_label=unlist(dataset_labels[match(dataset,datasets)]),
                                actuals,
                                probabilities)

          # 1.3. calculate ntiles per target class
          for (i in 1:length(y_values)) {
            #! Added small proportion to prevent equal ntile bounds
            # and reset to 0-1 range (to prevent probs > 1.0)
            range01 <- function(x){(x-min(x))/(max(x)-min(x))}
            prob_plus_smallrandom = range01(probabilities[,y_probvars[i]]+
                runif(NROW(probabilities))/1000000)
            # determine cutoffs based on prob_plus_smallrandom
            cutoffs = c(quantile(prob_plus_smallrandom,probs = seq(0,1,1/ntiles),
                                 na.rm = TRUE))
            # add ntile variable per y-class
            probabilities[,paste0('ntl_',y_values[i])] <- (ntiles+1)-as.numeric(
              cut(prob_plus_smallrandom,breaks=cutoffs,include.lowest = T))
          }
      } else {warning(paste0('Model object \'',mdl,'\' does not exist!'))}
    scores_and_ntiles = rbind(scores_and_ntiles,probabilities)
    }
  }
  cat('Data preparation step 1 succeeded! Dataframe created.')
  return(scores_and_ntiles)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### aggregate_over_ntiles()         ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Build 'ntiles_aggregate' with aggregated evaluation measures
#'
#' Build dataframe 'ntiles_aggregate' with aggregated actuals and predictions .
#' A record in 'ntiles_aggregate' is unique on the combination of models [m], datasets [d], targetvalues [t] and ntiles.
#' The size of ntiles_aggregate is (m*d*t*10) rows and 23 columns.
#' @param scores_and_ntiles Dataframe resulting from function \code{\link{prepare_scores_and_ntiles}} or a data frame that meets
#' requirements as specified in the section below: \bold{When you build scores_and_ntiles yourself} .
#' @return Dataframe \code{ntiles_aggregate} is built based on \code{scores_and_ntiles}.\cr\cr
#' \code{ntiles_aggregate} contains:
#' \tabular{lll}{
#'   \bold{column} \tab \bold{type} \tab \bold{definition} \cr
#'   model_label \tab String \tab Name of the model object \cr
#'   dataset_label \tab Factor \tab Datasets to include in the plot as factor levels\cr
#'   target_class\tab String or Integer\tab Target classes to include in the plot\cr
#'   ntile\tab Integer\tab Ntile groups based on model probability for target class\cr
#'   neg\tab Integer\tab Number of cases not belonging to target class in dataset in ntile\cr
#'   pos\tab Integer\tab Number of cases belonging to target class in dataset in ntile\cr
#'   tot\tab Integer\tab Total number of cases in dataset in ntile\cr
#'   pct\tab Decimal \tab Percentage of cases in dataset in ntile that belongs to
#'     target class (pos/tot)\cr
#'   negtot\tab Integer\tab Total number of cases not belonging to target class in dataset\cr
#'   postot\tab Integer\tab Total number of cases belonging to target class in dataset\cr
#'   tottot\tab Integer\tab Total number of cases in dataset\cr
#'   pcttot\tab Decimal\tab Percentage of cases in dataset that belongs to
#'     target class (postot / tottot)\cr
#'   cumneg\tab Integer\tab Cumulative number of cases not belonging to target class in
#'     dataset from ntile 1 up until ntile\cr
#'   cumpos\tab Integer\tab Cumulative number of cases belonging to target class in
#'     dataset from ntile 1 up until ntile\cr
#'   cumpos\tab Integer\tab Cumulative number of cases belonging to target class in
#'     dataset from ntile 1 up until ntile\cr
#'   cumtot\tab Integer\tab Cumulative number of cases in dataset from ntile 1
#'     up until ntile\cr
#'   gain\tab Decimal\tab Gains value for dataset for ntile (pos/postot)\cr
#'   cumgain\tab Decimal\tab Cumulative gains value for dataset for ntile
#'     (cumpos/postot)\cr
#'   gain_ref\tab Decimal\tab Lower reference for gains value for dataset for ntile
#'     (ntile/#ntiles)\cr
#'   gain_opt\tab Decimal\tab Upper reference for gains value for dataset for ntile\cr
#'   lift\tab Decimal\tab Lift value for dataset for ntile (pct/pcttot)\cr
#'   cumlift\tab Decimal\tab Cumulative lift value for dataset for ntile
#'     ((cumpos/cumtot)/pcttot)\cr
#'   cumlift_ref\tab Decimal\tab Reference value for Cumulative lift value (constant: 1)
#'  }
#' @section When you build scores_and_ntiles yourself:
#' To make plots with modelplotr, is not required to use the function prepare_scores_and_ntiles to generate scores_and_ntiles.
#' You can create your own dataframe containing actuals and predictions and ntiles (1st ntile = (1/#ntiles) percent
#' with highest model probability, last ntile = (1/#ntiles) percent with lowest probability according to model) ,
#' In that case, make sure the input dataframe contains the folowing columns & formats:
#' \tabular{lll}{
#'   \bold{column} \tab \bold{type} \tab \bold{definition} \cr
#'   model_label \tab Factor \tab Name of the model object \cr
#'   dataset_label \tab Factor \tab Datasets to include in the plot as factor levels\cr
#'   y_true \tab Factor \tab Target with actual values \cr
#'   prob_[tv1] \tab Decimal \tab Probability according to model for target value 1 \cr
#'   prob_[tv2] \tab Decimal \tab Probability according to model for target value 2 \cr
#'   ... \tab ... \tab ... \cr
#'   prob_[tvn] \tab Decimal \tab Probability according to model for target value n \cr
#'   ntl_[tv1] \tab Integer \tab Ntile based on probability according to model for target value 1 \cr
#'   ntl_[tv2] \tab Integerl \tab Ntile based on probability according to model for target value 2 \cr
#'   ... \tab ... \tab ... \cr
#'   ntl_[tvn] \tab Integer \tab Ntile based on probability according to model for target value n
#'  }
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
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
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' rf = caret::train(Species ~.,data = train, method = "rf")
#' mnl = caret::train(Species ~.,data = train, method = "multinom",trace = FALSE)
#' #.. or train models using h2o
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "Species",
#'                           x = setdiff(colnames(train), "Species"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' # preparation steps
#' prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm"),
#'                       model_labels = list("random forest","multinomial logit", "gradient boosting machine"),
#'                       target_column="Species")
#' head(scores_and_ntiles)
#' aggregate_over_ntiles()
#' plotting_scope()
#' # various plotting examples with different plotting scopes
#' plot_cumgains()
#' plot_cumgains(highlight_ntile=2)
#' plotting_scope(scope="compare_models")
#' plot_cumlift()
#' plot_cumlift(highlight_ntile=2,highlight_how="plot")
#' plotting_scope(scope="compare_targetclasses")
#' plot_response()
#' plot_response(custom_line_colors = c('green','orange','darkblue'))
#' plotting_scope(scope="compare_datasets")
#' plot_cumresponse()
#' plot_cumresponse(highlight_ntile=2,highlight_how="text")
#' plot_all()
#' @export
#' @importFrom magrittr %>%
aggregate_over_ntiles <- function(prepared_input){

  # check if input is dataframe
  if(!is.data.frame(prepared_input)) {
    stop('"prepared_input" should a be a dataframe!')}

  # check if input dataframe has required input columns
  needed_colnames <- c('model_label','dataset_label','y_true')
  check_colnames <- all(needed_colnames %in% colnames(prepared_input))
  check_probcols <- sum(stringr::str_count(colnames(prepared_input),'prob_'))>=1
  check_ntlcols <- sum(stringr::str_count(colnames(prepared_input),'ntl_'))>=1
  if(!all(check_colnames,check_probcols,check_ntlcols)) {
    stop('"prepared_input" dataframe does not contain all needed columns.
Use prepare_scores_and_deciles() or see ?aggregate_over_ntiles for details how to build required input yourself.')
    }

  modelgroups = levels(prepared_input$dataset_label)
  yvals = levels(prepared_input$y_true)
  ntiles = max(prepared_input[,ncol(prepared_input)])

  ntiles_aggregate <- data.frame()


  for (val in yvals) {

    eval_t_zero = scores_and_ntiles %>%
      dplyr::mutate("target_class"=val,"ntile"=0) %>%
      dplyr::group_by_("model_label","dataset_label","target_class","ntile") %>%
      dplyr::summarize(neg=0,
                pos=0,
                tot=0,
                pct=NA,
                negtot=NA,
                postot=NA,
                tottot=NA,
                pcttot=NA,
                cumneg=0,
                cumpos=0,
                cumtot=0,
                cumpct=NA,
                gain=0,
                cumgain=0,
                gain_ref=0,
                gain_opt=0,
                lift=NA,
                cumlift=NA,
                cumlift_ref = 1) %>%
      as.data.frame()
    ifelse(ntiles_aggregate$cumtot/ntiles_aggregate$postot>1,1,ntiles_aggregate$cumtot/ntiles_aggregate$postot)
    eval_t_add = scores_and_ntiles %>%
      dplyr::mutate("target_class"=val,"ntile"=get(paste0("ntl_",val))) %>%
      dplyr::group_by_("model_label","dataset_label","target_class","ntile") %>%
      dplyr::summarize(neg=sum(y_true!=target_class),
                pos=sum(y_true==target_class),
                tot=n(),
                pct=1.0*sum(y_true==target_class)/n()) %>%
      dplyr::group_by_("model_label","dataset_label","target_class") %>%
      dplyr::mutate(negtot=sum(neg),
             postot=sum(pos),
             tottot=sum(tot),
             pcttot=1.0*sum(pos)/sum(tot)) %>%
      dplyr::group_by_("model_label","dataset_label","target_class","negtot","postot","tottot","pcttot") %>%
      dplyr::mutate(cumneg=cumsum(neg),
             cumpos=cumsum(pos),
             cumtot=cumsum(tot),
             cumpct=1.0*cumsum(pos)/cumsum(tot),
             gain=pos/postot,
             cumgain=cumsum(pos)/postot,
             gain_ref=ntile/ntiles,
             gain_opt=ifelse(cumtot/postot>1,1,cumtot/postot),
             lift=pct/pcttot,
             cumlift=1.0*cumsum(pos)/cumsum(tot)/pcttot,
             cumlift_ref = 1) %>%
      as.data.frame()


    ntiles_aggregate = rbind(ntiles_aggregate,eval_t_zero,eval_t_add)
    ntiles_aggregate = ntiles_aggregate[with(ntiles_aggregate,order(target_class,dataset_label,ntile)),]
  }
  cat('Data preparation step 2 succeeded! Dataframe created.')
  return(ntiles_aggregate)
}



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plotting_scope()         ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Build 'plot_input' with subset for selected evaluation type.
#'
#' Build dataframe 'plot_input' with a subset of 'ntiles_aggregate' that meets the requested evaluation perspective.
#' There are four perspectives:
#' \describe{
#'   \item{"no_comparison" (default)}{In this perspective, you're interested in the performance of one model on one dataset
#'     for one target class. Therefore, only one line is plotted in the plots.
#'     The parameters \code{select_model_label}, \code{select_dataset_label} and \code{select_targetclass} determine which group is
#'     plotted. When not specified, the first alphabetic model, the first alphabetic dataset and
#'     the smallest (when \code{select_smallest_targetclass=TRUE}) or first alphabetic target value are selected }
#'   \item{"compare_models"}{In this perspective, you're interested in how well different models perform in comparison to
#'     each other on the same dataset and for the same target value. This results in a comparison between models available
#'     in ntiles_aggregate$model_label for a selected dataset (default: first alphabetic dataset) and for a selected target value
#'     (default: smallest (when \code{select_smallest_targetclass=TRUE}) or first alphabetic target value).}
#'   \item{"compare_datasets"}{In this perspective, you're interested in how well a model performs in different datasets
#'   for a specific model on the same target value. This results in a comparison between datasets available in
#'   ntiles_aggregate$dataset_label for a selected model (default: first alphabetic model) and for a selected target value (default:
#'   smallest (when \code{select_smallest_targetclass=TRUE}) or first alphabetic target value).}
#'   \item{"compare_targetclasses"}{In this perspective, you're interested in how well a model performs for different target
#'    values on a specific dataset.This resuls in a comparison between target classes available in ntiles_aggregate$target_class for
#'    a selected model (default: first alphabetic model) and for a selected dataset (default: first alphabetic dataset).}}
#' @param prepared_input Dataframe. Dataframe resulting from function \code{\link{aggregate_over_ntiles}()} or with similar
#' format. Default value is ntiles_aggregate, the output of \code{aggregate_over_ntiles()}. When ntiles_aggregate is not found, function
#' \code{aggregate_over_ntiles()} is automatically called.
#' @param scope String. Evaluation type of interest. Possible values:
#' "compare_models","compare_datasets", "compare_targetclasses","no_comparison".
#' Default is NA, equivalent to "no_comparison".
#' @param select_model_label String. Selected model when scope is "compare_datasets" or "compare_targetclasses" or "no_comparison".
#' Needs to be identical to model descriptions as specified in model_labels (or models when model_labels is not specified).
#' When scope is "compare_models", select_model_label can be used to take a subset of available models.
#' @param select_dataset_label String. Selected dataset when scope is compare_models or compare_targetclasses or no_comparison.
#' Needs to be identical to dataset descriptions as specified in dataset_labels (or datasets when dataset_labels is not
#' specified). When scope is "compare_datasets", select_dataset_label can be used to take a subset of available datasets.
#' @param select_targetclass String. Selected target value when scope is compare_models or compare_datasets or no_comparison.
#' Default is smallest value when select_smallest_targetclass=TRUE, otherwise first alphabetical value.
#' When scope is "compare_targetclasses", select_targetclass can be used to take a subset of available target classes.
#' @param select_smallest_targetclass Boolean. Select the target value with the smallest number of cases in dataset as group of
#' interest. Default is True, hence the target value with the least observations is selected.
#' @return Dataframe \code{plot_input} is a subset of \code{ntiles_aggregate}.
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
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
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' rf = caret::train(Species ~.,data = train, method = "rf")
#' mnl = caret::train(Species ~.,data = train, method = "multinom",trace = FALSE)
#' #.. or train models using h2o
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "Species",
#'                           x = setdiff(colnames(train), "Species"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' # preparation steps
#' prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm"),
#'                       model_labels = list("random forest","multinomial logit", "gradient boosting machine"),
#'                       target_column="Species")
#' head(scores_and_ntiles)
#' aggregate_over_ntiles()
#' plotting_scope()
#' # various plotting examples with different plotting scopes
#' plot_cumgains()
#' plot_cumgains(highlight_ntile=2)
#' plotting_scope(scope="compare_models")
#' plot_cumlift()
#' plot_cumlift(highlight_ntile=2,highlight_how="plot")
#' plotting_scope(scope="compare_targetclasses")
#' plot_response()
#' plot_response(custom_line_colors = c('green','orange','darkblue'))
#' plotting_scope(scope="compare_datasets")
#' plot_cumresponse()
#' plot_cumresponse(highlight_ntile=2,highlight_how="text")
#' plot_all()
#' @export
#' @importFrom magrittr %>%
plotting_scope <- function(prepared_input,
                               scope="no_comparison",
                               select_model_label=NA,
                               select_dataset_label=NA,
                               select_targetclass=NA,
                               select_smallest_targetclass=TRUE){

  # check if input has required input columns
  needed_colnames <- c('model_label','dataset_label','target_class','ntile','neg','pos','tot','pct',
                               'negtot','postot','tottot','pcttot','cumneg','cumpos','cumtot','cumpct',
                               'gain','cumgain','gain_ref','gain_opt','lift','cumlift','cumlift_ref')
  if(!all(needed_colnames %in% colnames(prepared_input))) {
    # if not, check if input data has format for aggregate_over_deciles
    # check if input dataframe has required input columns
    needed_colnames <- c('model_label','dataset_label','y_true')
    check_colnames <- all(needed_colnames %in% colnames(prepared_input))
    check_probcols <- sum(stringr::str_count(colnames(prepared_input),'prob_'))>=1
    check_ntlcols <- sum(stringr::str_count(colnames(prepared_input),'ntl_'))>=1
    if(!all(check_colnames,check_probcols,check_ntlcols)) {
      stop('"prepared_input" dataframe does not contain all needed columns.
Use prepare_scores_and_deciles() or see ?aggregate_over_ntiles for details how to build required input yourself.')
    } else{
      prepared_input <- aggregate_over_ntiles(prepared_input)
      cat('"prepared_input" aggregated...\n')
    }
  }


  # check if scope has a valid value
  if (!scope %in% c(NA,"compare_models","compare_datasets", "compare_targetclasses","no_comparison")) {
    stop('invalid value for scope.
      Select "compare_models","compare_datasets", "compare_targetclasses","no_comparison" or NA')
  }

  # check if select_model_label has a valid value
  for (selmod in select_model_label) {
    if (!selmod %in% c(NA,as.character(unique(prepared_input$model_label)))) {
    stop(paste0('invalid value for select_model_label
      Select ',paste(as.character(unique(prepared_input$model_label)), collapse = ', '),' or NA'))
  }}

  # check if select_dataset_label has a valid value
  for (selds in select_dataset_label) {
    if (!selds %in% c(NA,as.character(unique(prepared_input$dataset_label)))) {
    stop(paste0('invalid value for select_dataset_label
      Select ',paste(as.character(unique(prepared_input$dataset_label)), collapse = ', '),' or NA'))
  }}

  # check if select_targetclass has a valid value
  for (seltv in select_targetclass) {
    if (!seltv %in% c(NA,as.character(unique(prepared_input$target_class)))) {
    stop(paste0('invalid value for select_targetclass
      Select ',paste(as.character(unique(prepared_input$target_class)), collapse = ', '),' or NA'))
  }}

  #check scope to decide: max 1 value of select_... required?
  #then check if needed selections of model / dataset / targetvalues are set, else set to defaults
  # no model specified? take first model based on alphabetic name.
  models <- as.character(unique(prepared_input$model_label))
  no_model_selected <- is.na(as.list(select_model_label)[1])
  if (scope=="compare_models") {
    if (no_model_selected) select_model_label <- as.list(models) else select_model_label = select_model_label
  } else {
    if (no_model_selected) select_model_label <- sort(models)[1] else select_model_label <- select_model_label[1]
  }

  # no dataset specified? take first model based on alphabetic name.
  datasets <- as.character(unique(prepared_input$dataset_label))
  no_dataset_selected <- is.na(as.list(select_dataset_label)[1])
  if (scope=="compare_datasets") {
    if (no_dataset_selected) select_dataset_label <- as.list(datasets) else select_dataset_label = select_dataset_label
  } else {
    if (no_dataset_selected) select_dataset_label <- sort(datasets)[1] else select_dataset_label <- select_dataset_label[1]
  }

  # no target value specified? take smallest targetvalue
  targetvalues <- as.character(unique(prepared_input$target_class))
  no_targetvalue_selected <- is.na(as.list(select_targetclass)[1])
  #`%>%` <- magrittr::`%>%`
  smallest <- prepared_input%>%dplyr::select(target_class,postot)%>%
      dplyr::group_by(target_class)%>%dplyr::summarize(n=min(postot,na.rm = T))%>%
      dplyr::arrange(n)%>%dplyr::top_n(n=1, -n)%>%dplyr::slice(1)%>%dplyr::select(target_class)%>%as.character()
  if (scope=="compare_targetclasses") {
    if (no_targetvalue_selected) select_targetclass <- as.list(targetvalues) else select_targetclass = select_targetclass
  } else {
    if (no_targetvalue_selected) {
      if (select_smallest_targetclass==TRUE) select_targetclass <- smallest else select_targetclass <- sort(targetvalues)[1]
      } else select_targetclass <- select_targetclass[1]
  }

  #check evaluation type and print relevant processing output
    if (scope=="compare_datasets") {
      plot_input <- prepared_input %>%
        dplyr::filter(., model_label %in% select_model_label & dataset_label %in% select_dataset_label & target_class %in% select_targetclass) %>%
        dplyr::mutate(.,legend=as.factor(dataset_label))
        datasets_print <- paste('"', select_dataset_label, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Datasets ',datasets_print,' compared for model "',
          select_model_label,'" and target value "',select_targetclass,'".'))
    } else if (scope=="compare_models") {
      plot_input <- prepared_input %>%
        dplyr::filter(., model_label %in% select_model_label & dataset_label %in% select_dataset_label & target_class %in% select_targetclass) %>%
        dplyr::mutate(.,legend=as.factor(model_label))
        models_print <- paste('"', select_model_label, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Models ',models_print,' compared for dataset "',
          select_dataset_label,'" and target value "',select_targetclass,'".'))
    } else if (scope=="compare_targetclasses") {
      plot_input <- prepared_input %>%
        dplyr::filter(., model_label %in% select_model_label & dataset_label %in% select_dataset_label & target_class %in% select_targetclass)%>%
        dplyr::mutate(.,legend=as.factor(target_class))
        targetvalues_print <- paste('"', select_targetclass, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Target classes ',targetvalues_print,' compared for dataset "',
          select_dataset_label,'" and model "',select_model_label,'".'))
    } else {
      plot_input <- prepared_input %>%
        dplyr::filter(., model_label == select_model_label & dataset_label == select_dataset_label & target_class == select_targetclass)%>%
        dplyr::mutate(.,legend=as.factor(target_class))
      type_print <- (paste0('No comparison specified, default values are used. \n
Single evaluation line will be plotted: Target value "',
        select_targetclass,'" plotted for dataset "',
        select_dataset_label,'" and model "',select_model_label,'.\n"
-> To compare models, specify: scope = "compare_models"
-> To compare datasets, specify: scope = "compare_datasets"
-> To compare target classes, specify: scope = "compare_targetclasses"
-> To plot one line, do not specify scope or specify scope = "no_comparison".'))
    }
  plot_input <- cbind(scope=scope,plot_input)
  cat(paste0('Data preparation step 3 succeeded! Dataframe created.\n\n',type_print,'\n\n'))
  return(plot_input)
}

