##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### dataprep_modevalplots()      ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


#' Build 'eval_tot' containing Actuals, Probabilities and Deciles
#'
#' Build dataframe object 'eval_tot' that contains actuals and predictions on
#' the target variable for each dataset in datasets and each model in models
#'
#' @section When you build eval_tot yourself:
#' To make plots with modelplotr, is not required to use this function to generate eval_tot.
#' You can create your own dataframe containing actuals and predictions and deciles,
#' Please do check the required input for the \code{\link{input_modevalplots}} function if you
#' want to use that function to aggregate actuals and predictions

#' @param datasets List of Strings. A list of the names of the dataframe
#'   objects to include in model evaluation. All dataframes need to contain
#'   target variable and feature variables.
#' @param datasetlabels List of Strings. A list of labels for the datasets, user.
#'   When datasetlabels is not specified, the names from \code{datasets} are used.
#' @param models List of Strings. Names of the model objects containing parameters to
#'   apply models to data. To use this function, model objects need to be generated
#'   by the mlr package.
#' @param modellabels List of Strings. Labels for the models to use in plots.
#'   When modellabels is not specified, the names from \code{moddels} are used.
#' @param targetname String. Name of the target variable in datasets. Target
#'   can be either binary or multinomial. Continuous targets are not supported.
#' @return Dataframe. Dataframe \code{eval_tot} is built, based on the \code{datasets}
#'   and \code{models} specified. It contains the dataset name, actuals on the \code{target} ,
#'   the predicted probabilities for each class of the target and attribution to
#'   deciles in the dataset for each class of the target.
#'
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{input_modevalplots}} for details on the function \code{input_modevalplots} that
#' aggregates the output to the input for the plots.
#' @seealso \code{\link{scope_modevalplots}} for details on the function \code{scope_modevalplots} that
#' filters the output of \code{input_modevalplots} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/jurrr/modelplotr} for details on the package
#' @seealso \url{https://cmotions.nl/publicaties/} for our blog on the value of the model plots
#' @examples
#' data(iris)
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
#' lift()
#' response()
#' cumresponse()
#' multiplot(cumgains(),lift(),response(),cumresponse(),cols=2)
#' @export
#' @importFrom magrittr %>%
dataprep_modevalplots <- function(datasets,
                                  datasetlabels,
                                  models,
                                  modellabels ,
                                  targetname){
  if((typeof(datasets)!='character'&typeof(datasets)!="list")|typeof(datasets[[1]])!='character') {
    stop('"datasets" should a be list with dataset names as strings! (e.g. "list("train","test")")')}
  if(missing(datasetlabels)) {
    datasetlabels = datasets
  } else if((typeof(datasetlabels)!='character'&typeof(datasetlabels)!="list")|typeof(datasetlabels[[1]])!='character') {
    stop('datasetlabels should be list with desctiption strings! (e.g. "list("train set","test set")")')}
  if((typeof(models)!='character'&typeof(models)!="list")|typeof(models[[1]])!='character') {
    stop('"models" should a be list with model object names as string!.
      \n model objects need to be generated with mlr package!')}
  if(missing(modellabels)) modellabels = models
  # create per dataset (train, test,...) a set with y, y_pred, p_y and dec_y
  eval_tot = data.frame()
  if(typeof(targetname)!='character') {
    stop('"targetname" needs to a be a string with the name of the target variable in all datasets!')}

  for (dataset in datasets) {
    for (mdl in models) {

      if(max(class(try((mlr::getTaskDesc(get(mdl))),TRUE)))== "try-error") {
        stop('model objects need to be generated with mlr package')}

      # 1.1. get category prediction from model (NOT YET DYNAMIC!) and prepare
      actuals = get(dataset) %>% dplyr::select_(y_true=targetname)

      # 1.2. get probabilities per target category from model and prepare
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

      #name probability per target category
      colnames(probabilities) = paste0('prob_',y_values)
      y_probvars = colnames(probabilities)

      probabilities = cbind(modelname=unlist(modellabels[match(mdl,models)]),
                            dataset=unlist(datasetlabels[match(dataset,datasets)]),
                            actuals,
                            probabilities)

      # 1.3. calculate deciles per target category
      for (i in 1:length(y_values)) {
        #! Added small proportion to prevent equal decile bounds
        # and reset to 0-1 range (to prevent probs > 1.0)
        range01 <- function(x){(x-min(x))/(max(x)-min(x))}
        prob_plus_smallrandom = range01(probabilities[,y_probvars[i]]+
            runif(NROW(probabilities))/1000000)
        # determine cutoffs based on prob_plus_smallrandom
        cutoffs = c(quantile(prob_plus_smallrandom,probs = seq(0,1,0.1),
                             na.rm = TRUE))
        # add decile variable per y-class
        probabilities[,paste0('dcl_',y_values[i])] <- 11-as.numeric(
          cut(prob_plus_smallrandom,breaks=cutoffs,include.lowest = T))
      }
      eval_tot = rbind(eval_tot,probabilities)
    }
  }
  eval_tot <<- eval_tot
  return('Data preparation step 1 succeeded! Dataframe \'eval_tot\' created.')
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### input_modevalplots()         ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Build 'eval_t_tot' with aggregated evaluation measures
#'
#' Build dataframe 'eval_t_tot' with aggregated actuals and predictions .
#' A record in 'eval_t_tot' is unique on the combination of models, datasets, targetvalues and deciles (m*d*t*10).
#' @param eval_tot Dataframe resulting from function \code{\link{dataprep_modevalplots}}.
#' @return Dataframe \code{eval_t_tot} is built based on \code{eval_tot}.\cr\cr
#' \code{eval_t_tot} contains:
#' \tabular{lll}{
#'   \bold{column} \tab \bold{type} \tab \bold{definition} \cr
#'   modelname \tab String \tab Name of the model object \cr
#'   dataset \tab Factor \tab Datasets to include in the plot as factor levels\cr
#'   category\tab String or Integer\tab Target values to include in the plot\cr
#'   decile\tab Integer\tab Decile groups based on model probability for category\cr
#'   neg\tab Integer\tab Number of cases not belonging to category in dataset in decile\cr
#'   pos\tab Integer\tab Number of cases belonging to category in dataset in decile\cr
#'   tot\tab Integer\tab Total number of cases in dataset in decile\cr
#'   pct\tab Decimal \tab Percentage of cases in dataset in decile that belongs to
#'     category (pos/tot)\cr
#'   negtot\tab Integer\tab Total number of cases not belonging to category in dataset\cr
#'   postot\tab Integer\tab Total number of cases belonging to category in dataset\cr
#'   tottot\tab Integer\tab Total number of cases in dataset\cr
#'   pcttot\tab Decimal\tab Percentage of cases in dataset that belongs to
#'     category (postot / tottot)\cr
#'   cumneg\tab Integer\tab Cumulative number of cases not belonging to category in
#'     dataset from decile 1 up until decile\cr
#'   cumpos\tab Integer\tab Cumulative number of cases belonging to category in
#'     dataset from decile 1 up until decile\cr
#'   cumpos\tab Integer\tab Cumulative number of cases belonging to category in
#'     dataset from decile 1 up until decile\cr
#'   cumtot\tab Integer\tab Cumulative number of cases in dataset from decile 1
#'     up until decile\cr
#'   gain\tab Decimal\tab Gains value for dataset for decile (pos/postot)\cr
#'   cumgain\tab Decimal\tab Cumulative gains value for dataset for decile
#'     (cumpos/postot)\cr
#'   gain_ref\tab Decimal\tab Lower reference for gains value for dataset for decile
#'     (decile/10)\cr
#'   gain_opt\tab Decimal\tab Upper reference for gains value for dataset for decile\cr
#'   lift\tab Decimal\tab Lift value for dataset for decile (pct/pcttot)\cr
#'   cumlift\tab Decimal\tab Cumulative lift value for dataset for decile
#'     ((cumpos/cumtot)/pcttot)\cr
#'   cumlift_ref\tab Decimal\tab Reference value for Cumulative lift value (constant: 1)
#'  }
#' @section When you build eval_tot yourself:
#' To make plots with modelplotr, is not required to use the function dataprep_modevalplots to generate eval_tot.
#' You can create your own dataframe containing actuals and predictions and deciles (decile 1= 10 percent
#' with highest model probability, 10= 10 percent with lowest probability according to model) ,
#' In that case, make sure the input dataframe contains the folowing columns & formats:
#' \tabular{lll}{
#'   \bold{column} \tab \bold{type} \tab \bold{definition} \cr
#'   modelname \tab Factor \tab Name of the model object \cr
#'   dataset \tab Factor \tab Datasets to include in the plot as factor levels\cr
#'   y_true \tab Factor \tab Target with actual values \cr
#'   prob_[tv1] \tab Decimal \tab Probability according to model for target value 1 \cr
#'   prob_[tv2] \tab Decimal \tab Probability according to model for target value 2 \cr
#'   ... \tab ... \tab ... \cr
#'   prob_[tvn] \tab Decimal \tab Probability according to model for target value n \cr
#'   dcl_[tv1] \tab Integer \tab Decile based on probability according to model for target value 1 \cr
#'   dcl_[tv2] \tab Integerl \tab Decile based on probability according to model for target value 2 \cr
#'   ... \tab ... \tab ... \cr
#'   dcl_[tvn] \tab Integer \tab Decile based on probability according to model for target value n
#'  }
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{dataprep_modevalplots}} for details on the function \code{dataprep_modevalplots}
#' that generates the required input.
#' @seealso \code{\link{scope_modevalplots}} for details on the function \code{scope_modevalplots} that
#' filters the output of \code{input_modevalplots} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/jurrr/modelplotr} for details on the package
#' @seealso \url{https://cmotions.nl/publicaties/} for our blog on the value of the model plots
#' @examples
#' data(iris)
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
#' lift()
#' response()
#' cumresponse()
#' multiplot(cumgains(),lift(),response(),cumresponse(),cols=2)
#' @export
#' @importFrom magrittr %>%
input_modevalplots <- function(prepared_input=eval_tot){

  # check if eval_tot exists, otherwise create
  if (missing(prepared_input)&!exists("eval_tot")) {
    stop("Input dataframe (similar to) eval_tot not available!
      First run dataprep_modevalplots() to generate eval_tot.")
  }
  if(!is.data.frame(prepared_input)) {
    stop('"prepared_input" should a be a dataframe!')}

  modelgroups = levels(prepared_input$dataset)
  yvals = levels(prepared_input$y_true)

  eval_t_tot <- data.frame()


  for (val in yvals) {

    eval_t_zero = eval_tot %>%
      dplyr::mutate("category"=val,"decile"=0) %>%
      dplyr::group_by_("modelname","dataset","category","decile") %>%
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
    ifelse(eval_t_tot$cumtot/eval_t_tot$postot>1,1,eval_t_tot$cumtot/eval_t_tot$postot)
    eval_t_add = eval_tot %>%
      dplyr::mutate("category"=val,"decile"=get(paste0("dcl_",val))) %>%
      dplyr::group_by_("modelname","dataset","category","decile") %>%
      dplyr::summarize(neg=sum(y_true!=category),
                pos=sum(y_true==category),
                tot=n(),
                pct=1.0*sum(y_true==category)/n()) %>%
      dplyr::group_by_("modelname","dataset","category") %>%
      dplyr::mutate(negtot=sum(neg),
             postot=sum(pos),
             tottot=sum(tot),
             pcttot=1.0*sum(pos)/sum(tot)) %>%
      dplyr::group_by_("modelname","dataset","category","negtot","postot","tottot","pcttot") %>%
      dplyr::mutate(cumneg=cumsum(neg),
             cumpos=cumsum(pos),
             cumtot=cumsum(tot),
             cumpct=1.0*cumsum(pos)/cumsum(tot),
             gain=pos/postot,
             cumgain=cumsum(pos)/postot,
             gain_ref=decile/10,
             gain_opt=ifelse(cumtot/postot>1,1,cumtot/postot),
             lift=pct/pcttot,
             cumlift=1.0*cumsum(pos)/cumsum(tot)/pcttot,
             cumlift_ref = 1) %>%
      as.data.frame()


    eval_t_tot = rbind(eval_t_tot,eval_t_zero,eval_t_add)
    eval_t_tot = eval_t_tot[with(eval_t_tot,order(category,dataset,decile)),]
  }
  eval_t_tot <<- eval_t_tot
  return('Data preparation step 2 succeeded! Dataframe \'eval_t_tot\' created.')

}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### scope_modevalplots()         ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Build 'eval_t_type' with subset for selected evaluation type.
#'
#' Build dataframe 'eval_t_type' with a subset of 'eval_t_tot' that meets the requested evaluation perspective.
#' There are four perspectives:
#' \describe{
#'   \item{"NoComparison"}{(default) In this perspective, only one line is plotted in the plots.
#'     Parameters \code{select_model}, \code{select_dataset} and \code{select_targetvalue} determine which group is
#'     plotted. When not specified, the first alphabetic model, the first alphabetic dataset and
#'     the smallest (when \code{select_smallesttargetvalue=TRUE}) or first alphabetic target value are selected }
#'   \item{"CompareModels"}{Comparison between models available in eval_t_tot$modelname for selected dataset
#'   (default: first alphabetic dataset) and for selected target value (default: smallest).}
#'   \item{"CompareDatasets"}{Comparison between datasets available in eval_t_tot$dataset for selected model
#'   (default: first alphabetic model) and for selected target value (default: smallest).}
#'   \item{"CompareTargetValues"}{Comparison between target values available in eval_t_tot$category for selected model
#'   (default: first alphabetic model) and for selected dataset (default: first alphabetic dataset).}}
#' @param prepared_input Dataframe. Dataframe resulting from function dataprep_modevalplots() or with similar output format.
#' @param eval_type String. Evaluation type of interest. Possible values:
#' "CompareModels","CompareDatasets", "CompareTargetValues","NoComparison".
#' Default is NA, equivalent to "NoComparison".
#' @param select_model String. Selected model when eval_type is CompareDatasets or CompareTargetValues or NoComparison.
#' Equivalent to model descriptions as specified in modellabels (or models when modellabels is not specified).
#' @param select_dataset String. Selected dataset when eval_type is CompareModels or CompareTargetValues or NoComparison.
#' Equivalent to dataset descriptions as specified in datasetlabels (or datasets when datasetlabels is not specified).
#' @param select_targetvalue String. Selected target value when eval_type is CompareModels or CompareDatasets or NoComparison. Default is smallest value when select_smallesttargetvalue=TRUE, otherwise first alphabetical value.
#' @param select_smallesttargetvalue Boolean. Select the target value with the smallest number of cases in dataset as group of interest. Default is True, hence the smallest group is selected.
#' @return Dataframe \code{eval_t_type} is a subset of \code{eval_t_tot}.
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{input_modevalplots}} for details on the function \code{input_modevalplots} that
#' generates the required input.
#' @seealso \code{\link{dataprep_modevalplots}} for details on the function \code{dataprep_modevalplots}
#' that generates the required input.
#' filters the output of \code{input_modevalplots} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/jurrr/modelplotr} for details on the package
#' @seealso \url{https://cmotions.nl/publicaties/} for our blog on the value of the model plots
#' @examples
#' data(iris)
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
#' lift()
#' response()
#' cumresponse()
#' multiplot(cumgains(),lift(),response(),cumresponse(),cols=2)
#' @export
#' @importFrom magrittr %>%
#eval_type <- CompareDatasets
#select_smallesttargetvalue <- TRUE
scope_modevalplots <- function(prepared_input=eval_t_tot,
                               eval_type="NoComparison",
                               select_model=NA,
                               select_dataset=NA,
                               select_targetvalue=NA,
                               select_smallesttargetvalue=TRUE){

  # check if eval_tot exists, otherwise create
  if (missing(prepared_input)&!exists("eval_tot")) {
    stop("Input dataframe (similar to) eval_tot not available!
      First run dataprep_modevalplots() to generate eval_tot.")
  }
  if(!is.data.frame(prepared_input)) {
    stop('"prepared_input" should a be a dataframe!')}


  # check if eval_tot exists, otherwise create
  if (!(exists("eval_t_tot"))) {
    print('eval_t_tot not available; input_modelevalplots() is run...')
    input_modevalplots()
  }

  # check if eval_type has a valid value
  if (!eval_type %in% c(NA,"CompareModels","CompareDatasets", "CompareTargetValues","NoComparison")) {
    stop('invalid value for eval_type.
      Select "CompareModels","CompareDatasets", "CompareTargetValues","NoComparison" or NA')
  }

  # check if select_model has a valid value
  if (!select_model %in% c(NA,as.character(unique(prepared_input$modelname)))) {
    stop(paste0('invalid value for select_model
      Select ',paste(as.character(unique(prepared_input$modelname)), collapse = ', '),' or NA'))
  }

  # check if select_dataset has a valid value
  if (!select_dataset %in% c(NA,as.character(unique(prepared_input$dataset)))) {
    stop(paste0('invalid value for select_dataset
      Select ',paste(as.character(unique(prepared_input$dataset)), collapse = ', '),' or NA'))
  }

  # check if select_targetvalue has a valid value
  if (!select_targetvalue %in% c(NA,as.character(unique(prepared_input$category)))) {
    stop(paste0('invalid value for select_targetvalue
      Select ',paste(as.character(unique(prepared_input$category)), collapse = ', '),' or NA'))
  }

  #check if needed selections of model / dataset / targetvalues are set, else set to defaults
  # no model specified? take first model based on alphabetic name.
  models <- as.character(unique(prepared_input$modelname))
  if (is.na(select_model)) {
    select_model <- sort(models)[1]
    selmodelprint <- (paste0(select_model,' (alphabetical first)'))
  }
  else selmodprint <- (paste0(select_model,' (specified by user)'))

  # no dataset specified? take first model based on alphabetic name.
  datasets <- as.character(unique(prepared_input$dataset))
  if (is.na(select_dataset)) {
    select_dataset <- sort(datasets)[1]
    seldatasetprint <- (paste0(select_dataset,' (alphabetical first)'))
  }
  else seldatasetprint <- (paste0(select_dataset,' (specified by user)'))

  # no target value specified? take smallest targetvalue
  targetvalues <- as.character(unique(prepared_input$category))
  #`%>%` <- magrittr::`%>%`
    smallest <- prepared_input%>%dplyr::select(category,postot)%>%
      dplyr::group_by(category)%>%dplyr::summarize(n=min(postot,na.rm = T))%>%
      dplyr::arrange(n)%>%dplyr::top_n(n=1, -n)%>%dplyr::select(category)%>%as.character()
  if (is.na(select_targetvalue)){
    if (select_smallesttargetvalue==TRUE) {
      select_targetvalue <- smallest
      seltargetvalueprint <- (paste0(select_targetvalue,' (smallest group)'))
    }
    else {
      select_targetvalue <- sort(targetvalues)[1]
      seltargetvalueprint <- (paste0(select_targetvalue,' (alphabetical first)'))
    }
  }
  else  seltargetvalueprint <- (paste0(select_targetvalue,' (specified by user)'))


  #check evaluation type and print relevant processing output
    if (eval_type=="CompareDatasets") {
      eval_t_type <- prepared_input %>%
        dplyr::filter(., modelname == select_model & category == select_targetvalue) %>%
        dplyr::mutate(.,legend=as.factor(dataset))
        datasets_print <- paste('"', datasets, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Datasets ',datasets_print,' compared for model "',
          select_model,'" and target value "',select_targetvalue,'".'))
    } else if (eval_type=="CompareModels") {
      eval_t_type <- prepared_input %>%
        dplyr::filter(., dataset == select_dataset & category == select_targetvalue) %>%
        dplyr::mutate(.,legend=as.factor(modelname))
        models_print <- paste('"', models, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Models ',models_print,' compared for dataset "',
          select_dataset,'" and target value "',select_targetvalue,'".'))
    } else if (eval_type=="CompareTargetValues") {
      eval_t_type <- prepared_input %>%
        dplyr::filter(., modelname == select_model & dataset == select_dataset)%>%
        dplyr::mutate(.,legend=as.factor(category))
        targetvalues_print <- paste('"', targetvalues, '"', sep = "", collapse = ", ")
        type_print <- (paste0('Target values ',targetvalues_print,' compared for dataset "',
          select_dataset,'" and model "',select_model,'".'))
    } else {
      eval_t_type <- prepared_input %>%
        dplyr::filter(., modelname == select_model & dataset == select_dataset & category == select_targetvalue)%>%
        dplyr::mutate(.,legend=as.factor(category))
      type_print <- (paste0('No comparison specified! Single evaluation line will be plotted: \n Target value "',
        select_targetvalue,'" plotted for dataset "',
        select_dataset,'" and model "',select_model,'."
  To compare models, specify: eval_type = "CompareModels"
  To compare datasets, specify: eval_type = "CompareDatasets"
  To compare target values, specify: eval_type = "CompareTargetValues"
  To plot one line, do not specify eval_type or specify eval_type = "NoComparison".'))
    }
  eval_t_type <<- cbind(eval_type=eval_type,
                        eval_t_type)
  cat(paste0('Data preparation step 3 succeeded! Dataframe \'eval_t_type\' created.\n\n',type_print))
}

