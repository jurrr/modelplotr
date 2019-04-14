##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### prepare_scores_and_ntiles()      ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


#' Build a dataframe containing Actuals, Probabilities and Ntiles
#'
#' Build dataframe object that contains actuals and predictions on
#' the target variable for each dataset in \code{datasets} and each model in \code{models}
#'
#' @param datasets List of Strings. A list of the names of the dataframe
#'   objects to include in model evaluation. All dataframes need to contain a
#'   target variable and feature variables.
#' @param dataset_labels List of Strings. A list of labels for the datasets, shown in plots.
#'   When dataset_labels is not specified, the names from \code{datasets} are used.
#' @param models List of Strings. List of the names of the model objects, containing parameters to
#'   apply models to datasets. To use this function, model objects need to be generated
#'   by the mlr package or the caret package or the h20 package or the keras package.
#'   Modelplotr automatically detects whether the model is built using mlr or caret or h2o or keras.
#' @param model_labels List of Strings. Labels for the models, shown in plots.
#'   When model_labels is not specified, the names from \code{moddels} are used.
#' @param target_column String. Name of the target variable in datasets. Target
#'   can be either binary or multinomial. Continuous targets are not supported.
#' @param ntiles Integer. Number of ntiles. The ntile parameter represents the specified number
#'   of equally sized buckets the observations in each dataset are grouped into.
#'   By default, observations are grouped in 10 equally sized buckets, often referred to as deciles.
#' @return Dataframe. A dataframe is built, based on the \code{datasets}
#'   and \code{models} specified. It contains the dataset name, actuals on the \code{target_column} ,
#'   the predicted probabilities for each target class (eg. unique target value) and attribution to
#'   ntiles in the dataset for each target class.
#'
#' @section When you build scores_and_ntiles yourself:
#' To make plots with modelplotr, is not required to use this function to generate input for function \code{plotting_scope}
#' You can create your own dataframe containing actuals and predictions and ntiles,
#' See \code{\link{build_input_yourself}} for an example to build the required input for \code{\link{plotting_scope}}
#' or \code{\link{aggregate_over_ntiles}} yourself, within r or even outside of r.
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{vignette('modelplotr')}
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' transforms a dataframe created with  \code{prepare_scores_and_ntiles} or \code{aggregate_over_ntiles} to
#' a dataframe in the required format for all modelplotr plots.
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' aggregates the output of \code{prepare_scores_and_ntiles} to create a dataframe with aggregated actuals and predictions.
#' In most cases, you do not need to use it since the \code{plotting_scope} function will call this function automatically.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
#' @examples
#' \dontrun{
#' # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
#' data("bank_td")
#'
#' # prepare data for training model for binomial target has_td and train models
#' train_index =  sample(seq(1, nrow(bank_td)),size = 0.5*nrow(bank_td) ,replace = FALSE)
#' train = bank_td[train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#' test = bank_td[-train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#'
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "has_td")
#' testTask <- mlr::makeClassifTask(data = test, target = "has_td")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "has_td")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' # setting caret cross validation, here tuned for speed (not accuracy!)
#' fitControl <- caret::trainControl(method = "cv",number = 2,classProbs=TRUE)
#' # random forest using ranger package, here tuned for speed (not accuracy!)
#' rf = caret::train(has_td ~.,data = train, method = "ranger",trControl = fitControl,
#'                   tuneGrid = expand.grid(.mtry = 2,.splitrule = "gini",.min.node.size=10))
#' # mnl model using glmnet package
#' mnl = caret::train(has_td ~.,data = train, method = "glmnet",trControl = fitControl)
#' #... or train models using h2o...
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "has_td",
#'                           x = setdiff(colnames(train), "has_td"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' #... or train models using keras.
#' x_train <- as.matrix(train[,-1]); y=train[,1]; y_train <- keras::to_categorical(as.numeric(y)-1);
#' `%>%` <- magrittr::`%>%`
#' nn <- keras::keras_model_sequential() %>%
#' keras::layer_dense(units = 16,kernel_initializer = "uniform",activation = 'relu',
#'                    input_shape = NCOL(x_train))%>%
#'   keras::layer_dense(units = 16,kernel_initializer = "uniform", activation='relu') %>%
#'   keras::layer_dense(units = length(levels(train[,1])),activation='softmax')
#' nn %>% keras::compile(optimizer='rmsprop',loss='categorical_crossentropy',metrics=c('accuracy'))
#' nn %>% keras::fit(x_train,y_train,epochs = 20,batch_size = 1028,verbose=0)
#'
#' # preparation steps
#' scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm","nn"),
#'                       model_labels = list("random forest","multinomial logit",
#'                                           "gradient boosting machine","artificial neural network"),
#'                       target_column="has_td")
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles)
#' plot_cumgains(data = plot_input)
#' plot_cumlift(data = plot_input)
#' plot_response(data = plot_input)
#' plot_cumresponse(data = plot_input)
#' plot_multiplot(data = plot_input)
#' plot_costsrevs(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_profit(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_roi(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' }
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
        # 1.1. get target class prediction from model and prepare
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
              prob_pos <- mlr::getPredictionProbabilities(stats::predict(get(mdl),newdata=get(dataset)))
              probabilities <- data.frame(pos=prob_pos,neg=1-prob_pos)
          }
          # for multiclass targets
          else {
            probabilities <- as.data.frame(mlr::getPredictionProbabilities(stats::predict(get(mdl),newdata=get(dataset))))
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
        # 1.2.3. keras models
        else if (max(grepl('keras',class(get(mdl)))) == 1) {
        cat(paste0('... scoring keras model "',mdl,'" on dataset "',dataset,'".\n'))
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package \"keras\" needed for this function to work, but it's not installed. Please install it.",
               call. = FALSE)
        }else{
          # for binary targets
          if (length(levels(actuals$y_true)) == 2) {
              varnames = setdiff(colnames(get(dataset)), target_column)
              probabilities <- as.data.frame(stats::predict(get(mdl),as.matrix(get(dataset)[,varnames]),verbose=0))
              probabilities[,2] <- 1-probabilities[,1]
              y_values <- levels(actuals$y_true)
          }
          # for multiclass targets
          else {
            varnames = setdiff(colnames(get(dataset)), target_column)
            probabilities <- as.data.frame(stats::predict(get(mdl),as.matrix(get(dataset)[,varnames]),verbose=0))
            y_values <- levels(actuals$y_true)
          }
          }
        }
          # 1.2.4. caret models
        else {
          cat(paste0('... scoring caret model "',mdl,'" on dataset "',dataset,'".\n'))
          if (!requireNamespace("caret", quietly = TRUE)) {
            stop("Package \"caret\" needed for this function to work, but it's not installed. Please install it.",
              call. = FALSE)
          }
            probabilities <- stats::predict(get(mdl),newdata=get(dataset),type='prob')
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
              stats::runif(NROW(probabilities))/1000000)
          # determine cutoffs based on prob_plus_smallrandom
          cutoffs = c(stats::quantile(prob_plus_smallrandom,probs = seq(0,1,1/ntiles),
                               na.rm = TRUE))
          # add ntile variable per y-class
          probabilities[,paste0('ntl_',y_values[i])] <- (ntiles+1)-as.numeric(
            cut(prob_plus_smallrandom,breaks=cutoffs,include.lowest = TRUE))
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

#' Build a dataframe with aggregated evaluation measures
#'
#' Build a dataframe with aggregated actuals and predictions.
#' Records in this dataframe represent the unique combinations of models [m], datasets [d], targetvalues [t] and ntiles [n].
#' The size of this dataframe therefore is (m*d*t*n) rows and 23 columns. \cr\cr \bold{\emph{In most cases, you do not need to use function
#'   since the \code{\link{plotting_scope}} function will call this function automatically.}}
#' @param prepared_input Dataframe resulting from function \code{\link{prepare_scores_and_ntiles}} or a data frame that meets
#' requirements as specified in the section below: \bold{When you build input for aggregate_over_ntiles() yourself} .
#' @return Dataframe object is returned, containing:
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
#'   cumtot\tab Integer\tab Cumulative number of cases in dataset from ntile 1
#'     up until ntile\cr
#'   cumpct\tab Integer\tab Cumulative percentage of cases belonging to target class in
#'     dataset from ntile 1 up until ntile (cumpos/cumtot)\cr
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
#' @section When you build input for aggregate_over_ntiles() yourself:
#' To make plots with modelplotr, is not required to use the function prepare_scores_and_ntiles to generate the required input data.
#' You can create your own dataframe containing actuals and probabilities and ntiles (1st ntile = (1/#ntiles) percent
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
#' See \code{\link{build_input_yourself}} for an example to build the required input yourself.
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{vignette('modelplotr')}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \code{\link{build_input_yourself}} for an example to build the required input yourself.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
#' @examples
#' \dontrun{
#' # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
#' data("bank_td")
#'
#' # prepare data for training model for binomial target has_td and train models
#' train_index =  sample(seq(1, nrow(bank_td)),size = 0.5*nrow(bank_td) ,replace = FALSE)
#' train = bank_td[train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#' test = bank_td[-train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#'
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "has_td")
#' testTask <- mlr::makeClassifTask(data = test, target = "has_td")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "has_td")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' # setting caret cross validation, here tuned for speed (not accuracy!)
#' fitControl <- caret::trainControl(method = "cv",number = 2,classProbs=TRUE)
#' # random forest using ranger package, here tuned for speed (not accuracy!)
#' rf = caret::train(has_td ~.,data = train, method = "ranger",trControl = fitControl,
#'                   tuneGrid = expand.grid(.mtry = 2,.splitrule = "gini",.min.node.size=10))
#' # mnl model using glmnet package
#' mnl = caret::train(has_td ~.,data = train, method = "glmnet",trControl = fitControl)
#' #... or train models using h2o...
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "has_td",
#'                           x = setdiff(colnames(train), "has_td"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' #... or train models using keras.
#' x_train <- as.matrix(train[,-1]); y=train[,1]; y_train <- keras::to_categorical(as.numeric(y)-1);
#' `%>%` <- magrittr::`%>%`
#' nn <- keras::keras_model_sequential() %>%
#' keras::layer_dense(units = 16,kernel_initializer = "uniform",activation = 'relu',
#'                    input_shape = NCOL(x_train))%>%
#'   keras::layer_dense(units = 16,kernel_initializer = "uniform", activation='relu') %>%
#'   keras::layer_dense(units = length(levels(train[,1])),activation='softmax')
#' nn %>% keras::compile(optimizer='rmsprop',loss='categorical_crossentropy',metrics=c('accuracy'))
#' nn %>% keras::fit(x_train,y_train,epochs = 20,batch_size = 1028,verbose=0)
#'
#' # preparation steps
#' scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm","nn"),
#'                       model_labels = list("random forest","multinomial logit",
#'                                           "gradient boosting machine","artificial neural network"),
#'                       target_column="has_td")
#' aggregated <- aggregate_over_ntiles(prepared_input=scores_and_ntiles)
#' head(aggregated)
#' plot_input <- plotting_scope(prepared_input = aggregated)
#' head(plot_input)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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

    eval_t_zero = prepared_input %>%
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
    eval_t_add = prepared_input %>%
      dplyr::mutate("target_class"=val,"ntile"=get(paste0("ntl_",val))) %>%
      dplyr::group_by_("model_label","dataset_label","target_class","ntile") %>%
      dplyr::summarize(neg=sum(.data$y_true!=.data$target_class),
                pos=sum(.data$y_true==.data$target_class),
                tot=dplyr::n(),
                pct=1.0*sum(.data$y_true==.data$target_class)/dplyr::n()) %>%
      dplyr::group_by_("model_label","dataset_label","target_class") %>%
      dplyr::mutate(negtot=sum(.data$neg),
             postot=sum(.data$pos),
             tottot=sum(.data$tot),
             pcttot=1.0*sum(.data$pos)/sum(.data$tot)) %>%
      dplyr::group_by_("model_label","dataset_label","target_class","negtot","postot","tottot","pcttot") %>%
      dplyr::mutate(cumneg=cumsum(.data$neg),
             cumpos=cumsum(.data$pos),
             cumtot=cumsum(.data$tot),
             cumpct=1.0*cumsum(.data$pos)/cumsum(.data$tot),
             gain=.data$pos/.data$postot,
             cumgain=cumsum(.data$pos)/.data$postot,
             gain_ref=.data$ntile/ntiles,
             gain_opt=ifelse(.data$cumtot/.data$postot>1,1,.data$cumtot/.data$postot),
             lift=.data$pct/.data$pcttot,
             cumlift=1.0*cumsum(.data$pos)/cumsum(.data$tot)/.data$pcttot,
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

#' Build dataframe with formatted input for all plots.
#'
#' Build a dataframe in the required format for all modelplotr plots, relevant to the selected scope of evaluation.
#' Each record in this dataframe represents a unique combination of datasets, models, target classes and ntiles.
#' As an input, plotting_scope can handle both a dataframe created with \code{aggregate_over_ntiles} as well as a dataframe
#' created with \code{prepare_scores_and_ntiles} (or created otherwise with similar layout).
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
#' @param prepared_input Dataframe. Dataframe created with \code{\link{prepare_scores_and_ntiles}} or dataframe created with
#' \code{\link{aggregate_over_ntiles}} or a dataframe that is created otherwise with similar layout as the output of these functions
#' (see ?prepare_scores_and_ntiles and ?aggregate_over_ntiles for layout details).
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
#' @section When you build input for plotting_scope() yourself:
#' To make plots with modelplotr, is not required to use the function prepare_scores_and_ntiles to generate the required input data.
#' You can create your own dataframe containing actuals and probabilities and ntiles (1st ntile = (1/#ntiles) percent
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
#' See \link{build_input_yourself} for an example to build the required input yourself.
#' @return Dataframe \code{plot_input} is a subset of \code{ntiles_aggregate}.
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{vignette('modelplotr')}
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{build_input_yourself}} for an example to build the required input yourself.
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
#' @examples
#' \dontrun{
#' # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
#' data("bank_td")
#'
#' # prepare data for training model for binomial target has_td and train models
#' train_index =  sample(seq(1, nrow(bank_td)),size = 0.5*nrow(bank_td) ,replace = FALSE)
#' train = bank_td[train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#' test = bank_td[-train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#'
#' #train models using mlr...
#' trainTask <- mlr::makeClassifTask(data = train, target = "has_td")
#' testTask <- mlr::makeClassifTask(data = test, target = "has_td")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' task = mlr::makeClassifTask(data = train, target = "has_td")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' #... or train models using caret...
#' # setting caret cross validation, here tuned for speed (not accuracy!)
#' fitControl <- caret::trainControl(method = "cv",number = 2,classProbs=TRUE)
#' # random forest using ranger package, here tuned for speed (not accuracy!)
#' rf = caret::train(has_td ~.,data = train, method = "ranger",trControl = fitControl,
#'                   tuneGrid = expand.grid(.mtry = 2,.splitrule = "gini",.min.node.size=10))
#' # mnl model using glmnet package
#' mnl = caret::train(has_td ~.,data = train, method = "glmnet",trControl = fitControl)
#' #... or train models using h2o...
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' h2o_train = h2o::as.h2o(train)
#' h2o_test = h2o::as.h2o(test)
#' gbm <- h2o::h2o.gbm(y = "has_td",
#'                           x = setdiff(colnames(train), "has_td"),
#'                           training_frame = h2o_train,
#'                           nfolds = 5)
#' #... or train models using keras.
#' x_train <- as.matrix(train[,-1]); y=train[,1]; y_train <- keras::to_categorical(as.numeric(y)-1)
#' `%>%` <- magrittr::`%>%`
#' nn <- keras::keras_model_sequential() %>%
#' keras::layer_dense(units = 16,kernel_initializer = "uniform",activation = 'relu',
#'                    input_shape = NCOL(x_train))%>%
#'   keras::layer_dense(units=16,kernel_initializer="uniform",activation='relu') %>%
#'   keras::layer_dense(units=length(levels(train[,1])),activation='softmax')
#' nn %>% keras::compile(optimizer='rmsprop',loss='categorical_crossentropy',metrics=c('accuracy'))
#' nn %>% keras::fit(x_train,y_train,epochs = 20,batch_size = 1028,verbose=0)
#'
#' # preparation steps
#' scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl", "gbm","nn"),
#'                       model_labels = list("random forest","multinomial logit",
#'                                           "gradient boosting machine","artificial neural network"),
#'                       target_column="has_td")
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles)
#' plot_cumgains(data = plot_input)
#' plot_cumlift(data = plot_input)
#' plot_response(data = plot_input)
#' plot_cumresponse(data = plot_input)
#' plot_multiplot(data = plot_input)
#' plot_costsrevs(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_profit(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' plot_roi(data=plot_input,fixed_costs=1500,variable_costs_per_unit=10,profit_per_unit=50)
#' }
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
      dplyr::group_by(target_class)%>%dplyr::summarize(n=min(postot,na.rm = TRUE))%>%
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


#' Example: build required input from a custom model
#'
#' It's very easy to apply modelplotr
#' to predictive models that are developed in caret, mlr, h2o or keras. However, also for models that are developed differently,
#' even those built outside of R, it only takes a bit more work to use modelplotr on top of these models.
#' In this section we introduce the required format and an example.
#'
#' @section When you build input for plotting_scope() yourself:
#' To make plots with modelplotr, is not required to use the function prepare_scores_and_ntiles to generate the required input data.
#' You can create your own dataframe containing actuals and probabilities and ntiles (1st ntile = (1/#ntiles) percent
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
#' @examples
#' # load example data (Bank clients with/without a term deposit - see ?bank_td for details)
#' data("bank_td")
#' library(dplyr)
#' # prepare data for training model for binomial target has_td and train models
#' train_index =  sample(seq(1, nrow(bank_td)),size = 0.5*nrow(bank_td) ,replace = FALSE)
#' train = bank_td[train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#' test = bank_td[-train_index,c('has_td','duration','campaign','pdays','previous','euribor3m')]
#'
#' #train logistic regression model with stats package
#' glm.model <- glm(has_td ~.,family=binomial(link='logit'),data=train)
#' #score model
#' prob_no.term.deposit <- stats::predict(glm.model,newdata=train,type='response')
#' prob_term.deposit <- 1-prob_no.term.deposit
#' #set number of ntiles
#' ntiles = 10
#' # determine cutoffs
#' cutoffs = c(stats::quantile(prob_term.deposit,probs = seq(0,1,1/ntiles),na.rm = TRUE))
#' #calculate ntile values
#' ntl_term.deposit <- (ntiles+1)-as.numeric(cut(prob_term.deposit,breaks=cutoffs,include.lowest=TRUE))
#' ntl_no.term.deposit <- (ntiles+1)-ntl_term.deposit
#' # create scored data frame
#' scores_and_ntiles <- train %>%
#'     select(has_td) %>%
#'     mutate(model_label=factor('logistic regression'),
#'            dataset_label=factor('train data'),
#'            y_true=factor(has_td),
#'            prob_term.deposit = prob_term.deposit,
#'            prob_no.term.deposit = prob_no.term.deposit,
#'            ntl_term.deposit = ntl_term.deposit,
#'            ntl_no.term.deposit = ntl_no.term.deposit) %>%
#'     select(-has_td)
#'
#' # add test data
#' #score model on test data
#' prob_no.term.deposit <- stats::predict(glm.model,newdata=test,type='response')
#' prob_term.deposit <- 1-prob_no.term.deposit
#' #set number of ntiles
#' ntiles = 10
#' # determine cutoffs
#' cutoffs = c(stats::quantile(prob_term.deposit,probs = seq(0,1,1/ntiles),na.rm = TRUE))
#' #calculate ntile values
#' ntl_term.deposit <- (ntiles+1)-as.numeric(cut(prob_term.deposit,breaks=cutoffs,include.lowest=TRUE))
#' ntl_no.term.deposit <- (ntiles+1)-ntl_term.deposit
#' scores_and_ntiles <- scores_and_ntiles %>%
#'   rbind(
#'    test %>%
#'     select(has_td) %>%
#'     mutate(model_label=factor('logistic regression'),
#'            dataset_label=factor('test data'),
#'            y_true=factor(has_td),
#'            prob_term.deposit = prob_term.deposit,
#'            prob_no.term.deposit = prob_no.term.deposit,
#'            ntl_term.deposit = ntl_term.deposit,
#'            ntl_no.term.deposit = ntl_no.term.deposit) %>%
#'     select(-has_td)
#'     )
#'
#' plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope='compare_datasets')
#' plot_cumgains()
#'
#' @name build_input_yourself
NULL
