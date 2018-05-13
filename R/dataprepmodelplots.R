

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package modelplotr loaded! Happy model plotting!")
}


#' Build 'eval_tot' containing Actuals and Predictions
#'
#' Build dataframe object 'eval_tot' that contains actuals and predictions on dependent variable for each dataset in datasets
#' @param datasets List of strings. A list of the names of the dataframe objects to include in model evaluation. All dataframes need to contain target variable and feature variables. Default value is list("train","set"), hence the presence of objects named "train" and "test" in the environment is expected.
#' @param modelname String. Name of the model object containing parameters to apply model on data.
#' @param targetname String. Name of the target variable in datasets. Target can be either binary or multinomial. Continuous targets are not supported.
#' @return Dataframe. Dataframe 'eval_tot' is built based on \code{datasets}.
#' It contains the dataset name, actuals on the target \code{target} , the predicted probabilities for each class of the target
#' and attribution to deciles in the dataset for each class of the target.
#' @seealso \code{\link{input_modevalplots}} for details on the function that aggregates the output to the input for the plots.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
dataprep_modevalplots <- function(datasets = list("train","test"),modelname = "clf",targetname="y"){

  eval_tot = data.frame()

  # create per dataset (train, test,...) a set with y, y_pred, p_y per y-value, dec_y per y-value

  for (dataset in datasets) {

    # 1.1. get category prediction from model (NOT YET DYNAMIC!) and prepare
    actuals = get(dataset) %>% dplyr::select_(y_true=targetname)
    predictions = predict(get(modelname), get(dataset))

    #predictions

    # 1.2. get probabilities per target category from model (NOT YET DYNAMIC!) and prepare
    probabilities = as.data.frame(predict(get(modelname),newdata=get(dataset),type = "prob"))

    #name probability per target category
    y_values = colnames(probabilities)
    colnames(probabilities) = paste0('prob_',y_values)
    y_probvars = colnames(probabilities)

    probabilities = cbind(dataset,actuals,probabilities)

    # 1.3. calculate deciles per target category
    for (i in 1:length(y_values)) {
      #! Added small proportion to prevent equal decile bounds and reset to 0-1 range (to prevent probs > 1.0)
      range01 <- function(x){(x-min(x))/(max(x)-min(x))}
      prob_plus_smallrandom = range01(probabilities[,y_probvars[i]]+runif(NROW(probabilities))/1000000)
      # determine cutoffs based on prob_plus_smallrandom
      cutoffs = c(quantile(prob_plus_smallrandom,probs = seq(0,1,0.1),na.rm = TRUE))
      # add decile variable per y-class
      probabilities[,paste0('dcl_',y_values[i])] <- 11-as.numeric(cut(prob_plus_smallrandom,breaks=cutoffs,include.lowest = T))
    }
    for (i in 1:length(y_values)) {
      probabilities[,paste0('ind_',y_values[i])] <- 1
    }
    eval_tot = rbind(eval_tot,probabilities)
  }
  eval_tot <<- eval_tot
  return('Data preparation step 1 succeeded! Dataframe \'eval_tot\' created.')
}


#' Build 'eval_t' with aggregated evaluation measures
#'
#' Build dataframe 'eval_t' with aggregated actuals and predictions . A record in 'eval_t' is unique on the combination of datasets-decile.
#' @param eval_tot Dataframe resulting from function dataprep_modevalplots().
#' @return Dataframe 'eval_t' is built based on \code{eval_t}.
#' It contains ..
#' @seealso \code{\link{dataprep_modevalplots}} for details on the function that generated the required input.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#' @importFrom tidyverse %>%
input_modevalplots <- function(prepared_input=eval_tot){

  # check if eval_tot exists, otherwise create
  if (!(exists("eval_tot"))) dataprep_modevalplots()

  modelgroups = levels(prepared_input$dataset)
  yvals = levels(prepared_input$y_true)

  eval_t_tot <- data.frame()


  for (val in yvals) {

    eval_t_zero = eval_tot %>%
      dplyr::mutate("category"=val,"decile"=0) %>%
      dplyr::group_by_("dataset","category","decile") %>%
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

    eval_t_add = eval_tot %>%
      dplyr::mutate("category"=val,"decile"=get(paste0("dcl_",val))) %>%
      dplyr::group_by_("dataset","category","decile") %>%
      dplyr::summarize(neg=sum(y_true!=category),
                pos=sum(y_true==category),
                tot=n(),
                pct=1.0*sum(y_true==category)/n()) %>%
      dplyr::group_by_("dataset","category") %>%
      dplyr::mutate(negtot=sum(neg),
             postot=sum(pos),
             tottot=sum(tot),
             pcttot=1.0*sum(pos)/sum(tot)) %>%
      dplyr::group_by_("dataset","category","negtot","postot","tottot","pcttot") %>%
      dplyr::mutate(cumneg=cumsum(neg),
             cumpos=cumsum(pos),
             cumtot=cumsum(tot),
             cumpct=1.0*cumsum(pos)/cumsum(tot),
             gain=pos/postot,
             cumgain=cumsum(pos)/postot,
             gain_ref=decile/10,
             gain_opt=ifelse(decile>=ceiling(10*pcttot),1,(decile/10)/(ceiling(10*pcttot)/10)),
             lift=pct/pcttot,
             cumlift=1.0*cumsum(pos)/cumsum(tot)/pcttot,
             cumlift_ref = 1) %>%
      as.data.frame()

    eval_t_tot = rbind(eval_t_tot,eval_t_zero,eval_t_add)
    eval_t_tot = eval_t_tot[with(eval_t_tot,order(category,dataset,decile)),]
  }
  eval_t_tot <<- eval_t_tot
}
