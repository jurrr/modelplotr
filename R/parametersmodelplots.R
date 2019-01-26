##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### customize_plot_text()        ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Customize textual elements of the plots
#'
#' Function to overrule the default textual elements in the plots, like title, subtitle,
#' axis labels and annotation texts when the highlighting parameter \code{highlight_ntile}
#' is specified.
#'
#' @section How to customize textual elements of plots:
#' All textual parts of the plots can be customized, for instance to translate
#' textual elements to another language or to change the annotation text that is added with the
#' \code{highlight_ntile} parameter. Once you have created the \code{plot_input} dataframe
#' using \code{plotting_Scope}, you can run this \code{customize_plot_text()} function.
#' It returns a list, containing all textual elements of the plots, including annotation texts.
#' For instance, run \cr\cr
#' \code{my_plot_text <- customize_plot_text(plot_input = plot_input)} \cr\cr
#' The list contains plot-specific elements (e.g. \code{...$cumgains$...})). \cr
#' Now, you can change the textual elements by overriding the element(s) you want to customize.
#' For instance, if you want to change the textual elements of the gains plot to Dutch:\cr\cr
#' \code{my_plot_text$gains$plottitle <- 'Cumulatieve Gains grafiek'}\cr
#' \code{my_plot_text$gains$x_axis_label <- 'Deciel'}\cr
#' \code{my_plot_text$gains$y_axis_label <- 'cumulatieve gains'}\cr
#' \code{my_plot_text$cumgains$optimal_gains_label <- 'maximale gains'}\cr
#' \code{my_plot_text$cumgains$minimal_gains_label <- 'minimale gains'}\cr
#' \code{plot_cumgains(custom_plot_text = my_plot_text)}\cr\cr
#' To change the annotation text, use the placeholders starting with '&' to dynamically include:
#' \tabular{ll}{
#'   \bold{palaceholder} \tab \bold{placeholder value}\cr
#'   \code{&NTL} \tab ntile specified with parameter \code{highlight_ntile}.\cr
#'   \code{&PCTNTL} \tab Total percentage of dataset selected up until specified ntile.\cr
#'   \code{&MDL} \tab Selected model label(s).\cr
#'   \code{&DS} \tab Selected dataset label(s).\cr
#'   \code{&YVAL} \tab Selected target class (Y-value).\cr
#'   \code{&VALUE} \tab The plot specific value at specified ntile.
#'   Eg. Cumulative gains, Rumulative lift, Response, Cumulative response, Profit, ROI or Revenue.\cr
#' }
#' For instance, to translate the gains plot annotation text to Dutch:\cr
#' \code{my_plot_text$cumlift$annotationtext <- "Door &PCTNTL met de hoogste modelkans volgens model &MDL
#' in &DS te selecteren is deze selectie van &YVAL observaties &CUMLIFT keer beter dan een random selectie."}\cr
#' \code{plot_cumlift(highlight_ntile=3,custom_plot_text=my_plot_text)}
#'
#' @param plot_input Dataframe. Dataframe needs to be created with
#' \code{\link{plotting_scope}} or else meet required input format.
#' @return List with default values for all textual elements of the plots.
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
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_ntiles)
#' aggregate_over_ntiles()
#' plotting_scope(scope="compare_models")
#' custom_plot_text <- customize_plot_text(plot_input=plot_input)
#' custom_plot_text$cumlift$plottitle <- 'Cumulatieve Lift grafiek'
#' custom_plot_text$cumlift$x_axis_label <- 'Deciel'
#' plot_cumlift(custom_plot_text=custom_plot_text)
#' custom_plot_text$cumlift$annotationtext <- "Door &PCTNTL met de hoogste modelkans volgens model &MDL in &DS te selecteren is deze selectie van &YVAL observaties &CUMLIFT keer beter dan een random selectie."
#' plot_cumlift(highlight_ntile=3,custom_plot_text=custom_plot_text)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
customize_plot_text <- function(plot_input=plot_input){


  # create empty list for plot_text
  plot_text <- list()

  # add generic characteristics (derived from plot_input)
  plot_text$scope$scope <- max(as.character(plot_input$scope))
  plot_text$scope$sel_model <- max(as.character(plot_input$model_label))
  plot_text$scope$sel_dataset <- max(as.character(plot_input$dataset_label))
  plot_text$scope$sel_target_class <- max(as.character(plot_input$target_class))
  plot_text$scope$ntiles = max(plot_input$ntile)
  plot_text$scope$plotsubtitle <-
    ifelse(plot_text$scope$scope=="compare_datasets",
           paste0('scope: comparing datasets & model: ',plot_text$scope$sel_model,
                  ' & target class: ' ,plot_text$scope$sel_target_class),
           ifelse(plot_text$scope$scope=="compare_models",
                  paste0('scope: comparing models & dataset: ',plot_text$scope$sel_dataset,
                         ' & target class: ',plot_text$scope$sel_target_class),
                  ifelse(plot_text$scope$scope=="compare_targetclasses",
                         paste0('scope: comparing target classes & dataset: ',plot_text$scope$sel_dataset,
                                '  &  model: ',plot_text$scope$sel_model),
                         paste0('model: ',plot_text$scope$sel_model,
                                '  &  dataset: ',plot_text$scope$sel_dataset,
                                '  &  target class: ',plot_text$scope$sel_target_class))))
  plot_text$scope$x_axis_label <-
    ifelse(plot_text$scope$ntiles==10,'decile',
           ifelse(plot_text$scope$ntiles==4,'quartile',
                  ifelse(plot_text$scope$ntiles==5,'quintile',
                         ifelse(plot_text$scope$ntiles==20,'ventile',
                                ifelse(plot_text$scope$ntiles==100,'percentile',
                                       'ntile')))))

  # default values for textual plot elements

  # CUMGAINS
  plot_text$cumgains$plottitle <- "Cumulative gains"
  plot_text$cumgains$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$cumgains$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$cumgains$y_axis_label <- "cumulative gains"
  plot_text$cumgains$optimal_gains_label <- 'optimal gains'
  plot_text$cumgains$minimal_gains_label <- 'minimal gains'
  plot_text$cumgains$annotationtext <- "When we select &PCTNTL with the highest probability according to model &MDL, this selection holds &VALUE of all &YVAL cases in &DS."


  # CUMLIFT
  plot_text$cumlift$plottitle <- "Cumulative lift"
  plot_text$cumlift$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$cumlift$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$cumlift$y_axis_label <- "cumulative lift"
  plot_text$cumlift$lift_refline_label <- 'no lift'
  plot_text$cumlift$annotationtext <- "When we select &PCTNTL with the highest probability according to model &MDL in &DS, this selection for &YVAL cases is &VALUE times better than selecting without a model."


  # RESPONSE
  plot_text$response$plottitle <- "Response"
  plot_text$response$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$response$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$response$y_axis_label <- "response"
  plot_text$response$response_refline_label <- 'overall response'
  plot_text$response$annotationtext <- "When we select ntile &NTL according to model &MDL in dataset &DS the %% of &YVAL cases in the selection is &VALUE."


  # CUMRESPONSE
  plot_text$cumresponse$plottitle <- "Cumulative response"
  plot_text$cumresponse$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$cumresponse$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$cumresponse$y_axis_label <- "cumulative response"
  plot_text$cumresponse$response_refline_label <- 'overall response'
  plot_text$cumresponse$annotationtext <- "When we select ntiles 1 until &NTL according to model &MDL in dataset &DS the %% of &YVAL cases in the selection is &VALUE."


  # MULTIPLOT
  plot_text$multiplot$plottitle <-
    ifelse(plot_text$scope$scope=="compare_datasets",
           paste0('scope: comparing datasets & model: ',plot_text$scope$sel_model,
                  ' & target class: ' ,plot_text$scope$sel_target_class),
           ifelse(plot_text$scope$scope=="compare_models",
                  paste0('scope: comparing models & dataset: ',plot_text$scope$sel_dataset,
                         ' & target class: ',plot_text$scope$sel_target_class),
                  ifelse(plot_text$scope$scope=="compare_targetclasses",
                         paste0('scope: comparing target classes & dataset: ',plot_text$scope$sel_dataset,
                                '  &  model: ',plot_text$scope$sel_model),
                         paste0('model: ',plot_text$scope$sel_model,
                                '  &  dataset: ',plot_text$scope$sel_dataset,
                                '  &  target class: ',plot_text$scope$sel_target_class))))
  plot_text$multiplot$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$multiplot$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$multiplot$annotationtext <- NA

  # PROFIT
  plot_text$profit$plottitle <- "Profit"
  plot_text$profit$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$profit$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$profit$y_axis_label <- "Profit"
  plot_text$profit$profit_breakeven_refline_label <- 'break-even'
  plot_text$profit$profit_overall_refline_label <- 'overall profit'
  plot_text$profit$annotationtext <- "When we select ntiles 1 until &NTL in dataset &DS using model &MDL to target &YVAL cases the expected profit is &VALUE"

  # ROI
  plot_text$roi$plottitle <- "Return on Investment (ROI)"
  plot_text$roi$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$roi$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$roi$y_axis_label <- "% ROI"
  plot_text$roi$roi_breakeven_refline_label <- 'break-even'
  plot_text$roi$roi_overall_refline_label <- 'overall roi'
  plot_text$roi$annotationtext <- "When we select ntiles 1 until &NTL in dataset &DS using model &MDL to target &YVAL cases the expected return on investment is &VALUE."


  # COSTSREVS
  plot_text$costsrevs$plottitle <- "Costs and Revenues"
  plot_text$costsrevs$plotsubtitle <- plot_text$scope$plotsubtitle
  plot_text$costsrevs$x_axis_label <- plot_text$scope$x_axis_label
  plot_text$costsrevs$y_axis_label <- "costs / revenues"
  plot_text$costsrevs$costs_label <- "total costs"
  plot_text$costsrevs$revenues_label <- "revenues"
  plot_text$costsrevs$annotationtext <- "When we select ntiles 1 until &NTL in dataset &DS using model &MDL to target &YVAL cases the revenues are &VALUE"




  cat('List with default values for all textual plot elements is created.
      To customize titles, axis labels and annotation text, modify specific list elements.
      E.g, when List is named \'mylist\', to change the lift plot title to \'Cumulatieve Lift grafiek\', use:
      mylist$cumlift$title <- \'Cumulatieve Lift grafiek\'
      plot_cumlift(custom_plot_text = mylist)' )

  return(plot_text)

}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### setplotparams()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
# internal function to combine textual elements (default or customized), specified scope and line customization
# to generate list with all plot parameters (pp)

setplotparams <- function(plot_input,plottype,custom_line_colors,plot_text) {

  #  plottype <- "costsrevs"
  #  custom_line_colors <- NA

  # get textual elements and put them in pp (plot_params) list
  pp <- plot_text

  # ALL PLOTS
  pp$scope$plottype <- plottype
  pp$scope$levels <- unique(as.character(plot_input$legend))
  pp$scope$nlevels <- length(pp$scope$levels)
  pp$scope$randcols <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
  pp$scope$levelcols <- pp$scope$randcols[1:pp$scope$nlevels]
  pp$scope$xlabper <- ifelse(pp$scope$ntiles<=20,1,ifelse(pp$scope$ntiles<=40,2,5))
  pp$scope$ntile0 <- ifelse(pp$scope$plottype=="cumgains",1,0)
  if (length(custom_line_colors)==1 & is.na(custom_line_colors[1])){
    pp$scope$levelcols <- pp$scope$randcols[1:pp$scope$nlevels]
  } else if(length(custom_line_colors)==pp$scope$nlevels) {
    pp$scope$levelcols <- custom_line_colors
  } else if (length(custom_line_colors)<pp$scope$nlevels) {
    cat('specified custom_line_colors vector smaller than required length!
        It is extended with extra colors to match required length\n')
    lencustcols <- length(custom_line_colors)
    pp$scope$levelcols <- c(custom_line_colors,pp$scope$randcols[which(!pp$scope$randcols %in% custom_line_colors)][1:(pp$scope$nlevels-lencustcols)])
  } else if (length(custom_line_colors)>pp$scope$nlevels) {
    cat('specified custom_line_colors vector greater than required length!
        It is cropped to match required length\n')
    pp$scope$levelcols <- custom_line_colors[1:pp$scope$nlevels]
  } else {
    pp$scope$levelcols <- pp$scope$randcols[1:pp$scope$nlevels]
  }

  pp$scope$plottitle <- get('plottitle',get(pp$scope$plottype,plot_text))
  pp$scope$plotsubtitle <- get('plotsubtitle',get(pp$scope$plottype,plot_text))
  pp$scope$annotationtext <- get('annotationtext',get(pp$scope$plottype,plot_text))

  # GAINS
  if (pp$scope$scope=='compare_models') {
    pp$cumgains$reflevels <- paste0(pp$cumgains$optimal_gains_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$cumgains$reflevels <- paste0(pp$cumgains$optimal_gains_label,' (',pp$scope$levels,')')
  }
  pp$cumgains$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') {
    pp$cumgains$reflevelcols <- 'gray'
  } else { pp$cumgains$reflevelcols <- pp$scope$levelcols}
  pp$cumgains$levels <- c(pp$scope$levels,pp$cumgains$minimal_gains_label,pp$cumgains$reflevels)
  pp$cumgains$nlevels <- length(pp$cumgains$levels)
  pp$cumgains$legendcolumns <- ifelse(pp$cumgains$nlevels>6,2,1)
  pp$cumgains$linetypes <- c(rep('solid',pp$scope$nlevels),'dashed',rep('dotted',pp$cumgains$nreflevels))
  pp$cumgains$alphas <- c(rep(1,pp$scope$nlevels),1,rep(1,pp$cumgains$nreflevels))
  pp$cumgains$linecols <- c(pp$scope$levelcols,'gray',pp$cumgains$reflevelcols)
  pp$cumgains$linesizes <- c(rep(1,pp$scope$nlevels),0.5,rep(1.2,pp$cumgains$nreflevels))
  pp$cumgains$annolabelfmt <- 'scales::percent_format(accuracy=1)'

  # LIFT
  pp$cumlift$levels <- c(pp$scope$levels,pp$cumlift$lift_refline_label)
  pp$cumlift$nlevels <- length(pp$cumlift$levels)
  pp$cumlift$legendcolumns <- ifelse(pp$cumlift$nlevels>6,2,1)
  pp$cumlift$linetypes <- c(rep('solid',pp$scope$nlevels),'dashed')
  pp$cumlift$alphas <- c(rep(1,pp$scope$nlevels),1)
  pp$cumlift$linecols <- c(pp$scope$levelcols,'gray')
  pp$cumlift$linesizes <- c(rep(1,pp$scope$nlevels),0.5)
  pp$cumlift$annolabelfmt <- 'scales::comma_format(accuracy=0.1)'

  # RESPONSE
  if (pp$scope$scope=='compare_models') {
    pp$response$reflevels <- paste0(pp$response$response_refline_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$response$reflevels <- paste0(pp$response$response_refline_label,' (',pp$scope$levels,')')
  }
  pp$response$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') pp$response$reflevelcols <- 'gray' else pp$response$reflevelcols <- pp$scope$levelcols
  pp$response$levels <- c(pp$scope$levels,pp$response$reflevels)
  pp$response$nlevels <- length(pp$response$levels)
  pp$response$legendcolumns <- ifelse(pp$response$nlevels>6,2,1)
  pp$response$linetypes <- c(rep('solid',pp$scope$nlevels),rep('dashed',pp$response$nreflevels))
  pp$response$alphas <- c(rep(1,pp$scope$nlevels),rep(1,pp$response$nreflevels))
  pp$response$linecols <- c(pp$scope$levelcols,pp$response$reflevelcols)
  pp$response$linesizes <- c(rep(1,pp$scope$nlevels),rep(0.8,pp$response$nreflevels))
  pp$response$annolabelfmt <- 'scales::percent_format(accuracy=0.1)'

  # CUMRESPONSE
  if (pp$scope$scope=='compare_models') {
    pp$cumresponse$reflevels <- paste0(pp$cumresponse$response_refline_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$cumresponse$reflevels <- paste0(pp$cumresponse$response_refline_label,' (',pp$scope$levels,')')
  }
  pp$cumresponse$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') pp$cumresponse$reflevelcols <- 'gray' else pp$cumresponse$reflevelcols <- pp$scope$levelcols
  pp$cumresponse$levels <- c(pp$scope$levels,pp$cumresponse$reflevels)
  pp$cumresponse$nlevels <- length(pp$cumresponse$levels)
  pp$cumresponse$legendcolumns <- ifelse(pp$cumresponse$nlevels>6,2,1)
  pp$cumresponse$linetypes <- c(rep('solid',pp$scope$nlevels),rep('dashed',pp$cumresponse$nreflevels))
  pp$cumresponse$alphas <- c(rep(1,pp$scope$nlevels),rep(1,pp$cumresponse$nreflevels))
  pp$cumresponse$linecols <- c(pp$scope$levelcols,pp$cumresponse$reflevelcols)
  pp$cumresponse$linesizes <- c(rep(1,pp$scope$nlevels),rep(0.8,pp$cumresponse$nreflevels))
  pp$cumresponse$annolabelfmt <- 'scales::percent_format(accuracy=0.1)'

  # MULTIPLOT
  pp$multiplot$annolabelfmt <- ''

  # PROFIT
  if (pp$scope$scope=='compare_models') {
    pp$profit$reflevels <- paste0(pp$profit$profit_overall_refline_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$profit$reflevels <- paste0(pp$profit$profit_overall_refline_label,' (',pp$scope$levels,')')
  }
  pp$profit$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') {
    pp$profit$reflevelcols <- 'gray'
  } else { pp$profit$reflevelcols <- pp$scope$levelcols}
  pp$profit$levels <- c(pp$scope$levels,pp$profit$profit_breakeven_refline_label,pp$profit$reflevels)
  pp$profit$nlevels <- length(pp$profit$levels)
  pp$profit$legendcolumns <- ifelse(pp$profit$nlevels>6,2,1)
  pp$profit$linetypes <- c(rep('solid',pp$scope$nlevels),'dashed',rep('dotted',pp$profit$nreflevels))
  pp$profit$alphas <- c(rep(1,pp$scope$nlevels),1,rep(1,pp$profit$nreflevels))
  pp$profit$linecols <- c(pp$scope$levelcols,'gray',pp$profit$reflevelcols)
  pp$profit$linesizes <- c(rep(1,pp$scope$nlevels),0.8,rep(1.2,pp$profit$nreflevels))
  pp$profit$annolabelfmt <- 'scales::dollar_format(prefix = "€")'

  # ROI
  if (pp$scope$scope=='compare_models') {
    pp$roi$reflevels <- paste0(pp$roi$roi_overall_refline_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$roi$reflevels <- paste0(pp$roi$roi_overall_refline_label,' (',pp$scope$levels,')')
  }
  pp$roi$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') {
    pp$roi$reflevelcols <- 'gray'
  } else { pp$roi$reflevelcols <- pp$scope$levelcols}
  pp$roi$levels <- c(pp$scope$levels,pp$roi$roi_breakeven_refline_label,pp$roi$reflevels)
  pp$roi$nlevels <- length(pp$roi$levels)
  pp$roi$legendcolumns <- ifelse(pp$roi$nlevels>6,2,1)
  pp$roi$linetypes <- c(rep('solid',pp$scope$nlevels),'dashed',rep('dotted',pp$roi$nreflevels))
  pp$roi$alphas <- c(rep(1,pp$scope$nlevels),1,rep(1,pp$roi$nreflevels))
  pp$roi$linecols <- c(pp$scope$levelcols,'gray',pp$roi$reflevelcols)
  pp$roi$linesizes <- c(rep(1,pp$scope$nlevels),1.2,rep(0.7,pp$roi$nreflevels))
  pp$roi$annolabelfmt <- 'scales::percent_format(accuracy=1)'

  # COSTSREVS
  if (pp$scope$scope=='compare_models') {
    pp$costsrevs$costlevels <- paste0(pp$costsrevs$costs_label,' (',unique(plot_input$dataset_label),')')
  } else {
    pp$costsrevs$costlevels <- paste0(pp$costsrevs$costs_label,' (',pp$scope$levels,')')
  }
  pp$costsrevs$nreflevels <- ifelse(pp$scope$scope=='compare_models',1,pp$scope$nlevels)
  if (pp$scope$scope=='compare_models') {
    pp$costsrevs$reflevelcols <- 'gray'
  } else { pp$costsrevs$reflevelcols <- pp$scope$levelcols}
  pp$costsrevs$levels <- paste0(pp$costsrevs$revenues_label,' (',pp$scope$levels,')')
  pp$costsrevs$levels <- c(pp$costsrevs$levels,pp$costsrevs$costlevels)
  pp$costsrevs$nlevels <- length(pp$costsrevs$levels)
  pp$costsrevs$legendcolumns <- ifelse(pp$costsrevs$nlevels>6,2,1)
  pp$costsrevs$linetypes <- c(rep('solid',pp$scope$nlevels),rep('dashed',pp$costsrevs$nreflevels))
  pp$costsrevs$alphas <- c(rep(1,pp$scope$nlevels),rep(1,pp$roi$nreflevels))
  pp$costsrevs$linecols <- c(pp$scope$levelcols,pp$costsrevs$reflevelcols)
  pp$costsrevs$linesizes <- c(rep(1,pp$scope$nlevels),rep(1,pp$costsrevs$nreflevels))
  pp$costsrevs$annolabelfmt <- 'scales::dollar_format(prefix = "€")'

  pp$scope$annolabelfmt = get('annolabelfmt',get(pp$scope$plottype,pp))

  return(pp)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### annotate_plot()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#highlight_ntile='max'
annotate_plot <- function(plot=plot,highlight_input=plot_input_prepared,
                          highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp){

  if(!is.na(highlight_ntile)) {

    # check if scores_and_ntiles exists, otherwise create
    if (highlight_ntile<1 | (highlight_ntile>pp$scope$ntiles & highlight_ntile!='max_roi'& highlight_ntile!='max_profit')) {
      stop(paste0("Value for highlight_ntile not valid! Choose ntile value to highlight in range [1:",pp$scope$ntiles,"]
                  or use highlight_ntile='max_profit' or highlight_ntile='max_roi' for maximum value highlighting"))
    }
    if(!highlight_how %in% c('plot','text','plot_text')){
      cat("no valid value for highlight_how specified; default value (plot_text) is chosen
          -> choose 'plot_text' to highlight both the plot and add explanatory text below the plot
          -> choose 'plot' to only highlight both the plot - no explanatory text is added below the plot
          -> choose 'text' to only add explanatory text below the plot - the chosen ntile is not highlighted in the plot \n")
      highlight_how <- 'plot_text'
    }

    # prepare input for highlighting
    # when maximum value for financial plots is requested: get ntiles with maximum
    if(highlight_ntile == 'max_profit'){
      highlight_input = highlight_input %>% dplyr::group_by(legend) %>%
        dplyr::filter(refline == 0 & max_profit == 1) %>%
        dplyr::top_n(1,wt = plotvalue)  %>%
        dplyr::ungroup()
      highlight_ntile_num = highlight_input$ntile
    } else if(highlight_ntile == 'max_roi'){
        highlight_input = highlight_input %>% dplyr::group_by(legend) %>%
          dplyr::filter(refline == 0 & max_roi == 1) %>%
          dplyr::top_n(1,wt = plotvalue)  %>%
          dplyr::ungroup()
        highlight_ntile_num = highlight_input$ntile
      } else {
      highlight_input = highlight_input %>% dplyr::filter(ntile==highlight_ntile & refline==0)
      highlight_ntile_num = highlight_ntile
    }

    if(highlight_how %in% c('plot','plot_text')){
      # check ggplot version (clip=off is available in version 3.0 and later)
      if(packageVersion("ggplot2") < 3.0) {
        warning(paste0('You are using ggplot2 version ',packageVersion("ggplot2"),'. ggplot2 >= 3.0.0 is required for nicer annotated plots!'))
      }

      # add highlighting
      plot <- plot +
        # add highlighting cicle(s) to plot at ntile value
        ggplot2::geom_point(data = highlight_input,
                            ggplot2::aes(x=ntile,y=plotvalue,color=legend),shape=1,size=5,show.legend = FALSE)+
        # add line(s) from annotated point(s) to Y axis
        ggplot2::geom_segment(data = highlight_input,
                              ggplot2::aes(x=-Inf,y=plotvalue,xend=ntile+0.5,yend=plotvalue,colour=legend),
                              linetype="dotted",size=0.5,show.legend = FALSE)+
        # add line(s) from annotated point(s) to X axis
        ggplot2::geom_segment(data = highlight_input,
                              ggplot2::aes(x=ntile,y=-Inf,xend=ntile,yend=plotvalue+0.05,colour=legend),
                              linetype="dotted",size=1,show.legend = FALSE) +
        # add value labels for annotated points to Y axis
        ggplot2::geom_label(data=highlight_input,
                            ggplot2::aes(x=-Inf,y=plotvalue,label = eval(parse(text=paste0(pp$scope$annolabelfmt,"(plotvalue)"))),color=legend),fill="white",alpha=0.6,
                            hjust = 0, fontface = "bold",show.legend = FALSE)

      # emphasize ntile for which annotation is added on X axis
      if(min(highlight_ntile_num) == max(highlight_ntile_num) & highlight_ntile_num[1] %%  pp$scope$xlabper == 0){
        xbreaks <- seq((1-pp$scope$ntile0)*pp$scope$xlabper,pp$scope$ntiles+pp$scope$ntile0,pp$scope$xlabper)
        xfaces <- c(rep("plain",(pp$scope$ntile0+highlight_ntile_num-1)/pp$scope$xlabper),
                    "bold",
                    rep("plain",(pp$scope$ntiles+pp$scope$ntile0-highlight_ntile_num)/pp$scope$xlabper))
        xsizes <- c(rep(10,(pp$scope$ntile0+highlight_ntile_num-1)/pp$scope$xlabper),
                    12,
                    rep(10,(pp$scope$ntiles+pp$scope$ntile0-highlight_ntile_num)/pp$scope$xlabper))
        plot <- plot  +
          ggplot2::theme(
            axis.line = ggplot2::element_line(color="black"),
            axis.text.x = ggplot2::element_text(face=xfaces,size=xsizes))+
          ggplot2::scale_x_continuous(name=pp$scope$x_axis_label, breaks=xbreaks,labels=xbreaks,expand = c(0, 0.02))
      }else{
        xbreaks <- seq((1-pp$scope$ntile0)*pp$scope$xlabper,pp$scope$ntiles+pp$scope$ntile0,pp$scope$xlabper)
        xfaces <- rep("plain",(pp$scope$ntiles/pp$scope$xlabper)+pp$scope$ntile0)
        xsizes <- rep(10,(pp$scope$ntiles/pp$scope$xlabper)+pp$scope$ntile0)
        plot <- plot  +
          ggplot2::theme(
            axis.line = ggplot2::element_line(color="black"),
            axis.text.x = ggplot2::element_text(face=xfaces,size=xsizes))+
          ggplot2::scale_x_continuous(name=pp$scope$x_axis_label, breaks=xbreaks,labels=xbreaks,expand = c(0, 0.02))+
          # add value labels for annotated points to X axis
          ggplot2::geom_label(data=highlight_input %>% dplyr::filter(ntile %in% highlight_ntile_num & refline==0),
                              ggplot2::aes(x=highlight_ntile_num,y=-Inf,label = highlight_ntile_num,color=legend),fill="white",
                              vjust=0.2,fontface = "bold",alpha=0.8,show.legend = FALSE)
      }
      # make sure value labels for annotated points to X axis aren't clipped
      if(packageVersion("ggplot2") >= 3.0) plot <- plot + ggplot2::coord_cartesian(clip = 'off' )
    }

    # annotation text

    annovalues <- highlight_input %>%
      dplyr::filter(ntile %in% highlight_ntile_num & refline==0) %>%
      dplyr::mutate(xmin=rep(0,pp$scope$nlevels),
                    xmax=rep(100,pp$scope$nlevels),
                    ymin=seq(1,pp$scope$nlevels,1),
                    ymax=seq(2,pp$scope$nlevels+1,1),
                    # create variables with the values needed for the annotation texts
                    NTL=highlight_ntile_num,
                    PCTNTL=sprintf("%1.0f%%",100*highlight_ntile_num/pp$scope$ntiles),
                    MDL=model_label,
                    DS=dataset_label,
                    YVAL=target_class,
                    VALUE=eval(parse(text=paste0(pp$scope$annolabelfmt,"(plotvalue)"))),
                    # replace the placeholders for values in the annotation text per plot type
                    annotationtext =
                      eval(parse(text=paste0("sprintf('",stringr::str_replace_all(pp$scope$annotationtext,'&[A-Z]+','%s'), " ', ",
                      paste(substr(unlist(stringr:: str_extract_all(pp$scope$annotationtext,'&[A-Z]+')),2,100),
                      collapse = ', '),')'))))

    cat(paste(' ',paste0('Plot annotation for plot: ',pp$scope$plottitle),
              paste(paste0('- ',annovalues$annotationtext), collapse = '\n'),' ',' ', sep = '\n'))

    if(highlight_how %in% c('text','plot_text')){
      # create annotation text element to add to grob
      annotextplot <- ggplot2::ggplot(annovalues,
                                      ggplot2::aes(label = annotationtext, xmin = xmin, xmax = xmax, ymin = ymin,ymax = ymax,color=legend)) +
        ggplot2::geom_rect(fill=NA,color=NA) +
        ggplot2::scale_color_manual(values=pp$scope$levelcols)+
        ggfittext::geom_fit_text(place = "center",grow = TRUE,reflow = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position="none",
                       line =ggplot2::element_blank(),
                       title=ggplot2::element_blank(),
                       axis.text=ggplot2::element_blank())+
        ggplot2::scale_y_reverse()

      #remove title from plot
      plot <- plot + ggplot2::theme(
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank())

      # create title and subtitle elements for grob

      title <- grid::textGrob(pp$scope$plottitle, gp=grid::gpar(fontsize=18))
      subtitle <- grid::textGrob(pp$scope$plotsubtitle, gp=grid::gpar(fontsize=10,fontface="italic",col="black"))

      #add x axis labels when no annotation is applied to plot
      if(highlight_how =='text') {
        plot <- plot + ggplot2::scale_x_continuous(name=pp$cumgains$x_axis_label,
                                                                            breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
                                                                            labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))+
                       ggplot2::theme(axis.line.x=ggplot2::element_line(),axis.line.y=ggplot2::element_line())
      }

      # create grob layout and add elements to it
      lay <- as.matrix(c(1,2,rep(3,20),rep(4,1+pp$scope$nlevels)))
      plot <- gridExtra::arrangeGrob(title,subtitle,plot,annotextplot, layout_matrix = lay,
                                     widths = grid::unit(18, "cm"),heights = grid::unit(rep(12/(23+pp$scope$nlevels),23+pp$scope$nlevels), "cm"))

    }
    }
  return(plot)
}


quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


