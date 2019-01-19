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
#'   \code{&CUMGAINS} \tab Cumulative gains value at specified ntile.\cr
#'   \code{&CUMLIFT} \tab Cumulative lift value at specified ntile.\cr
#'   \code{&RESPONSE} \tab Response value at specified ntile.\cr
#'   \code{&CUMRESPONSE} \tab Cumulative response value at specified ntile.\cr
#' }
#' For instance, to translate the gains plot annotation text to Dutch:\cr
#' \code{my_plot_text$cumlift$annotationtext <- "Door &PCTNTL met de hoogste modelkans volgens model &MDL
#' in &DS te selecteren is deze selectie van &YVAL observaties &CUMLIFT keer beter dan een random selectie."}\cr
#' \code{plot_cumlift(highlight_ntile=3,custom_plot_text=custom_plot_text)}
#'
#' @param plot_input Dataframe. Dataframe needs to be created with
#' \code{\link{plotting_scope}} or else meet required input format.
#' @return List \code{custom_plot_text} .
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
  scope <- max(as.character(plot_input$scope))
  sel_model <- max(as.character(plot_input$model_label))
  sel_dataset <- max(as.character(plot_input$dataset_label))
  sel_target_class <- max(as.character(plot_input$target_class))
  ntiles = max(plot_input$ntile)

  custom_plot_text <- list()
  plotsubtitle <-
    ifelse(scope=="compare_datasets",
           paste0('scope: comparing datasets & model: ',sel_model,' & target class: ' ,sel_target_class),
             ifelse(scope=="compare_models",
                    paste0('scope: comparing models & dataset: ',sel_dataset,' & target class: ',sel_target_class),
                    ifelse(scope=="compare_targetclasses",
                           paste0('scope: comparing target classes & dataset: ',sel_dataset,'  &  model: ',sel_model),
                           paste0('model: ',sel_model,'  &  dataset: ',sel_dataset,'  &  target class: ',sel_target_class))))
  x_axis_label <-
      ifelse(ntiles==10,'decile',ifelse(ntiles==4,'quartile',ifelse(ntiles==5,'quintile',
          ifelse(ntiles==20,'ventile',ifelse(ntiles==100,'percentile','ntile')))))
  custom_plot_text$multiplot$plottitle <-
    ifelse(scope=="compare_datasets",
         paste0('scope: comparing datasets & model: ',sel_model,' & target class: ' ,sel_target_class),
         ifelse(scope=="compare_models",
                paste0('scope: comparing models & dataset: ',sel_dataset,' & target class: ',sel_target_class),
                ifelse(scope=="compare_targetclasses",
                       paste0('scope: comparing target classes & dataset: ',sel_dataset,'  &  model: ',sel_model),
                       paste0('model: ',sel_model,'  &  dataset: ',sel_dataset,'  &  target class: ',sel_target_class))))
  custom_plot_text$cumlift$plottitle <- "Cumulative lift"
  custom_plot_text$cumgains$plottitle <- "Cumulative gains"
  custom_plot_text$response$plottitle <- "Response"
  custom_plot_text$cumresponse$plottitle <- "Cumulative response"
  custom_plot_text$cumlift$plotsubtitle <- plotsubtitle
  custom_plot_text$cumgains$plotsubtitle <- plotsubtitle
  custom_plot_text$response$plotsubtitle <- plotsubtitle
  custom_plot_text$cumresponse$plotsubtitle <- plotsubtitle
  custom_plot_text$multiplot$plotsubtitle <- plotsubtitle
  custom_plot_text$cumresponse$x_axis_label <- x_axis_label
  custom_plot_text$cumlift$x_axis_label <- x_axis_label
  custom_plot_text$cumgains$x_axis_label <- x_axis_label
  custom_plot_text$response$x_axis_label <- x_axis_label
  custom_plot_text$multiplot$x_axis_label <- x_axis_label
  custom_plot_text$cumresponse$y_axis_label <- "cumulative response"
  custom_plot_text$cumlift$y_axis_label <- "cumulative lift"
  custom_plot_text$cumgains$y_axis_label <- "cumulative gains"
  custom_plot_text$response$y_axis_label <- "response"
  custom_plot_text$cumgains$optimal_gains_label <- 'optimal gains'
  custom_plot_text$cumgains$minimal_gains_label <- 'minimal gains'
  custom_plot_text$cumlift$lift_refline_label <- 'no lift'
  custom_plot_text$response$response_refline_label <- 'overall response'
  custom_plot_text$cumresponse$response_refline_label <- 'overall response'
  custom_plot_text$cumlift$lift_refline_label <- 'no lift'
  custom_plot_text$response$response_refline_label <- 'overall response'
  custom_plot_text$cumresponse$response_refline_label <- 'overall response'
  custom_plot_text$cumlift$annotationtext <- "When we select &PCTNTL with the highest probability according to model &MDL in &DS, this selection for &YVAL cases is &CUMLIFT times better than selecting without a model."
  custom_plot_text$cumgains$annotationtext <- "When we select &PCTNTL with the highest probability according to &MDL, this selection holds &CUMGAINS of all &YVAL cases in &DS."
  custom_plot_text$response$annotationtext <- "When we select ntile &NTL according to model &MDL in dataset &DS the %% of &YVAL cases in the selection is &RESPONSE"
  custom_plot_text$cumresponse$annotationtext <- "When we select ntiles 1 until &NTL according to model &MDL in dataset &DS the %% of &YVAL cases in the selection is &CUMRESPONSE."
  custom_plot_text$multiplot$annotationtext <- NA
  cat('List with default values for all textual plot elements is created.
To customize titles, axis labels and annotation text, modify specific list elements.
E.g, when List is named \'mylist\', to change the lift plot title to \'Cumulatieve Lift grafiek\', use:
    mylist$cumlift$title <- \'Cumulatieve Lift grafiek\'
    plot_cumlift(custom_plot_text = mylist)' )

  return(custom_plot_text)

  }


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### setplotparams()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


setplotparams <- function(plot_input,plottype,custom_line_colors,custom_plot_text) {

#  plot_input <- plot_input
#  plottype <- "cumlift"
#  custom_line_colors <- NA
#  custom_plot_text <- custom_plot_text

  # PLOT TEXT
  #if(is.na(custom_plot_text[1])) custom_plot_text <- customize_plot_text(plot_input=plot_input)

  pp <- list()

  # ALL PLOTS
  pp$plottype <- plottype
  pp$seltype <- max(as.character(plot_input$scope))
  pp$selmod <- max(as.character(plot_input$model_label))
  pp$seldata <- max(as.character(plot_input$dataset_label))
  pp$selval <- max(as.character(plot_input$target_class))
  pp$levels <- unique(as.character(plot_input$legend))
  pp$nlevels <- length(pp$levels)
  pp$randcols <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
  pp$levelcols <- pp$randcols[1:pp$nlevels]
  pp$ntiles <- max(plot_input$ntile)
  pp$ntiles_label <- get('x_axis_label',get(pp$plottype,custom_plot_text))
  pp$xlabper <- ifelse(max(plot_input$ntile)<=20,1,ifelse(max(plot_input$ntile)<=40,2,5))
  pp$ntile0 <- ifelse(pp$plottype=="cumgains",1,0)
  if (length(custom_line_colors)==1 & is.na(custom_line_colors[1])){
    pp$levelcols <- pp$randcols[1:pp$nlevels]
  } else if(length(custom_line_colors)==pp$nlevels) {
    pp$levelcols <- custom_line_colors
  } else if (length(custom_line_colors)<pp$nlevels) {
    cat('specified custom_line_colors vector smaller than required length!
      It is extended with extra colors to match required length')
    pp$lencustcols <- length(custom_line_colors)
    pp$levelcols <- c(custom_line_colors,pp$randcols[which(!pp$randcols %in% custom_line_colors)][1:(pp$nlevels-pp$lencustcols)])
  } else if (length(custom_line_colors)>pp$nlevels) {
    cat('specified custom_line_colors vector greater than required length!
      It is cropped to match required length')
    pp$lencustcols <- length(custom_line_colors)
    pp$levelcols <- custom_line_colors[1:pp$nlevels]
  } else {
    pp$levelcols <- pp$randcols[1:pp$nlevels]
  }
  pp$legendcolumns <- ifelse(pp$nnewlevels>6,2,1)
  pp$linetypes <- c(rep('solid',pp$nlevels),'dashed')
  pp$alphas <- c(rep(1,pp$nlevels),1)
  pp$linecols <- c(pp$levelcols,'gray')
  pp$linesizes <- c(rep(1,pp$nlevels),0.5)

  pp$plottitle <- get('plottitle',get(pp$plottype,custom_plot_text))
  pp$plotsubtitle <- get('plotsubtitle',get(pp$plottype,custom_plot_text))

  pp$multiplottitle <- get('plottitle',get('multiplot',custom_plot_text))

  pp$annotationtext = get('annotationtext',get(pp$plottype,custom_plot_text))

  # GAINS
  if (pp$seltype=='compare_models') {
    pp$optgainsreflevels <- paste0(get('optimal_gains_label',get('cumgains',custom_plot_text)),' (',unique(plot_input$dataset_label),')')
  } else {
    pp$optgainsreflevels <- paste0(get('optimal_gains_label',get('cumgains',custom_plot_text)),' (',pp$levels,')')
  }
  pp$optimalgainslabel <- get('optimal_gains_label',get('cumgains',custom_plot_text))
  pp$minimalgainslabel <- get('minimal_gains_label',get('cumgains',custom_plot_text))
  pp$noptgainsreflevels <- ifelse(pp$seltype=='compare_models',1,pp$nlevels)
  if (pp$seltype=='compare_models') pp$optgainsreflevelcols <- 'gray' else pp$optgainsreflevelcols <- pp$levelcols
  pp$gainslevels <- c(pp$levels,pp$minimalgainslabel,pp$optgainsreflevels)
  pp$ngainslevels <- length(pp$gainslevels)
  pp$gainslegendcolumns <- ifelse(pp$ngainslevels>6,2,1)
  pp$gainslinetypes <- c(rep('solid',pp$nlevels),'dashed',rep('dotted',pp$noptgainsreflevels))
  pp$gainsalphas <- c(rep(1,pp$nlevels),1,rep(1,pp$noptgainsreflevels))
  pp$gainslinecols <- c(pp$levelcols,'gray',pp$optgainsreflevelcols)
  pp$gainslinesizes <- c(rep(1,pp$nlevels),0.5,rep(1.2,pp$noptgainsreflevels))

  # LIFT
  pp$liftreflabel <- get('lift_refline_label',get('cumlift',custom_plot_text))
  pp$liftlevels <- c(pp$levels,pp$liftreflabel)
  pp$nliftlevels <- length(pp$liftlevels)
  pp$liftlegendcolumns <- ifelse(pp$nliftlevels>6,2,1)
  pp$liftlinetypes <- c(rep('solid',pp$nlevels),'dashed')
  pp$liftalphas <- c(rep(1,pp$nlevels),1)
  pp$liftlinecols <- c(pp$levelcols,'gray')
  pp$liftlinesizes <- c(rep(1,pp$nlevels),0.5)

  # RESPONSE
  if (pp$seltype=='compare_models') {
    pp$respreflevels <- paste0(get('response_refline_label',get('response',custom_plot_text)),' (',unique(plot_input$dataset_label),')')
  } else {
    pp$respreflevels <- paste0(get('response_refline_label',get('response',custom_plot_text)),' (',pp$levels,')')
  }
  pp$nrespreflevels <- ifelse(pp$seltype=='compare_models',1,pp$nlevels)
  if (pp$seltype=='compare_models') pp$respreflevelcols <- 'gray' else pp$respreflevelcols <- pp$levelcols
  pp$resplevels <- c(pp$levels,pp$respreflevels)
  pp$nresplevels <- length(pp$resplevels)
  pp$resplegendcolumns <- ifelse(pp$nresplevels>6,2,1)
  pp$resplinetypes <- c(rep('solid',pp$nlevels),rep('dashed',pp$nrespreflevels))
  pp$respalphas <- c(rep(1,pp$nlevels),rep(1,pp$nrespreflevels))
  pp$resplinecols <- c(pp$levelcols,pp$respreflevelcols)
  pp$resplinesizes <- c(rep(1,pp$nlevels),rep(0.8,pp$nrespreflevels))

  # CUMRESPONSE
  if (pp$seltype=='compare_models') {
    pp$cumrespreflevels <- paste0(get('response_refline_label',get('cumresponse',custom_plot_text)),' (',unique(plot_input$dataset_label),')')
  } else {
    pp$cumrespreflevels <- paste0(get('response_refline_label',get('cumresponse',custom_plot_text)),' (',pp$levels,')')
  }
  pp$cumresplevels <- c(pp$levels,pp$cumrespreflevels)


  return(pp)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### annotate_plot()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

annotate_plot <- function(plot=plot,plot_input=plot_input_prepared,
                          highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp){

  if(!is.na(highlight_ntile)) {

    # check if scores_and_ntiles exists, otherwise create
    if (highlight_ntile<1 | highlight_ntile>pp$ntiles) {
      stop(paste0("Value for highlight_ntile not valid! Choose ntile value to highlight in range [1:",pp$ntiles,"]"))
    }
    if(!highlight_how %in% c('plot','text','plot_text')){
      cat("no valid value for highlight_how specified; default value (plot_text) is chosen
-> choose 'plot_text' to highlight both the plot and add explanatory text below the plot
-> choose 'plot' to only highlight both the plot - no explanatory text is added below the plot
-> choose 'text' to only add explanatory text below the plot - the chosen ntile is not highlighted in the plot \n")
      highlight_how <- 'plot_text'
    }

    if(highlight_how %in% c('plot','plot_text')){
    # check ggplot version (clip=off is available in version 3.0 and later)
    if(packageVersion("ggplot2") < 3.0) {
      warning(paste0('You are using ggplot2 version ',packageVersion("ggplot2"),'. ggplot2 >= 3.0.0 is required for nicer annotated plots!'))
    }
    plot <- plot +
      # add highlighting cicle(s) to plot at ntile value
      ggplot2::geom_point(data = plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0),
        ggplot2::aes(x=ntile,y=plotvalue,color=legend),shape=1,size=5,show.legend = FALSE)+
      # add line(s) from annotated point(s) to Y axis
      ggplot2::geom_segment(data = plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0),
        ggplot2::aes(x=-Inf,y=plotvalue,xend=ntile+0.5,yend=plotvalue,colour=legend),
        linetype="dotted",size=0.5,show.legend = FALSE)+
      # add line(s) from annotated point(s) to X axis
      ggplot2::geom_segment(data = plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0),
        ggplot2::aes(x=ntile,y=0,xend=ntile,yend=plotvalue+0.05),colour="gray",
        linetype="dotted",size=1,show.legend = FALSE) +
      # add value labels for annotated points to Y axis
      ggplot2::geom_label(data=plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0),
        ggplot2::aes(x=-Inf,y=plotvalue,label = sprintf("%1.0f%%", 100*plotvalue),color=legend),fill="white",alpha=0.6,
        hjust = 0, fontface = "bold",show.legend = FALSE)
      # emphasize ntile for which annotation is added on X axis
      if(highlight_ntile %%  pp$xlabper == 0){
        xbreaks <- seq((1-pp$ntile0)*pp$xlabper,pp$ntiles+pp$ntile0,pp$xlabper)
        xfaces <- c(rep("plain",(pp$ntile0+highlight_ntile-1)/pp$xlabper),
                    "bold",
                    rep("plain",(pp$ntiles+pp$ntile0-highlight_ntile)/pp$xlabper))
        xsizes <- c(rep(10,(pp$ntile0+highlight_ntile-1)/pp$xlabper),
                    12,
                    rep(10,(pp$ntiles+pp$ntile0-highlight_ntile)/pp$xlabper))
        plot <- plot  +
          ggplot2::theme(
            axis.line = ggplot2::element_line(color="black"),
            axis.text.x = ggplot2::element_text(face=xfaces,size=xsizes))+
          ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=xbreaks,labels=xbreaks,expand = c(0, 0.02))
      }else{
        xbreaks <- seq((1-pp$ntile0)*pp$xlabper,pp$ntiles+pp$ntile0,pp$xlabper)
        xfaces <- rep("plain",(pp$ntiles/pp$xlabper)+pp$ntile0)
        xsizes <- rep(10,(pp$ntiles/pp$xlabper)+pp$ntile0)
        plot <- plot  +
          ggplot2::theme(
            axis.line = ggplot2::element_line(color="black"),
            axis.text.x = ggplot2::element_text(face=xfaces,size=xsizes))+
          ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=xbreaks,labels=xbreaks,expand = c(0, 0.02))+
          # add value labels for annotated points to X axis
          ggplot2::geom_label(data=plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0),
            ggplot2::aes(x=highlight_ntile,y=-Inf,label = highlight_ntile),fill="white",vjust=0.2,fontface = "bold",show.legend = FALSE)
      }
    # make sure value labels for annotated points to X axis aren't clipped
    if(packageVersion("ggplot2") >= 3.0) plot <- plot + ggplot2::coord_cartesian(clip = 'off' )
    }

    # annotation text

    annovalues <- plot_input %>% dplyr::filter(ntile==highlight_ntile & refline==0) %>%
      dplyr::mutate(xmin=rep(0,pp$nlevels),
      xmax=rep(100,pp$nlevels),
      ymin=seq(1,pp$nlevels,1),
      ymax=seq(2,pp$nlevels+1,1),
      # create variables with the values needed for the annotation texts
      NTL=highlight_ntile,
      PCTNTL=sprintf("%1.0f%%",100*highlight_ntile/pp$ntiles),
      MDL=model_label,
      DS=dataset_label,
      YVAL=target_class,
      CUMGAINS=sprintf("%1.0f%%", 100*plotvalue),
      CUMLIFT=sprintf("%1.1f", plotvalue),
      RESPONSE=sprintf("%1.0f%%",100*plotvalue),
      CUMRESPONSE=sprintf("%1.0f%%",100*plotvalue),
      # replace the placeholders for values in the annotation text per plot type
      annotationtext = eval(parse(text=paste0("sprintf('",stringr::str_replace_all(pp$annotationtext,'&[A-Z]+','%s'), " ', ",
                                              paste(substr(unlist(stringr:: str_extract_all(pp$annotationtext,'&[A-Z]+')),2,100),collapse = ', '),')'))))

    cat(paste(' ','Plot annotation:',paste(paste0('- ',annovalues$annotationtext), collapse = '\n'),' ',' ', sep = '\n'))

    if(highlight_how %in% c('text','plot_text')){
      # create annotation text element to add to grob
      annotextplot <- ggplot2::ggplot(annovalues,
      ggplot2::aes(label = annotationtext, xmin = xmin, xmax = xmax, ymin = ymin,ymax = ymax,color=legend)) +
      ggplot2::geom_rect(fill=NA,color=NA) +
      ggplot2::scale_color_manual(values=pp$levelcols)+
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

      title <- grid::textGrob(pp$plottitle, gp=grid::gpar(fontsize=18))
      subtitle <- grid::textGrob(pp$plotsubtitle, gp=grid::gpar(fontsize=10,fontface="italic",col="black"))

      # create grob layout and add elements to it
      lay <- as.matrix(c(1,2,rep(3,20),rep(4,1+pp$nlevels)))
      plot <- gridExtra::arrangeGrob(title,subtitle,plot,annotextplot, layout_matrix = lay,
        widths = grid::unit(18, "cm"),heights = grid::unit(rep(12/(23+pp$nlevels),23+pp$nlevels), "cm"))
      }
  }
  return(plot)
}


quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumgains()                   ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative gains plot
#'
#' Generates the cumulative gains plot. This plot, often referred to as the gains chart,
#' helps answering the question: \bold{\emph{When we apply the model and select the best X ntiles,
#' what percentage of the actual target class observations can we expect to target?}}
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_ntile Integer. Specifying the ntile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the ntile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen ntile.
#' "text" adds text below the plot explaining the plot at chosen ntile but does not highlight the plot.
#' @param save_fig Logical. Save plot to file? Default = FALSE. When set to TRUE, saved plots are optimized for 18x12cm.
#' @param save_fig_filename String. Filename of saved plot. Default the plot is saved as {working_dir_path}/{plotname}.png.
#' @return ggplot object. Cumulative gains plot.
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
#' plot_cumgains()
#' plot_cumgains(custom_line_colors=c("orange","purple"))
#' plot_cumgains(highlight_ntile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumgains <- function(data=plot_input,highlight_ntile=NA,highlight_how='plot_text',
                          save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,...) {

  # check if highlight_decile is used instead of highlight_ntile
  if ('highlight_decile' %in% names(match.call())) {
    warning("parameter highlight_decile is depreciated and replaced by highlight_ntile.")
    highlight_ntile <-match.call(expand.dots = FALSE)$...$highlight_decile
  }
  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  if(is.null(custom_plot_text)) custom_plot_text <- quiet(customize_plot_text(plot_input = plot_input))

  pp <- setplotparams(plot_input = plot_input,plottype = "cumgains",custom_line_colors=custom_line_colors,custom_plot_text=custom_plot_text)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::select(scope:ntile,plotvalue=cumgain,legend,refline)
  if (pp$seltype=="compare_models") {
    optreflines <- plot_input %>%
      dplyr::mutate(legend=paste0(pp$optimalgainslabel,' (',dataset_label,')'),model_label='',plotvalue=gain_opt,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    optreflines <- plot_input%>%
      dplyr::mutate(legend=paste0(pp$optimalgainslabel,' (',legend,')'),plotvalue=gain_opt,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline)
  }
  # optreflines <- plot_input %>%
  #     dplyr::mutate(legend=pp$optgainsreflevels,model_label='',plotvalue=gain_opt,refline=1) %>%
  #     dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
  #     dplyr::distinct()
  minrefline <- plot_input %>%
    dplyr::mutate(legend=paste0(pp$minimalgainslabel),model_label='',dataset_label='',target_class='',plotvalue=gain_ref,refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,optreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$gainslevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$gainslinetypes,guide=ggplot2::guide_legend(ncol=pp$gainslegendcolumns))+
    ggplot2::scale_color_manual(values=pp$gainslinecols)+
    ggplot2::scale_size_manual(values=pp$gainslinesizes)+
    ggplot2::scale_alpha_manual(values=pp$gainsalphas) +
    ggplot2::scale_y_continuous(name=get('y_axis_label',get(pp$plottype,custom_plot_text)),breaks=seq(0,1,0.2),labels = scales::percent ,expand = c(0, 0.02)) +
    ggplot2::labs(title=pp$plottitle,subtitle=pp$plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(size = 10,hjust = 0.5,face="italic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position=c(0.975,0.025),legend.justification=c(1, 0),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("white",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())

  #annotate plot at ntile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=seq(0,pp$ntiles,pp$xlabper),
                                                              labels=seq(0,pp$ntiles,pp$xlabper),expand = c(0, 0.02))

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\\\|/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  } else {
    return(plot)
  }
}





##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumlift()                       ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative Lift plot
#'
#' Generates the cumulative lift plot, often referred to as lift plot or index plot,
#' helps you answer the question: When we apply the model and select the best X ntiles,
#' how many times better is that than using no model at all?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_ntile Integer. Specifying the ntile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the ntile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen ntile.
#' "text" adds text below the plot explaining the plot at chosen ntile but does not highlight the plot.
#' @param save_fig Logical. Save plot to file? Default = FALSE. When set to TRUE, saved plots are optimized for 18x12cm.
#' @param save_fig_filename String. Filename of saved plot. Default the plot is saved as {working_dir_path}/{plotname}.png.
#' @return ggplot object. Lift plot.
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
#' plotting_scope(scope="compare_datasets")
#' plot_cumlift()
#' plot_cumlift(custom_line_colors=c("orange","purple"))
#' plot_cumlift(highlight_ntile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumlift <- function(data=plot_input,highlight_ntile=NA,highlight_how='plot_text',
                         save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,...) {

  if ('highlight_decile' %in% names(match.call())) {
    warning("parameter highlight_decile is depreciated and replaced by highlight_ntile.")
    highlight_ntile <-match.call(expand.dots = FALSE)$...$highlight_decile
  }

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  if(is.null(custom_plot_text)) custom_plot_text <- quiet(customize_plot_text(plot_input = plot_input))

  pp <- setplotparams(plot_input = plot_input,plottype = "cumlift",custom_line_colors=custom_line_colors,custom_plot_text=custom_plot_text)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=cumlift,legend,refline)
  minrefline <- plot_input %>% dplyr::filter(ntile>0) %>%
    dplyr::mutate(legend=pp$liftreflabel,model_label='',dataset_label='',target_class='',plotvalue=cumlift_ref,refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$liftlevels)


  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$liftlinetypes,guide=ggplot2::guide_legend(ncol=pp$liftlegendcolumns))+
    ggplot2::scale_color_manual(values=pp$liftlinecols)+
    ggplot2::scale_size_manual(values=pp$liftlinesizes)+
    ggplot2::scale_alpha_manual(values=pp$liftalphas) +
    ggplot2::scale_y_continuous(name=get('y_axis_label',get(pp$plottype,custom_plot_text)),labels = scales::percent,expand = c(0, 0.02)) +
    ggplot2::expand_limits(y=c(0,max(2,max(plot_input_prepared$plotvalue,na.rm = T)))) +
    ggplot2::labs(title=pp$plottitle,subtitle=pp$plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10,hjust = 0.5,face="italic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position=c(0.975,0.975),legend.justification=c(1, 1),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("white",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())

  #annotate plot at ntile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=seq(0,pp$ntiles,pp$xlabper),
    labels=seq(0,pp$ntiles,pp$xlabper),expand = c(0, 0.02))

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\\\|/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
    cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  } else {
  return(plot)
  }
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_response()                   ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Response plot
#'
#' Generates the response plot. It plots the percentage of target class observations
#' per ntile. It can be used to answer the following business question: When we apply
#' the model and select ntile X, what is the expected percentage of target class observations
#' in that ntile?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_ntile Integer. Specifying the ntile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the ntile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen ntile.
#' "text" adds text below the plot explaining the plot at chosen ntile but does not highlight the plot.
#' @param save_fig Logical. Save plot to file? Default = FALSE. When set to TRUE, saved plots are optimized for 18x12cm.
#' @param save_fig_filename String. Filename of saved plot. Default the plot is saved as {working_dir_path}/{plotname}.png.
#' @return ggplot object. Response plot.
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
#' plotting_scope(scope="compare_targetclasses")
#' plot_response()
#' plot_response(custom_line_colors=RColorBrewer::brewer.pal(3,"Dark2"))
#' plot_response(highlight_ntile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_response <- function(data=plot_input,highlight_ntile=NA,highlight_how='plot_text',
                          save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,...) {

  if ('highlight_decile' %in% names(match.call())) {
    warning("parameter highlight_decile is depreciated and replaced by highlight_ntile.")
    highlight_ntile <-match.call(expand.dots = FALSE)$...$highlight_decile
  }

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  if(is.null(custom_plot_text)) custom_plot_text <- quiet(customize_plot_text(plot_input = plot_input))

  pp <- setplotparams(plot_input = plot_input,plottype = "response",custom_line_colors=custom_line_colors,custom_plot_text=custom_plot_text)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=pct,legend,refline)
  if (pp$seltype=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(get('response_refline_label',get('response',custom_plot_text)),' (',dataset_label,')'),model_label='',plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input%>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(get('response_refline_label',get('response',custom_plot_text)),' (',legend,')'),plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$resplevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$resplinetypes,guide=ggplot2::guide_legend(ncol=pp$resplegendcolumns))+
    ggplot2::scale_color_manual(values=pp$resplinecols)+
    ggplot2::scale_size_manual(values=pp$resplinesizes)+
    ggplot2::scale_alpha_manual(values=pp$respalphas)+
    ggplot2::scale_y_continuous(name=get('y_axis_label',get(pp$plottype,custom_plot_text)) ,expand = c(0, 0.02),labels = scales::percent) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$plottitle,subtitle=pp$plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10,hjust = 0.5,face="italic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position=c(0.975,0.975),legend.justification=c(1, 1),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("white",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())


  #annotate plot at ntile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=seq(0,pp$ntiles,pp$xlabper),
    labels=seq(0,pp$ntiles,pp$xlabper),expand = c(0, 0.02))

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\\\|/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
    cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  } else {
    return(plot)
  }
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumresponse()                ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative Respose plot
#'
#' Generates the cumulative response plot. It plots the cumulative percentage of
#' target class observations up until that ntile. It helps answering the question:
#' When we apply the model and select up until ntile X, what is the expected percentage of
#' target class observations in the selection?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_ntile Integer. Specifying the ntile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the ntile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen ntile.
#' "text" adds text below the plot explaining the plot at chosen ntile but does not highlight the plot.
#' @param save_fig Logical. Save plot to file? Default = FALSE. When set to TRUE, saved plots are optimized for 18x12cm.
#' @param save_fig_filename String. Filename of saved plot. Default the plot is saved as {working_dir_path}/{plotname}.png.
#' @return ggplot object. Cumulative Response plot.
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
#' plot_cumresponse()
#' plot_cumresponse(custom_line_colors="pink")
#' plot_cumresponse(highlight_ntile=3)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_ntiles}} for details on the function \code{prepare_scores_and_ntiles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_ntiles}} for details on the function \code{aggregate_over_ntiles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_ntiles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumresponse <- function(data=plot_input,highlight_ntile=NA,highlight_how='plot_text',
                             save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,...) {

  if ('highlight_decile' %in% names(match.call())) {
    warning("parameter highlight_decile is depreciated and replaced by highlight_ntile.")
    highlight_ntile <-match.call(expand.dots = FALSE)$...$highlight_decile
  }

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  if(is.null(custom_plot_text)) custom_plot_text <- quiet(customize_plot_text(plot_input = plot_input))

  pp <- setplotparams(plot_input = plot_input,plottype = "cumresponse",custom_line_colors=custom_line_colors,custom_plot_text=custom_plot_text)
  #plot_input = plot_input
  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=cumpct,legend,refline)
  if (pp$seltype=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(get('response_refline_label',get('cumresponse',custom_plot_text)),' (',dataset_label,')'),model_label='',plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(get('response_refline_label',get('cumresponse',custom_plot_text)),' (',legend,')'),plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$cumresplevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$resplinetypes,guide=ggplot2::guide_legend(ncol=pp$resplegendcolumns))+
    ggplot2::scale_color_manual(values=pp$resplinecols)+
    ggplot2::scale_size_manual(values=pp$resplinesizes)+
    ggplot2::scale_alpha_manual(values=pp$respalphas)+
    ggplot2::scale_y_continuous(name=get('y_axis_label',get(pp$plottype,custom_plot_text)) ,expand = c(0, 0.02),labels = scales::percent) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$plottitle,subtitle=pp$plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10,hjust = 0.5,face="italic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position=c(0.975,0.975),legend.justification=c(1, 1),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("white",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())


  #annotate plot at ntile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$ntiles_label, breaks=seq(0,pp$ntiles,pp$xlabper),
    labels=seq(0,pp$ntiles,pp$xlabper),expand = c(0, 0.02))

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\\\|/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
    cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  } else {
    return(plot)
  }
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_all()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


#' Create plot with all four evaluation plots
#'
#'
#' Generates a layout containing a number graphical elements, including title, subtitle and the four
#' model evaluation plots: cumulative gains plot, lift plot, response plot and cumulative response plot.
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param save_fig Logical. Save plot to file? Default = FALSE. When set to TRUE, saved plot_all is optimized for 36x24cm.
#' @param save_fig_filename String. Filename of saved plot. Default the plot is saved as {working_dir_path}/{plotname}.png.
#' @return gtable, containing 6 grobs.
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
plot_all <- function(data=plot_input,save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,...) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  if(is.null(custom_plot_text)) custom_plot_text <- quiet(customize_plot_text(plot_input = plot_input))

  pp <- setplotparams(plot_input = plot_input,plottype = "multiplot",custom_line_colors=custom_line_colors,custom_plot_text=custom_plot_text)

  # make plot_cumgains without subtitle
  cumgainsplot <- plot_cumgains() + ggplot2::labs(title=get('plottitle',get('cumgains',custom_plot_text)),subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make plot_cumlift without subtitle
  cumliftplot <- plot_cumlift() + ggplot2::labs(title=get('plottitle',get('cumlift',custom_plot_text)),subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make plot_response without subtitle
  responseplot <- plot_response() + ggplot2::labs(title=get('plottitle',get('response',custom_plot_text)),subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank())
  # make plot_cumresponse without subtitle
  cumresponseplot <- plot_cumresponse()+ ggplot2::labs(title=get('plottitle',get('cumresponse',custom_plot_text)),subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank())

  # create title text element to add to grob
  plottitle <- data.frame(text=pp$multiplottitle)
  titletextplot <- ggplot2::ggplot(plottitle,
    ggplot2::aes(label = text, xmin = 0, xmax = 1, ymin = 0,ymax = 1)) +
    ggplot2::geom_rect(fill=NA,color=NA) +
    ggfittext::geom_fit_text(place = "center",grow = TRUE,reflow = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="none",
      line =ggplot2::element_blank(),
      title=ggplot2::element_blank(),
      axis.text=ggplot2::element_blank())

  lay <- rbind(c(1,1,1,1),
    c(1,1,1,1),
    c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),c(2,2,3,3),
    c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5),c(4,4,5,5))

  plot <- gridExtra::arrangeGrob(titletextplot,cumgainsplot,
    cumliftplot,
    responseplot,
    cumresponseplot, layout_matrix = lay)

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\\\|/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
    cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 36, height = 24, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }

  grid::grid.newpage()
  grid::grid.draw(plot)

}
