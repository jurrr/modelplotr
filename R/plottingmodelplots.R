##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### setplotparams()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##


setplotparams <- function(plot_input,plottype,custom_line_colors) {

#  plot_input <- plot_input
#  plottype <- "Lift"
# custom_line_colors <- NA
  pp <- list()

  # ALL PLOTS
  pp$plottype <- plottype
  pp$seltype <- max(as.character(plot_input$scope))
  pp$selmod <- max(as.character(plot_input$modelname))
  pp$seldata <- max(as.character(plot_input$dataset))
  pp$selval <- max(as.character(plot_input$category))
  pp$levels <- unique(as.character(plot_input$legend))
  pp$nlevels <- length(pp$levels)
  pp$randcols <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
  pp$levelcols <- pp$randcols[1:pp$nlevels]
  pp$decile0 <- ifelse(pp$plottype=="Cumulative gains",1,0)
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

  pp$plottitle <- pp$plottype
  pp$plotsubtitle <-
    ifelse(pp$seltype=="compare_datasets",paste0('scope: comparing datasets & model: ',
      pp$selmod,' & target value: ' ,pp$selval),
      ifelse(pp$seltype=="compare_models",paste0('scope: comparing models & dataset: ',
        pp$seldata,' & target value: ',pp$selval),
        ifelse(pp$seltype=="compare_targetclasses",paste0('scope: comparing target classes & dataset: ',
          pp$seldata,'"  &  model: ',pp$selmod),
          paste0('model: ',pp$selmod,'  &  dataset: ',pp$seldata,'"  &  target value: ',pp$selval))))

  pp$multiplottitle <- ifelse(pp$seltype=="compare_datasets",
                          paste0('scope: comparing datasets & model: ',pp$selmod,' & target value: ' ,pp$selval),
                        ifelse(pp$seltype=="compare_models",
                          paste0('scope: comparing models & dataset: ',pp$seldata,' & target value: ',pp$selval),
                        ifelse(pp$seltype=="compare_targetclasses",
                          paste0('scope: comparing target classes & dataset: ',pp$seldata,'  &  model: ',pp$selmod),
                          paste0('scope: no comparison & model: ',
                            pp$selmod,'  &  dataset: ',pp$seldata,'"  &  target value: ',pp$selval))))

  # GAINS
  if (pp$seltype=='compare_models') {
    pp$optgainsreflevels <- paste0('optimal gains (',unique(plot_input$dataset),')')
  } else {
    pp$optgainsreflevels <- paste0('optimal gains (',pp$levels,')')
  }
  pp$noptgainsreflevels <- ifelse(pp$seltype=='compare_models',1,pp$nlevels)
  if (pp$seltype=='compare_models') pp$optgainsreflevelcols <- 'gray' else pp$optgainsreflevelcols <- pp$levelcols
  pp$gainslevels <- c(pp$levels,'minimal gains',pp$optgainsreflevels)
  pp$ngainslevels <- length(pp$gainslevels)
  pp$gainslegendcolumns <- ifelse(pp$ngainslevels>6,2,1)
  pp$gainslinetypes <- c(rep('solid',pp$nlevels),'dashed',rep('dotted',pp$noptgainsreflevels))
  pp$gainsalphas <- c(rep(1,pp$nlevels),1,rep(1,pp$noptgainsreflevels))
  pp$gainslinecols <- c(pp$levelcols,'gray',pp$optgainsreflevelcols)
  pp$gainslinesizes <- c(rep(1,pp$nlevels),0.5,rep(1.2,pp$noptgainsreflevels))

  # LIFT
  pp$liftreflabel <- 'no lift'
  pp$liftlevels <- c(pp$levels,pp$liftreflabel)
  pp$nliftlevels <- length(pp$liftlevels)
  pp$liftlegendcolumns <- ifelse(pp$nliftlevels>6,2,1)
  pp$liftlinetypes <- c(rep('solid',pp$nlevels),'dashed')
  pp$liftalphas <- c(rep(1,pp$nlevels),1)
  pp$liftlinecols <- c(pp$levelcols,'gray')
  pp$liftlinesizes <- c(rep(1,pp$nlevels),0.5)

  # RESPONSE
  if (pp$seltype=='compare_models') {
    pp$respreflevels <- paste0('overall response (',unique(plot_input$dataset),')')
  } else {
    pp$respreflevels <- paste0('overall response (',pp$levels,')')
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

  return(pp)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### annotate_plot()              ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

annotate_plot <- function(plot=plot,plot_input=plot_input_prepared,
                          highlight_decile=highlight_decile,highlight_how=highlight_how,pp=pp){
  if(!is.na(highlight_decile)) {

    # check if scores_and_deciles exists, otherwise create
    if (highlight_decile<1 | highlight_decile>10) {
      stop("Value for highlight_decile not valid! Choose decile value to highlight in range [1:10]")
    }
    if(!highlight_how %in% c('plot','text','plot_text')){
      cat("no valid value for highlight_how specified; default value (plot_text) is chosen
-> choose 'plot_text' to highlight both the plot and add explanatory text below the plot
-> choose 'plot' to only highlight both the plot - no explanatory text is added below the plot
-> choose 'text' to only add explanatory text below the plot - the chosen decile is not highlighted in the plot \n")
      highlight_how <- 'plot_text'
    }

    if(highlight_how %in% c('plot','plot_text')){
    plot <- plot +
      # add highlighting cicle(s) to plot at decile value
      ggplot2::geom_point(data = plot_input %>% dplyr::filter(decile==highlight_decile & refline==0),
        ggplot2::aes(x=decile,y=plotvalue,color=legend),shape=1,size=5,show.legend = FALSE)+
      # add line(s) from annotated point(s) to Y axis
      ggplot2::geom_segment(data = plot_input %>% dplyr::filter(decile==highlight_decile & refline==0),
        ggplot2::aes(x=-Inf,y=plotvalue,xend=decile+0.5,yend=plotvalue,colour=legend),
        linetype="dotted",size=0.5,show.legend = FALSE)+
      # add line(s) from annotated point(s) to X axis
      ggplot2::geom_segment(data = plot_input %>% dplyr::filter(decile==highlight_decile & refline==0),
        ggplot2::aes(x=decile,y=0,xend=decile,yend=plotvalue+0.05),colour="gray",
        linetype="dotted",size=1,show.legend = FALSE) +
      # add value labels for annotated points to X axis
      ggplot2::coord_cartesian(clip = 'off' )+
      ggplot2::geom_label(data=plot_input %>% dplyr::filter(decile==highlight_decile & refline==0),
        ggplot2::aes(x=-Inf,y=plotvalue,label = sprintf("%1.0f%%", 100*plotvalue),color=legend),fill="white",
        hjust = 0, fontface = "bold",show.legend = FALSE) +
      # emphasize decile for which annotation is added
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          face=c(rep("plain",pp$decile0+highlight_decile-1),"bold",rep("plain",10+pp$decile0-highlight_decile-1)),
          size=c(rep(10,pp$decile0+highlight_decile-1),12,rep(10,10+pp$decile0-highlight_decile-1))))
    }
    annovalues <- plot_input %>% dplyr::filter(decile==highlight_decile & refline==0) %>%
      dplyr::mutate(xmin=rep(0,pp$nlevels),
      xmax=rep(100,pp$nlevels),
      ymin=seq(1,pp$nlevels,1),
      ymax=seq(2,pp$nlevels+1,1),
      gainstext = paste0("When we select ",highlight_decile*10,"% with the highest probability according to ",
        modelname,", this selection holds ",sprintf("%1.0f%%", 100*plotvalue),
        " of all ",category," cases in ",dataset,"."),
      lifttext = paste0("When we select ",highlight_decile*10,"% with the highest probability according to model ",
          modelname," in ",dataset,", this selection for ",category," cases is ",sprintf("%1.1f", plotvalue),
          ' times better than selecting without a model.'),
      responsetext = paste0("When we select decile ",highlight_decile," according to model ",
          modelname," in dataset ",dataset," the % of ",category," cases in the selection is ",
          sprintf("%1.0f%%", 100*plotvalue),"."),
      cumresponsetext = paste0("When we select deciles 1 until ",highlight_decile," according to model ",
        modelname," in dataset ",dataset," the % of ",category," cases in the selection is ",
        sprintf("%1.0f%%", 100*plotvalue),".")
      )

    if(pp$plottype=="Cumulative gains") annovalues$text <- annovalues$gainstext
    if(pp$plottype=="Cumulative lift") annovalues$text <- annovalues$lifttext
    if(pp$plottype=="Response") annovalues$text <- annovalues$responsetext
    if(pp$plottype=="Cumulative response") annovalues$text <- annovalues$cumresponsetext

    cat(paste(' ','Plot annotation:',paste(paste0('- ',annovalues$text), collapse = '\n'),' ',' ', sep = '\n'))

    if(highlight_how %in% c('text','plot_text')){
      # create annotation text element to add to grob
      annotextplot <- ggplot2::ggplot(annovalues,
      ggplot2::aes(label = text, xmin = xmin, xmax = xmax, ymin = ymin,ymax = ymax,color=legend)) +
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
      cat('Plotted output is a TableGrob object with these characteristics: \n')
      plot <- gridExtra::arrangeGrob(title,subtitle,plot,annotextplot, layout_matrix = lay,
        widths = grid::unit(18, "cm"),heights = grid::unit(rep(12/(23+pp$nlevels),23+pp$nlevels), "cm"))
      }
  }
  return(plot)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumgains()                   ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative gains plot
#'
#' Generates the cumulative gains plot. This plot, often referred to as the gains chart,
#' helps answering the question: \bold{\emph{When we apply the model and select the best X deciles,
#' what percentage of the actual target class observations can we expect to target?}}
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_decile Integer. Specifying the decile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the decile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen decile.
#' "text" adds text below the plot explaining the plot at chosen decile but does not highlight the plot.
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
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' prepare_scores_and_deciles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_deciles)
#' aggregate_over_deciles()
#' plotting_scope(scope="compare_models")
#' plot_cumgains()
#' plot_cumgains(custom_line_colors=c("orange","purple"))
#' plot_cumgains(highlight_decile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_deciles}} for details on the function \code{prepare_scores_and_deciles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_deciles}} for details on the function \code{aggregate_over_deciles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_deciles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumgains <- function(data=plot_input,custom_line_colors=NA,highlight_decile=NA,highlight_how='plot_text',
                          save_fig=FALSE,save_fig_filename=NA) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_decile <- highlight_decile
  highlight_how <- highlight_how

  pp <- setplotparams(plot_input = plot_input,plottype = "Cumulative gains",custom_line_colors=custom_line_colors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::select(scope:decile,plotvalue=cumgain,legend,refline)
  if (pp$seltype=="compare_models") {
    optreflines <- plot_input %>%
      dplyr::mutate(legend=paste0('optimal gains (',dataset,')'),modelname='',plotvalue=gain_opt,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    optreflines <- plot_input%>%
      dplyr::mutate(legend=paste0('optimal gains (',legend,')'),plotvalue=gain_opt,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline)
  }
  minrefline <- plot_input %>%
    dplyr::mutate(legend=paste0('minimal gains'),modelname='',dataset='',category='',plotvalue=gain_ref,refline=1) %>%
    dplyr::select(scope:decile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,optreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$gainslevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$gainslinetypes,guide=ggplot2::guide_legend(ncol=pp$gainslegendcolumns))+
    ggplot2::scale_color_manual(values=pp$gainslinecols)+
    ggplot2::scale_size_manual(values=pp$gainslinesizes)+
    ggplot2::scale_alpha_manual(values=pp$gainsalphas)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="cumulative gains",breaks=seq(0,1,0.2),labels = scales::percent ,expand = c(0, 0.02)) +
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

  #annotate plot at decile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_decile=highlight_decile,highlight_how=highlight_how,pp=pp)

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\|\\/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      print("No filename specified! Specify 'save_fig_filename' to customize location and name.")
    }
    print(paste0("Plot is saved as: ",filename))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  }
  return(plot)
  }





##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumlift()                       ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative Lift plot
#'
#' Generates the cumulative lift plot, often referred to as lift plot or index plot,
#' helps you answer the question: When we apply the model and select the best X deciles,
#' how many times better is that than using no model at all?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_decile Integer. Specifying the decile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the decile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen decile.
#' "text" adds text below the plot explaining the plot at chosen decile but does not highlight the plot.
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
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' prepare_scores_and_deciles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_deciles)
#' aggregate_over_deciles()
#' plotting_scope(scope="compare_datasets")
#' plot_cumlift()
#' plot_cumlift(custom_line_colors=c("orange","purple"))
#' plot_cumlift(highlight_decile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_deciles}} for details on the function \code{prepare_scores_and_deciles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_deciles}} for details on the function \code{aggregate_over_deciles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_deciles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumlift <- function(data=plot_input,custom_line_colors=NA,highlight_decile=NA,highlight_how='plot_text',
                         save_fig=FALSE,save_fig_filename=NA) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_decile <- highlight_decile

  pp <- setplotparams(plot_input = plot_input,plottype = "Cumulative lift",custom_line_colors=custom_line_colors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(decile>0) %>%
    dplyr::select(scope:decile,plotvalue=cumlift,legend,refline)
  minrefline <- plot_input %>% dplyr::filter(decile>0) %>%
    dplyr::mutate(legend=pp$liftreflabel,modelname='',dataset='',category='',plotvalue=cumlift_ref,refline=1) %>%
    dplyr::select(scope:decile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$liftlevels)


  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$liftlinetypes,guide=ggplot2::guide_legend(ncol=pp$liftlegendcolumns))+
    ggplot2::scale_color_manual(values=pp$liftlinecols)+
    ggplot2::scale_size_manual(values=pp$liftlinesizes)+
    ggplot2::scale_alpha_manual(values=pp$liftalphas)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="cumulative lift" ,labels = scales::percent,expand = c(0, 0.02)) +
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

  #annotate plot at decile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_decile=highlight_decile,highlight_how=highlight_how,pp=pp)

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\|\\/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      print("No filename specified! Specify 'save_fig_filename' to customize location and name.")
    }
    print(paste0("Plot is saved as: ",filename))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  }
  return(plot)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_response()                   ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Response plot
#'
#' Generates the response plot. It plots the percentage of target class observations
#' per decile. It can be used to answer the following business question: When we apply
#' the model and select decile X, what is the expected percentage of target class observations
#' in that decile?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_decile Integer. Specifying the decile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the decile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen decile.
#' "text" adds text below the plot explaining the plot at chosen decile but does not highlight the plot.
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
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' prepare_scores_and_deciles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_deciles)
#' aggregate_over_deciles()
#' plotting_scope(scope="compare_targetclasses")
#' plot_response()
#' plot_response(custom_line_colors=RColorBrewer::brewer.pal(3,"Dark2"))
#' plot_response(highlight_decile=2)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_deciles}} for details on the function \code{prepare_scores_and_deciles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_deciles}} for details on the function \code{aggregate_over_deciles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_deciles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_response <- function(data=plot_input,custom_line_colors=NA,highlight_decile=NA,highlight_how='plot_text',
                          save_fig=FALSE,save_fig_filename=NA) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_decile <- highlight_decile

  pp <- setplotparams(plot_input = plot_input,plottype = "Response",custom_line_colors=custom_line_colors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(decile>0) %>%
    dplyr::select(scope:decile,plotvalue=pct,legend,refline)
  if (pp$seltype=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(decile>0) %>%
      dplyr::mutate(legend=paste0('overall response (',dataset,')'),modelname='',plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input%>%
      dplyr::filter(decile>0) %>%
      dplyr::mutate(legend=paste0('overall response (',legend,')'),plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$resplevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$resplinetypes,guide=ggplot2::guide_legend(ncol=pp$resplegendcolumns))+
    ggplot2::scale_color_manual(values=pp$resplinecols)+
    ggplot2::scale_size_manual(values=pp$resplinesizes)+
    ggplot2::scale_alpha_manual(values=pp$respalphas)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="response" ,expand = c(0, 0.02),labels = scales::percent) +
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


  #annotate plot at decile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_decile=highlight_decile,highlight_how=highlight_how,pp=pp)

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\|\\/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      print("No filename specified! Specify 'save_fig_filename' to customize location and name.")
    }
    print(paste0("Plot is saved as: ",filename))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  }

  return(plot)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_cumresponse()                ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

#' Cumulative Respose plot
#'
#' Generates the cumulative response plot. It plots the cumulative percentage of
#' target class observations up until that decile. It helps answering the question:
#' When we apply the model and select up until decile X, what is the expected percentage of
#' target class observations in the selection?
#' @param data Dataframe. Dataframe needs to be created with \code{\link{plotting_scope}}
#' or else meet required input format.
#' @param custom_line_colors Vector of Strings. Specifying colors for the lines in the plot.
#' When not specified, colors from the RColorBrewer palet "Set1" are used.
#' @param highlight_decile Integer. Specifying the decile at which the plot is annotated
#' and/or performances are highlighted.
#' @param highlight_how String. How to annotate the plot. Possible values: "plot_text","plot", "text".
#' Default is "plot_text", both highlighting the decile and value on the plot as well as in text below the plot.
#' "plot" only highligths the plot, but does not add text below the plot explaining the plot at chosen decile.
#' "text" adds text below the plot explaining the plot at chosen decile but does not highlight the plot.
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
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' prepare_scores_and_deciles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_deciles)
#' aggregate_over_deciles()
#' plotting_scope()
#' plot_cumresponse()
#' plot_cumresponse(custom_line_colors="pink")
#' plot_cumresponse(highlight_decile=3)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{modelplotr}} for generic info on the package \code{moddelplotr}
#' @seealso \code{\link{prepare_scores_and_deciles}} for details on the function \code{prepare_scores_and_deciles}
#' that generates the required input.
#' @seealso \code{\link{aggregate_over_deciles}} for details on the function \code{aggregate_over_deciles} that
#' generates the required input.
#' @seealso \code{\link{plotting_scope}} for details on the function \code{plotting_scope} that
#' filters the output of \code{aggregate_over_deciles} to prepare it for the required evaluation.
#' @seealso \url{https://github.com/modelplot/modelplotr} for details on the package
#' @seealso \url{https://modelplot.github.io/} for our blog on the value of the model plots
plot_cumresponse <- function(data=plot_input,custom_line_colors=NA,highlight_decile=NA,highlight_how='plot_text',
                             save_fig=FALSE,save_fig_filename=NA) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_decile <- highlight_decile

  pp <- setplotparams(plot_input = plot_input,plottype = "Cumulative response",custom_line_colors=custom_line_colors)
  #plot_input = plot_input
  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(decile>0) %>%
    dplyr::select(scope:decile,plotvalue=cumpct,legend,refline)
  if (pp$seltype=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(decile>0) %>%
      dplyr::mutate(legend=paste0('overall response (',dataset,')'),modelname='',plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input %>%
      dplyr::filter(decile>0) %>%
      dplyr::mutate(legend=paste0('overall response (',legend,')'),plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:decile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$resplevels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$resplinetypes,guide=ggplot2::guide_legend(ncol=pp$resplegendcolumns))+
    ggplot2::scale_color_manual(values=pp$resplinecols)+
    ggplot2::scale_size_manual(values=pp$resplinesizes)+
    ggplot2::scale_alpha_manual(values=pp$respalphas)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="cumulative response" ,expand = c(0, 0.02),labels = scales::percent) +
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


  #annotate plot at decile value
  plot <- annotate_plot(plot=plot,plot_input = plot_input_prepared,
                        highlight_decile=highlight_decile,highlight_how=highlight_how,pp=pp)

  #save plot when requested
  if(save_fig) {
    if(!is.na(save_fig_filename)){
      if(!grepl("\\.[a-zA-Z]{3,4}",save_fig_filename)) save_fig_filename <- paste0(save_fig_filename,'.png')
      if(grepl("\\|\\/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      print("No filename specified! Specify 'save_fig_filename' to customize location and name.")
    }
    print(paste0("Plot is saved as: ",filename))
    ggplot2::ggsave(file=filename,plot=plot,width = 18, height = 12, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }
  if(length(plot$layout)>0) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  }

  return(plot)
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
#' trainTask <- mlr::makeClassifTask(data = train, target = "Species")
#' testTask <- mlr::makeClassifTask(data = test, target = "Species")
#' mlr::configureMlr() # this line is needed when using mlr without loading it (mlr::)
#' #estimate models
#' task = mlr::makeClassifTask(data = train, target = "Species")
#' lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
#' rf = mlr::train(lrn, task)
#' lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
#' mnl = mlr::train(lrn, task)
#' prepare_scores_and_deciles(datasets=list("train","test"),
#'                       dataset_labels = list("train data","test data"),
#'                       models = list("rf","mnl"),
#'                       model_labels = list("random forest","multinomial logit"),
#'                       target_column="Species")
#' head(scores_and_deciles)
#' aggregate_over_deciles()
#' plotting_scope()
#' plot_cumgains()
#' plot_cumlift()
#' plot_response()
#' plot_cumresponse()
#' plot_all()
#' @export
plot_all <- function(data=plot_input,custom_line_colors=NA,save_fig=FALSE,save_fig_filename=NA) {

  plot_input <- data
  custom_line_colors <- custom_line_colors
  pp <- setplotparams(plot_input = plot_input,plottype = "ALL",custom_line_colors=custom_line_colors)

  # make plot_cumgains without
  cumgainsplot <- plot_cumgains() + ggplot2::labs(title="Cumulative gains",subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make lift
  cumliftplot <- plot_cumlift() + ggplot2::labs(title="Cumulative lift",subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make response
  responseplot <- plot_response() + ggplot2::labs(title="Response",subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank())
  # make gains
  cumresponseplot <- plot_cumresponse()+ ggplot2::labs(title="Cumulative response",subtitle=NA) +
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
      if(grepl("\\|\\/",save_fig_filename)) {
        filename <- save_fig_filename
      } else {
        filename <- paste0(getwd(),'/',save_fig_filename)
      }
    } else {
      filename <-   paste0(getwd(),'/',pp$plottype,'.png')
      print("No filename specified! Specify 'save_fig_filename' to customize location and name.")
    }
    print(paste0("Plot is saved as: ",filename))
    ggplot2::ggsave(file=filename,plot=plot,width = 36, height = 24, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }

  grid::grid.newpage()
  grid::grid.draw(plot)

}

