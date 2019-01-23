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

  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  pp <- setplotparams(plot_input = plot_input,plottype = "cumgains",custom_line_colors=custom_line_colors,plot_text=plot_text)

  # rearrange plot_input

  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::select(scope:ntile,plotvalue=cumgain,legend,refline)

  if (pp$scope$scope=="compare_models") {
    optreflines <- plot_input %>%
      dplyr::mutate(legend=paste0(pp$cumgains$optimal_gains_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=gain_opt,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    optreflines <- plot_input%>%
      dplyr::mutate(legend=paste0(pp$cumgains$optimal_gains_label,' (',legend,')'),
                    plotvalue=gain_opt,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline)
  }

  minrefline <- plot_input %>%
    dplyr::mutate(legend=paste0(pp$cumgains$minimal_gains_label),
                  model_label='',
                  dataset_label='',
                  target_class='',
                  plotvalue=gain_ref,
                  refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,optreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$cumgains$levels)


  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$cumgains$linetypes,guide=ggplot2::guide_legend(ncol=pp$cumgains$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$cumgains$linecols)+
    ggplot2::scale_size_manual(values=pp$cumgains$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$cumgains$alphas) +
    ggplot2::scale_y_continuous(name=pp$cumgains$y_axis_label,breaks=seq(0,1,0.2),labels = scales::percent ,expand = c(0, 0.02)) +
    ggplot2::labs(title=pp$cumgains$plottitle,subtitle=pp$cumgains$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$cumgains$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
                                                              labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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

  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  pp <- setplotparams(plot_input = plot_input,plottype = "cumlift",custom_line_colors=custom_line_colors,plot_text=plot_text)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=cumlift,legend,refline)
  minrefline <- plot_input %>% dplyr::filter(ntile>0) %>%
    dplyr::mutate(legend=pp$cumlift$lift_refline_label,
                  model_label='',dataset_label='',target_class='',plotvalue=cumlift_ref,refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()
  plot_input_prepared <- rbind(minrefline,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$cumlift$levels)


  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$cumlift$linetypes,guide=ggplot2::guide_legend(ncol=pp$cumlift$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$cumlift$linecols)+
    ggplot2::scale_size_manual(values=pp$cumlift$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$cumlift$alphas) +
    ggplot2::scale_y_continuous(name=pp$cumlift$y_axis_label,labels = scales::percent,expand = c(0, 0.02)) +
    ggplot2::expand_limits(y=c(0,max(2,max(plot_input_prepared$plotvalue,na.rm = T)))) +
    ggplot2::labs(title=pp$cumlift$plottitle,subtitle=pp$cumlift$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$cumlift$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
    labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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

  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  pp <- setplotparams(plot_input = plot_input,plottype = "response",custom_line_colors=custom_line_colors,plot_text=plot_text)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=pct,legend,refline)
  if (pp$scope$scope=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$response$response_refline_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=pcttot,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input%>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$response$response_refline_label,' (',legend,')'),
                    plotvalue=pcttot,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$response$levels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$response$linetypes,guide=ggplot2::guide_legend(ncol=pp$response$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$response$linecols)+
    ggplot2::scale_size_manual(values=pp$response$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$response$alphas)+
    ggplot2::scale_y_continuous(name=pp$response$y_axis_label ,expand = c(0, 0.02),labels = scales::percent) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$response$plottitle,subtitle=pp$response$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$response$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
    labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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

  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  pp <- setplotparams(plot_input = plot_input,plottype = "cumresponse",custom_line_colors=custom_line_colors,plot_text=plot_text)
  #plot_input = plot_input
  # rearrange plot_input
  vallines <- plot_input %>% dplyr::mutate(refline=0) %>% dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=cumpct,legend,refline)
  if (pp$scope$scope=="compare_models") {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$cumresponse$response_refline_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=pcttot,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    minreflines <- plot_input %>%
      dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$cumresponse$response_refline_label,' (',legend,')'),plotvalue=pcttot,refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  }
  plot_input_prepared <- rbind(minreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$cumresponse$levels)

  #make plot
  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$cumresponse$linetypes,
                                   guide=ggplot2::guide_legend(ncol=pp$cumresponse$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$cumresponse$linecols)+
    ggplot2::scale_size_manual(values=pp$cumresponse$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$cumresponse$alphas)+
    ggplot2::scale_y_continuous(name=pp$cumresponse$y_axis_label ,expand = c(0, 0.02),labels = scales::percent) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$cumresponse$plottitle,subtitle=pp$cumresponse$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)
  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$cumresponse$ntiles_label, breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
    labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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

  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  pp <- setplotparams(plot_input = plot_input,plottype = "multiplot",custom_line_colors=custom_line_colors,plot_text=plot_text)

  # make plot_cumgains without subtitle
  cumgainsplot <- plot_cumgains() + ggplot2::labs(title=pp$cumgains$plottitle,subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make plot_cumlift without subtitle
  cumliftplot <- plot_cumlift() + ggplot2::labs(title=pp$cumlift$plottitle,subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank())
  # make plot_response without subtitle
  responseplot <- plot_response() + ggplot2::labs(title=pp$response$plottitle,subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank())
  # make plot_cumresponse without subtitle
  cumresponseplot <- plot_cumresponse()+ ggplot2::labs(title=pp$cumresponse$plottitle,subtitle=NA) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face="bold",hjust = 0.5),
      plot.subtitle = ggplot2::element_blank())

  # create title text element to add to grob
  plottitle <- data.frame(text=pp$multiplot$plottitle)
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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
      cat("No filename for saved plot specified! Specify 'save_fig_filename' to customize location and name.\n")
    }
    cat(paste0("Plot is saved as: ",filename,"\n\n"))
    ggplot2::ggsave(file=filename,plot=plot,width = 36, height = 24, units = "cm",dpi=320)
    #ggplot2::ggsave(file=filename,plot=plot)
  }

  grid::grid.newpage()
  grid::grid.draw(plot)

}




##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
#### plot_roi()              ####
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
#' @export
plot_roi <- function(data=plot_input,highlight_ntile='max',highlight_how='plot_text',
                     save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,
                     fixed_costs,variable_costs_per_unit,profit_per_unit,...){

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  # check if plot text customization is requested, else generate default plot text elements
  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  # generate plot parameters
  pp <- setplotparams(plot_input = plot_input,plottype = "roi",custom_line_colors=custom_line_colors,plot_text=plot_text)


  #data <- plot_input
  #plot_input <- data
  #fixed_costs <- 25000+6500
  #variable_costs_per_unit <- (65000+113000)/(420000)
  #profit_per_unit <- 100


  # add extra financial measures to plot_input

  plot_input <- plot_input %>% dplyr::mutate(fixed_costs = fixed_costs,
                                      variable_costs = variable_costs_per_unit*cumtot,
                                      investments = fixed_costs + variable_costs,
                                      revenues = profit_per_unit*cumpos,
                                      profit = revenues-investments,
                                      roi = profit/investments,
                                      variable_costs_tot =variable_costs_per_unit*tottot,
                                      investments_tot = fixed_costs + variable_costs_tot,
                                      revenues_tot = profit_per_unit*postot,
                                      profit_tot = revenues_tot-investments_tot,
                                      roi_ref = profit_tot/investments_tot)

  # rearrange plot_input

  vallines <- plot_input %>%
    dplyr::mutate(refline=0) %>%
    dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=roi,legend,refline)

  if (pp$scope$scope=="compare_models") {
    overallreflines <- plot_input %>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$roi$roi_overall_refline_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=roi_ref,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    overallreflines <- plot_input%>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$roi$roi_overall_refline_label,' (',legend,')'),
                    plotvalue=roi_ref,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline)
  }

  breakevenrefline <- plot_input %>% dplyr::filter(ntile>0) %>%
    dplyr::mutate(legend=pp$roi$roi_breakeven_refline_label,
                  model_label='',dataset_label='',target_class='',plotvalue=0,refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()

  plot_input_prepared <- rbind(breakevenrefline,overallreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$roi$levels)

  #make roi plot

  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$roi$linetypes,guide=ggplot2::guide_legend(ncol=pp$roi$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$roi$linecols)+
    ggplot2::scale_size_manual(values=pp$roi$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$roi$alphas)+
    ggplot2::scale_y_continuous(name=pp$roi$y_axis_label,labels = scales::percent ,expand = c(0, 0.5)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$roi$plottitle,subtitle=pp$roi$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)

  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$roi$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
                                                                        labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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
#### plot_profit()              ####
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
#' @export
plot_profit <- function(data=plot_input,highlight_ntile='max',highlight_how='plot_text',
                     save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,
                     fixed_costs,variable_costs_per_unit,profit_per_unit,...){

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  # check if plot text customization is requested, else generate default plot text elements
  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  # generate plot parameters
  pp <- setplotparams(plot_input = plot_input,plottype = "profit",custom_line_colors=custom_line_colors,plot_text=plot_text)


  #data <- plot_input
  #plot_input <- data
  #fixed_costs <- 25000+6500
  #variable_costs_per_unit <- (65000+113000)/(420000)
  #profit_per_unit <- 100


  # add extra financial measures to plot_input

  plot_input <- plot_input %>% dplyr::mutate(fixed_costs = fixed_costs,
                                             variable_costs = variable_costs_per_unit*cumtot,
                                             investments = fixed_costs + variable_costs,
                                             revenues = profit_per_unit*cumpos,
                                             profit = revenues-investments,
                                             roi = profit/investments,
                                             variable_costs_tot =variable_costs_per_unit*tottot,
                                             investments_tot = fixed_costs + variable_costs_tot,
                                             revenues_tot = profit_per_unit*postot,
                                             profit_tot = revenues_tot-investments_tot,
                                             roi_ref = profit_tot/investments_tot)

  # rearrange plot_input

  vallines <- plot_input %>%
    dplyr::mutate(refline=0) %>%
    dplyr::filter(ntile>0) %>%
    dplyr::select(scope:ntile,plotvalue=profit,legend,refline)

  if (pp$scope$scope=="compare_models") {
    overallreflines <- plot_input %>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$profit$profit_overall_refline_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=profit_tot,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    overallreflines <- plot_input%>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$profit$profit_overall_refline_label,' (',legend,')'),
                    plotvalue=profit_tot,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline)
  }

  breakevenrefline <- plot_input %>% dplyr::filter(ntile>0) %>%
    dplyr::mutate(legend=pp$profit$profit_breakeven_refline_label,
                  model_label='',dataset_label='',target_class='',plotvalue=0,refline=1) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)%>%
    dplyr::distinct()

  plot_input_prepared <- rbind(breakevenrefline,overallreflines,vallines)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$profit$levels)

  #make profit plot

  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$profit$linetypes,guide=ggplot2::guide_legend(ncol=pp$profit$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$profit$linecols)+
    ggplot2::scale_size_manual(values=pp$profit$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$profit$alphas)+
    ggplot2::scale_y_continuous(name=pp$profit$y_axis_label,labels = scales::dollar_format(prefix = "€"), expand = c(0, 0.5)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$profit$plottitle,subtitle=pp$profit$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)

  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$cumgains$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
                                                                        labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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
#### plot_costsrevs()              ####
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
#' @export
plot_costsrevs <- function(data=plot_input,highlight_ntile=NA,highlight_how='plot_text',
                        save_fig=FALSE,save_fig_filename=NA,custom_line_colors=NA,custom_plot_text=NULL,
                        fixed_costs,variable_costs_per_unit,profit_per_unit,...){

  plot_input <- data
  custom_line_colors <- custom_line_colors
  highlight_ntile <- highlight_ntile
  highlight_how <- highlight_how

  # check if plot text customization is requested, else generate default plot text elements
  if(is.null(custom_plot_text)) {
    plot_text <- quiet(customize_plot_text(plot_input = plot_input))
  } else {
    plot_text <- custom_plot_text
  }

  # generate plot parameters
  pp <- setplotparams(plot_input = plot_input,plottype = "costsrevs",custom_line_colors=custom_line_colors,plot_text=plot_text)


  #data <- plot_input
  #plot_input <- data
  #fixed_costs <- 32000
  #variable_costs_per_unit <- 10
  #profit_per_unit <- 100
  #library(magrittr)

  # add extra financial measures to plot_input

  plot_input <- plot_input %>% dplyr::mutate(fixed_costs = fixed_costs,
                                             variable_costs = variable_costs_per_unit*cumtot,
                                             investments = fixed_costs + variable_costs,
                                             revenues = profit_per_unit*cumpos,
                                             profit = revenues-investments,
                                             roi = profit/investments,
                                             variable_costs_tot =variable_costs_per_unit*tottot,
                                             investments_tot = fixed_costs + variable_costs_tot,
                                             revenues_tot = profit_per_unit*postot,
                                             profit_tot = revenues_tot-investments_tot,
                                             roi_ref = profit_tot/investments_tot)

  # rearrange plot_input

  vallines_revenues <- plot_input %>%
    dplyr::filter(ntile>0) %>%
    dplyr::mutate(legend=paste0(pp$costsrevs$revenues_label,' (',legend,')'),
                  model_label='',
                  plotvalue=revenues,
                  refline=0) %>%
    dplyr::select(scope:ntile,plotvalue,legend,refline)


  if (pp$scope$scope=="compare_models") {
    vallines_costs <- plot_input %>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$costsrevs$costs_label,' (',dataset_label,')'),
                    model_label='',
                    plotvalue=investments,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline) %>%
      dplyr::distinct()
  } else {
    vallines_costs <- plot_input%>% dplyr::filter(ntile>0) %>%
      dplyr::mutate(legend=paste0(pp$costsrevs$costs_label,' (',legend,')'),
                    plotvalue=investments,
                    refline=1) %>%
      dplyr::select(scope:ntile,plotvalue,legend,refline)
  }


  plot_input_prepared <- rbind(vallines_revenues,vallines_costs)
  plot_input_prepared$legend <- factor(plot_input_prepared$legend,levels=pp$costsrevs$levels)

  #make profit plot

  plot <- plot_input_prepared %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=ntile,y=plotvalue, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$costsrevs$linetypes,guide=ggplot2::guide_legend(ncol=pp$costsrevs$legendcolumns))+
    ggplot2::scale_color_manual(values=pp$costsrevs$linecols)+
    ggplot2::scale_size_manual(values=pp$costsrevs$linesizes)+
    ggplot2::scale_alpha_manual(values=pp$costsrevs$alphas)+
    ggplot2::scale_y_continuous(name=pp$costsrevs$y_axis_label,labels = scales::dollar_format(prefix = "€"), expand = c(0, 0.5)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$costsrevs$plottitle,subtitle=pp$costsrevs$plotsubtitle) +
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
  plot <- annotate_plot(plot=plot,highlight_input = plot_input_prepared,
                        highlight_ntile=highlight_ntile,highlight_how=highlight_how,pp=pp)

  #add x axis labels when no annotation is applied
  if(is.na(highlight_ntile)) plot <- plot + ggplot2::scale_x_continuous(name=pp$cumgains$ntiles_label,
                                                                        breaks=seq(0,pp$scope$ntiles,pp$scope$xlabper),
                                                                        labels=seq(0,pp$scope$ntiles,pp$scope$xlabper),expand = c(0, 0.02))

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
      filename <-   paste0(getwd(),'/',pp$scope$plottype,'.png')
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

