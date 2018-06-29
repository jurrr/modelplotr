# multiplot


#' Create multiplot
#'
#'
#' @param plotlist List of plot objects.
#' @param file ???
#' @param cols Integer. Number of columns
#' @param layout ???
#' @return Multiplot constisting of plots in \code{plotlist}.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
      ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col))
    }
  }
}

setplotparams <- function(plot_input,plottype,customlinecolors) {

#  plot_input <- eval_t_type
#  plottype <- "Lift"
  pp <- list()

  # ALL PLOTS
  pp$plottype <- plottype
  pp$seltype <- max(as.character(plot_input$eval_type))
  pp$selmod <- max(as.character(plot_input$modelname))
  pp$seldata <- max(as.character(plot_input$dataset))
  pp$selval <- max(as.character(plot_input$category))
  pp$levels <- levels(plot_input$legend)
  pp$nlevels <- length(pp$levels)
  pp$randcols <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
  pp$levelcols <- pp$randcols[1:pp$nlevels]
  if(!is.na(customlinecolors) & length(customlinecolors)==pp$nlevels) {
    pp$levelcols <- customlinecolors
  } else if (!is.na(customlinecolors)) {
    print('specified customlinecolors vector not of required length! \
      It is cropped or extended with extra colors to match required length')
    pp$lencustcols <- length(customlinecolors)
    pp$levelcols <- c(customlinecolors[1:pp$nlevels],
      randcols[which(!pp$randcols %in% customlinecolors)][1:(pp$nlevels-pp$lencustcols)])
  } else {
    pp$levelcols <- pp$randcols[1:pp$nlevels]
  }
  pp$legendcolumns <- ifelse(pp$nnewlevels>6,2,1)
  pp$linetypes <- c(rep('solid',pp$nlevels),'dashed')
  pp$alphas <- c(rep(1,pp$nlevels),1)
  pp$linecols <- c(pp$levelcols,'gray')
  pp$linesizes <- c(rep(1,pp$nlevels),0.5)

  pp$plottitle <- paste0(pp$plottype,' chart',
    ifelse(pp$seltype=="CompareDatasets",' - comparing datasets',
      ifelse(pp$seltype=="CompareModels",' - comparing models',
        ifelse(pp$seltype=="CompareTargetValues",' - comparing target values',''))))
  pp$plotsubtitle <-
    ifelse(pp$seltype=="CompareDatasets",paste0('model: "',pp$selmod,'"  &  target value: "' ,pp$selval,'"'),
      ifelse(pp$seltype=="CompareModels",paste0('dataset: "',pp$seldata,'"  &  target value: "',pp$selval,'"'),
        ifelse(pp$seltype=="CompareTargetValues",paste0('dataset: "',pp$seldata,'"  &  model: "',pp$selmod,'"'),
          paste0('model: "',pp$selmod,'"  &  dataset: "',pp$seldata,'"  &  target value: "',pp$selval,'"'))))

  # GAINS
  if (pp$seltype=='CompareModels') {
    pp$optgainsreflevels <- paste0('optimal gains (',unique(plot_input$dataset),')')
  } else {
    pp$optgainsreflevels <- paste0('optimal gains (',pp$levels,')')
  }
  pp$noptgainsreflevels <- ifelse(pp$seltype=='CompareModels',1,pp$nlevels)
  if (pp$seltype=='CompareModels') pp$optgainsreflevelcols <- 'gray' else pp$optgainsreflevelcols <- pp$levelcols
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
  if (pp$seltype=='CompareModels') {
    pp$respreflevels <- paste0('overall response (',unique(plot_input$dataset),')')
  } else {
    pp$respreflevels <- paste0('overall response (',pp$levels,')')
  }
  pp$nrespreflevels <- ifelse(pp$seltype=='CompareModels',1,pp$nlevels)
  if (pp$seltype=='CompareModels') pp$respreflevelcols <- 'gray' else pp$respreflevelcols <- pp$levelcols
  pp$resplevels <- c(pp$levels,pp$respreflevels)
  pp$nresplevels <- length(pp$resplevels)
  pp$resplegendcolumns <- ifelse(pp$nresplevels>6,2,1)
  pp$resplinetypes <- c(rep('solid',pp$nlevels),rep('dashed',pp$nrespreflevels))
  pp$respalphas <- c(rep(1,pp$nlevels),rep(1,pp$nrespreflevels))
  pp$resplinecols <- c(pp$levelcols,pp$respreflevelcols)
  pp$resplinesizes <- c(rep(1,pp$nlevels),rep(0.8,pp$nrespreflevels))

  return(pp)
}


#' Cumulative gains plot
#'
#'
#' @param plot_input Dataframe. Dataframe needs to be created with input_modevalplots
#' or else meet required input format (see link to format in "See Also" section)
#' @param customlinecolors String.
#' @return Cumulative gains plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{\link{input_modevalplots}} for details on the required input dataframe format.
cumgains <- function(plot_input=eval_t_type,customlinecolors=NA) {

  customlinecolors <- customlinecolors

  pp <- setplotparams(plot_input = plot_input,plottype = "Gains",customlinecolors=customlinecolors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::select(eval_type:decile,cumgain,legend)
  if (pp$seltype=="CompareModels") {
    optreflines <- plot_input %>%
      dplyr::mutate(legend=paste0('optimal gains (',dataset,')'),modelname='',cumgain=gain_opt) %>%
      dplyr::select(eval_type:decile,cumgain,legend) %>%
      dplyr::distinct()
  } else {
    optreflines <- plot_input%>%
      dplyr::mutate(legend=paste0('optimal gains (',legend,')'),cumgain=gain_opt) %>%
      dplyr::select(eval_type:decile,cumgain,legend)
  }
  minrefline <- plot_input %>%
    dplyr::mutate(legend=paste0('minimal gains'),modelname='',dataset='',category='',cumgain=gain_ref) %>%
    dplyr::select(eval_type:decile,cumgain,legend)%>%
    dplyr::distinct()
  plot_input <- rbind(minrefline,optreflines,vallines)
  plot_input$legend <- factor(plot_input$legend,levels=pp$gainslevels)

  #make plot
  plot_input %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=cumgain, colour=legend,linetype=legend,size=legend,alpha=legend)) +
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
      legend.position = c(1-0.15*pp$gainslegendcolumns, (pp$ngainslevels/pp$gainslegendcolumns)/10),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("lightgray",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())
  }




#' Lift plot
#'
#'
#' @param plot_input Dataframe.
#' @param targetcat String.
#' @return Lift plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#' @importFrom magrittr %>%
lift <- function(plot_input=eval_t_type,customlinecolors=NA) {

  customlinecolors <- customlinecolors

  pp <- setplotparams(plot_input = plot_input,plottype = "Lift",customlinecolors=customlinecolors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::filter(decile>0) %>% dplyr::select(eval_type:decile,cumlift,legend)
  minrefline <- plot_input %>% dplyr::filter(decile>0) %>%
    dplyr::mutate(legend=pp$liftreflabel,modelname='',dataset='',category='',cumlift=cumlift_ref) %>%
    dplyr::select(eval_type:decile,cumlift,legend)%>%
    dplyr::distinct()
  plot_input <- rbind(minrefline,vallines)
  plot_input$legend <- factor(plot_input$legend,levels=pp$liftlevels)


  #make plot
  plot_input %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=cumlift, colour=legend,linetype=legend,size=legend,alpha=legend)) +
    ggplot2::scale_linetype_manual(values=pp$liftlinetypes,guide=ggplot2::guide_legend(ncol=pp$liftlegendcolumns))+
    ggplot2::scale_color_manual(values=pp$liftlinecols)+
    ggplot2::scale_size_manual(values=pp$liftlinesizes)+
    ggplot2::scale_alpha_manual(values=pp$liftalphas)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="cumulative lift" ,expand = c(0, 0.02)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(title=pp$plottitle,subtitle=pp$plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10,hjust = 0.5,face="italic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position = c(1-0.15*pp$liftlegendcolumns, 1-(pp$nliftlevels/pp$liftlegendcolumns)/10),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("lightgray",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())
}


#lift()


#' Response plot
#'
#'
#' @param plot_input Dataframe.
#' @param targetcat String.
#' @return Lift plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#' @importFrom magrittr %>%
response <- function(plot_input=eval_t_type,customlinecolors=NA) {

  customlinecolors <- customlinecolors

  pp <- setplotparams(plot_input = plot_input,plottype = "Response",customlinecolors=customlinecolors)

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::filter(decile>0) %>% dplyr::select(eval_type:decile,response=pct,legend)
  minrefline <- plot_input %>% dplyr::filter(decile>0) %>%
    dplyr::mutate(legend=paste0('overall response (',legend,')'),response=pcttot) %>%
    dplyr::select(eval_type:decile,response,legend)%>%
    dplyr::distinct()
  plot_input <- rbind(minrefline,vallines)
  plot_input$legend <- factor(plot_input$legend,levels=pp$resplevels)

  #make plot
  plot_input %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=response, colour=legend,linetype=legend,size=legend,alpha=legend)) +
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
      legend.position = c(1-0.15*pp$resplegendcolumns, 1-(pp$nresplevels/pp$resplegendcolumns)/10),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("lightgray",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())
}


#response()

#' Cumulative response plot
#'
#'
#' @param plot_input Dataframe.
#' @param targetcat String.
#' @return Cumulative response plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#' @importFrom magrittr %>%
cumresponse <- function(plot_input=eval_t_type,customlinecolors=NA) {

  customlinecolors <- customlinecolors

  pp <- setplotparams(plot_input = plot_input,plottype = "Cumulative Response",customlinecolors=customlinecolors)
  #plot_input = eval_t_type
  # rearrange plot_input
  vallines <- plot_input %>% dplyr::filter(decile>0) %>% dplyr::select(eval_type:decile,cumresponse=cumpct,legend)
  minrefline <- plot_input %>% dplyr::filter(decile>0) %>%
    dplyr::mutate(legend=paste0('overall response (',legend,')'),cumresponse=pcttot) %>%
    dplyr::select(eval_type:decile,cumresponse,legend)%>%
    dplyr::distinct()
  plot_input <- rbind(minrefline,vallines)
  plot_input$legend <- factor(plot_input$legend,levels=pp$resplevels)

  #make plot
  plot_input %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=cumresponse, colour=legend,linetype=legend,size=legend,alpha=legend)) +
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
      legend.position = c(1-0.15*pp$resplegendcolumns, 1-(pp$nresplevels/pp$resplegendcolumns)/10),
      legend.background = ggplot2::element_rect(color = NA, fill = ggplot2::alpha("lightgray",0.2), size = 0),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"),
      axis.line.x=ggplot2::element_line(),
      axis.line.y=ggplot2::element_line())

}

#cumresponse()

#multiplot(cumgains,lift,response,cumresponse,cols=2)

#' Save Model Plots to file.
#'
#'
#' @param plots list of strings. List of plot names as strings.
#' @param dir String. Directory on file system where to save plots to.
#'   Default: working directory.
#' @return Save model plots to file
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
#'
savemodelplots <- function(plots=c("cumgains","lift","response","cumresponse"), dir = getwd()) {
  for (plot in plots) {
    png(paste0(dir, "/", plot, ".png"))
    print(get(plot))
    dev.off()
    print(paste0('saved ',plot,' to ',dir, "/", plot, ".png", sep = ''))
  }
}

