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
  # needed for optimizing plot layout
  nlevels <- length(levels(plot_input$legend))
  randcols <- RColorBrewer::brewer.pal(n = 8, name = "Accent")
  levelcols <- randcols[1:nlevels]
  levels <- c(levels(plot_input$legend),'minimal gains',paste0('optimal gains (',levels(plot_input$legend),')'))
  linetypes <- c(rep('solid',nlevels),'dashed',rep('dotted',nlevels))
  if(length(customlinecolors)==nlevels) levelcols <- customlinecolors
  else if (!is.na(customlinecolors)) {
    print('specified customlinecolors vector not of required length! \
      It is cropped or extended with extra colors to match required length')
    linecols <- c(customlinecolors[1:nlevels],randcols[which(!randcols %in% customlinecolors)])
  }
  else linecols <- c(levelcols,'gray',levelcols)
  linesizes <- c(rep(1,nlevels),1,rep(1,nlevels))
  seltype <- max(as.character(plot_input$eval_type))
  selmod <- max(as.character(plot_input$modelname))
  seldata <- max(as.character(plot_input$dataset))
  selval <- max(as.character(plot_input$category))
  plottitle <- paste0('Gains chart - comparing ',ifelse(seltype=="CompareDatasets",'datasets',
    ifelse(seltype=="CompareModels",'models','target values')))
  plotsubtitle <- ifelse(plot_input$eval_type=="CompareDatasets",paste0('model: ',selmod,' & target value: ',selval),
    ifelse(plot_input$eval_type=="CompareModels",paste0('dataset: ',seldata,' & target value: ',selval),
      paste0('dataset: ',seldata,' & model: ',selmod)))

  # rearrange plot_input
  vallines <- plot_input %>% dplyr::select(eval_type:decile,cumgain,legend)
  optreflines <- plot_input%>% dplyr::mutate(legend=paste0('optimal gains (',legend, ')'),cumgain=gain_opt) %>% dplyr::select(eval_type:decile,cumgain,legend)
  minrefline <- plot_input %>% dplyr::mutate(legend=paste0('minimal gains'),cumgain=gain_ref) %>% dplyr::select(eval_type:decile,cumgain,legend)
  plot_input <- rbind(minrefline,optreflines,vallines)
  plot_input$legend <- factor(plot_input$legend,levels=levels)

  #make plot
  plot_input %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=decile,y=cumgain, colour=legend,linetype=legend,size=legend)) +
    ggplot2::scale_linetype_manual(values=linetypes)+
    ggplot2::scale_color_manual(values=linecols)+
    ggplot2::scale_size_manual(values=linesizes)+
    ggplot2::scale_x_continuous(name="decile", breaks=0:10, labels=0:10,expand = c(0, 0.02)) +
    ggplot2::scale_y_continuous(name="cumulative gains",breaks=seq(0,1,0.2),labels = scales::percent ,expand = c(0, 0.02)) +
    ggplot2::labs(title=plottitle,subtitle=plotsubtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20,hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(size = 14,hjust = 0.5)) +
    ggplot2::theme(legend.title = ggplot2::element_blank() ,
      legend.position = c(0.85, 0.2),
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
lift <- function(plot_input=eval_t_type) {
  plot_input %>%
  dplyr::filter(decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=cumlift, colour=legend)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 1,colour="gray",linetype=2) +
  ggplot2::scale_x_continuous(name="decile", breaks=1:10, labels=1:10) +
  ggplot2::scale_y_continuous(name="cumulative lift") +
  ggplot2::expand_limits(y=0) +
  ggplot2::ggtitle(paste("Lift chart")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20,hjust = 0.5)) +
  ggplot2::theme(legend.position="top",
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"))
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
response <- function(plot_input=eval_t_type) {
  plot_input %>%
  dplyr::filter(decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=pct, colour=legend)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=pcttot,colour=dataset),linetype=2) +
  ggplot2::scale_x_continuous( name="decile", breaks=1:10, labels=1:10) +
  ggplot2::scale_y_continuous(name="response",labels = scales::percent) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ggtitle(paste("Response chart")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20,hjust = 0.5)) +
  ggplot2::theme(legend.position="top",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"))
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
cumresponse <- function(plot_input=eval_t_type) {
  plot_input %>%
  dplyr::filter(decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=cumpct, colour=legend)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=pcttot,colour=dataset),linetype=2) +
  ggplot2::scale_x_continuous( name="decile", breaks=1:10, labels=1:10) +
  ggplot2::scale_y_continuous(name="cumulative response",labels = scales::percent) +
  ggplot2::expand_limits(y=0) +
  ggplot2::ggtitle(paste("Cumulative response chart")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20,hjust = 0.5)) +
  ggplot2::theme(legend.position="top",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line( linetype=3,size=.1, color="lightgray"))
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

