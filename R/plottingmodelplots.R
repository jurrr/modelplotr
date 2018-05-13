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
#' @param plot_input Dataframe.
#' @param targetcat String.
#' @return Cumulative gains plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
cumgains <- function(plot_input=eval_t_tot,targetcat='setosa') {
  plot_input %>%
  dplyr::filter(category==targetcat) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=cumgain, colour=dataset)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=gain_opt, colour=dataset),linetype=2) +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=gain_ref), colour="gray",linetype=2) +
  ggplot2::scale_x_discrete(name="decile", breaks=0:10, labels=0:10) +
  ggplot2::scale_y_continuous(name="cumulative gains",breaks=seq(0,1,0.2),labels = scales::percent ) +
  ggplot2::ggtitle(paste("Gains chart")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.position="top")
  }

#cumgains()


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
lift <- function(plot_input=eval_t_tot,targetcat='setosa') {
  plot_input %>%
  dplyr::filter(category==targetcat & decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=cumlift, colour=dataset)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 1,colour="gray",linetype=2) +
  ggplot2::scale_x_discrete(name="decile", breaks=0:10, labels=0:10) +
  ggplot2::scale_y_continuous(name="cumulative lift") +
  ggplot2::ggtitle(paste("Lift chart")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.position="top")
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
response <- function(plot_input=eval_t_tot,targetcat='setosa') {
  plot_input %>%
  dplyr::filter(category==targetcat & decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=pct, colour=dataset)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=pcttot,colour=dataset),linetype=2) +
  ggplot2::scale_x_discrete( name="decile", breaks=1:10, labels=1:10) +
  ggplot2::scale_y_continuous(name="response",labels = scales::percent) +
  ggplot2::ggtitle(paste("Response chart")) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,size = 20)) +
  ggplot2::theme(legend.position="top")
}

#response()

#' Cumulative responnse plot
#'
#'
#' @param plot_input Dataframe.
#' @param targetcat String.
#' @return Cumulative response plot.
#' @examples
#' add(1, 1)
#' add(10, 10)
#' @export
cumresponse <- function(plot_input=eval_t_tot,targetcat='setosa') {
  plot_input %>%
  dplyr::filter(category==targetcat & decile>0) %>%
  ggplot2::ggplot(ggplot2::aes(x=decile,y=cumpct, colour=dataset)) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(x=decile,y=pcttot,colour=dataset),linetype=2) +
  ggplot2::scale_x_discrete( name="decile", breaks=1:10, labels=1:10) +
  ggplot2::scale_y_continuous(name="cumulative response",labels = scales::percent) +
  ggplot2::ggtitle(paste("Cumulative response chart")) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,size = 20)) +
  ggplot2::theme(legend.position="top")
}

#cumresponse()

#multiplot(cumgains,lift,response,cumresponse,cols=2)
