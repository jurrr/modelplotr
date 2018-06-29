.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package modelplotr loaded! Happy model plotting!")
}


#' modelplotr: A package for creating evaluation plots to WOW your business.
#'
#' The modelplotr package provides two categories of important functions:
#' datapreparation and plotting.
#'
#' @author Jurriaan Nagelkerke <jurriaan.nagelkerke@@gmail.com> [aut, cre]
#' @author Pieter Marcus <pieter.marcus@@persgroep.net> [aut]
#'
#' @section Datapreparation functions:
#'  The datapreparation functions are:
#' \describe{
#'   \item{dataprep_modevalplots()}{a
#'   function that builds a dataframe object \code{'eval_tot'} that contains
#'   actuals and predictions on dependent variable for each dataset in datasets.}
#'   \item{input_modevalplots()}{a function that creates a dataframe \code{'eval_t_tot'} with aggregated
#'     actuals and predictions. A record in 'eval_t_tot' is unique on the combination
#'     of datasets-decile.}
#'   \item{scope_modevalplots()}{a function that creates a dataframe \code{'eval_t_type'}  with a subset
#'     of 'eval_t_tot', relevant to the selected scope of evaluation. }}
#' @section Plotting functions:
#'   The plotting functions are:
#' \describe{
#'   \item{cumgains()}{Generates the cumulative gains chart. This plot, often named gains chart,
#'     helps answering the question: When we apply the model and select the best X deciles,
#'     what percentage of the actual target class observations can we expect to target? }
#'     \item{lift()}{Generates the cumulative lift chart, often referred to as lift chart or index chart,
#'     helps you answer the question: When we apply the model and select the best X deciles,
#'     how many times better is that than using no model at all?}
#'     \item{response()}{Generates the response plot. It plots the percentage of target class observations
#'     per decile. It can be used to answer the following business question: When we apply
#'     the model and select decile X, what is the expected percentage of target class observations
#'     in that decile?}
#'     \item{cumresponse()}{Generates the cumulative response plot. It plots the cumulative percentage of
#'      target class observations up until that decile. It helps answering the question:
#'      When we apply the model and select up until decile X, what is the expected percentage of
#'      target class observations in the selection? }
#'     \item{multiplot()}{Generates a canvas with all four evaluation plots combined}}
#'
#' @seealso \url{https://github.com/jurrr/modelplotr} for details on the package
#' @seealso \url{https://cmotions.nl/publicaties/} for our blog post on using modelplotr
#'
#' @docType package
#' @name modelplotr
NULL
