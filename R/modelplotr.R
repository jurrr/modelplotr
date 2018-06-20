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
#'   \item{dataprep_modevalplots}{a function that builds a dataframe object
#'     'eval_tot' that contains actuals and predictions on
#'     dependent variable for each dataset in datasets.}
#'   \item{input_modevalplots}{a function that creates a dataframe  with aggregated
#'     actuals and predictions. A record in 'eval_t' is unique on the combination
#'     of datasets-decile.}
#'     }
#' @section Plotting functions:
#'   The plotting functions are:
#'   \describe{
#'   \item{cumgains}{cumulative gains plot}
#'   \item{lift}{lift plot}
#'   \item{response}{response plot}
#'   \item{cumresponse}{cumulative response plot}
#'   \item{multiplot}{multiplot}
#'   }
#'
#' @seealso \url{https://github.com/jurrr/modelplotr} for details on the package
#' @seealso \url{https://cmotions.nl/publicaties/} for our blog post on using modelplotr
#'
#' @docType package
#' @name modelplotr
NULL
