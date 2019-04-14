#' Bank clients that have/have not subscribed a term deposit.
#'
#' A dataset containing some customer characteristics for clients of a bank that have/have not subscribed a term deposit.
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{has_td}{has the client subscribed a term deposit? Values: "term deposit", "no".
#'   This variable is used as the binary target variable in examples for the modelplotr package.}
#'   \item{td_type}{what type of term deposit did the client subscribe? Values: "no.td", "td.type.A", "td.type.B", "td.type.C".
#'   This variable is used as the multinomial target variable in examples for the modelplotr package.}
#'   \item{duration}{last contact duration, in seconds (numeric)}
#'   \item{campaign}{number of contacts performed during this campaign and for this client}
#'   \item{pdays}{number of days that passed by after the client was last contacted from a previous campaign}
#'   \item{previous}{number of contacts performed before this campaign and for this client (numeric)}
#'   \item{euribor3m}{euribor 3 month rate}
#' }
#' @source This dataset is a subset of the dataset made available by the University of California, Irvine.
#' The complete dataset is available here: \url{https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip}
"bank_td"

# # SCRIPT TO CREATE bank_td
# library(dplyr)
# #zipname = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip'
# # we encountered that the source at uci.edu is not always available, therefore we made a copy to our repos.
# zipname = 'https://modelplot.github.io/img/bank-additional.zip'
# csvname = 'bank-additional/bank-additional-full.csv'
# temp <- tempfile()
# download.file(zipname,temp, mode="wb")
# bank <- read.table(unzip(temp,csvname),sep=";", stringsAsFactors=FALSE,header = TRUE)
# unlink(temp)
#
# summary(bank)
# bank <- bank %>% select('y','duration','campaign','pdays','previous','euribor3m') %>% sample_n(2000)
#
# # definition of y_cat, created to show modelplotr for multinomial targets
# bank <- bank %>%
#   mutate(has_td = case_when(y =='yes' ~ 'term.deposit',
#                             y =='no' ~ 'no.term.deposit'),
#          has_td = factor(has_td,levels=c('term.deposit','no.term.deposit')),
#          td_value = rexp(nrow(bank),1/5000)+ bank$duration * 20 +
#            500*(bank$campaign*rnorm(nrow(bank),1,1))+bank$euribor3m*(100+rnorm(nrow(bank),200,100)),
#          td_type = case_when(y == 'no' ~ 'no.td',
#                            td_value < 10000 ~ 'td.type.A',
#                            td_value <= 25000 ~ 'td.type.B',
#                            TRUE ~ 'td.type.C'),
#          td_type = factor(td_type,levels = c('no.td','td.type.A','td.type.B','td.type.C'))) %>%
#   select(has_td,td_type,duration,campaign,pdays,previous,euribor3m)
#
# bank_td <- bank
# usethis::use_data(bank_td,overwrite = TRUE)

