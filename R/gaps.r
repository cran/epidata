#' Retreive the percent by which hourly wages of female workers are less than hourly wages of male workers
#'
#' The gender wage gap is the percent by which hourly wages of female workers are less than
#' hourly wages of male workers. It is also often expressed as a wage ratio (women's
#' share of men's wages) by subtracting the gap from 100 percent.
#'
#' \itemize{
#' \item{A median gender wage gap of 17.3 percent means that a typical woman is paid 17.3
#'       percent less per hour than a typical man.}
#' \item{An average gender wage gap of 19.7 percent means that on average women are paid
#'       19.7 percent less per hour than men.}
#' \item{A regression-based gender wage gap of 21.7 percent means that on average women
#'       are paid 21.7 percent less per hour than men, all else held equal (controlling for
#'       gender, race and ethnicity, education, experience, and geographic location).}
#' }
#'
#' @param by \code{NULL} or \code{r} for a parition by race
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @export
#' @examples
#' if (not_dos()) get_gender_wage_gap()
#'
#' if (not_dos())
#' get_gender_wage_gap("r")
get_gender_wage_gap <- function(by=NULL) {

  params <- list(subject="wagegap-mf")

  if (!is.null(by)) params <- make_params(params, by, c("r"))

  res <- epi_query(params)
  if (is.null(res)) return(data.frame())

  cols <- stringi::stri_trans_tolower(res$columns$name)
  cols <- stringi::stri_replace_all_regex(cols, "[\\('\\)]", "")
  cols <- stringi::stri_replace_all_regex(cols, "[[:space:]" %s+%
                                            rawToChar(as.raw(c(0xe2, 0x80, 0x93))) %s+% "-]+",
                                          "_")
  out <- setNames(as_data_frame(res$data), cols)
  out <- dplyr::mutate_all(out, "clean_cols")
  out <- suppressMessages(readr::type_convert(out))

  show_citation(res)

  out

}

#' Retreive the percent by which hourly wages of black workers are less than hourly wages of white workers
#'
#' The black-white wage gap is the percent by which hourly wages of black workers are less
#' than hourly wages of white workers. It is also often expressed as a wage ratio (black
#' workers' share of white workers' wages) by subtracting the gap from 100 percent.
#'
#' \itemize{
#' \item{A median black-white wage gap of 26.2 percent means that a typical black worker
#'       is paid 26.2 percent less per hour than a typical white worker.}
#' \item{An average black-white wage gap of 26.6 percent means that on average black
#'       workers are paid 26.6 percent less per hour than white workers.}
#' \item{A regression-based black-white wage gap of 15.2 percent means that on average
#'       black workers are paid 15.2 percent less per hour than white workers, all else
#'       held equal (controlling for gender, race and ethnicity, education, experience,
#'       and geographic location).}
#' }
#'
#' @param by \code{NULL} or \code{g} for a parition by gender
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @export
#' @examples
#' if (not_dos()) get_black_white_wage_gap()
#'
#' if (not_dos()) get_black_white_wage_gap("g")
get_black_white_wage_gap <- function(by=NULL) {

  params <- list(subject="wagegap-bw")

  if (!is.null(by)) params <- make_params(params, by, c("g"))

  res <- epi_query(params)
  if (is.null(res)) return(data.frame())

  cols <- stringi::stri_trans_tolower(res$columns$name)
  cols <- stringi::stri_replace_all_regex(cols, "[\\('\\)]", "")
  cols <- stringi::stri_replace_all_regex(cols, "[[:space:]" %s+%
                                            rawToChar(as.raw(c(0xe2, 0x80, 0x93))) %s+% "-]+",
                                          "_")
  out <- setNames(as_data_frame(res$data), cols)
  out <- dplyr::mutate_all(out, "clean_cols")
  out <- suppressMessages(readr::type_convert(out))

  show_citation(res)

  out

}

#' Retreive the percent by which hourly wages of Hispanic workers are less than hourly wages of white workers
#'
#' The Hispanic-white wage gap is the percent by which hourly wages of Hispanic workers
#' are less than hourly wages of white workers. It is also often expressed as a wage ratio
#' (Hispanic workers' share of white workers' wages) by subtracting the gap from 100 percent.
#'
#' \itemize{
#' \item{A median Hispanic-white wage gap of 29.6 percent means that a typical Hispanic
#'       worker is paid 29.6 percent less per hour than a typical white worker.}
#' \item{An average Hispanic-white wage gap of 30.1 percent means that on average Hispanic
#'       workers are paid 30.1 percent less per hour than white workers.}
#' \item{A regression-based Hispanic-white wage gap of 11.1 percent means that on average
#'       Hispanic workers are paid 11.1 percent less per hour than white workers, all
#'       else held equal (controlling for gender, race and ethnicity, education,
#'       experience, and geographic location).}
#' }
#'
#' @param by \code{NULL} or \code{g} for a parition by gender
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @export
#' @examples
#' if (not_dos()) get_hispanic_white_wage_gap()
#'
#' if (not_dos()) get_hispanic_white_wage_gap("g")
get_hispanic_white_wage_gap <- function(by=NULL) {

  params <- list(subject="wagegap-hw")

  if (!is.null(by)) params <- make_params(params, by, c("g"))

  res <- epi_query(params)
  if (is.null(res)) return(data.frame())

  cols <- stringi::stri_trans_tolower(res$columns$name)
  cols <- stringi::stri_replace_all_regex(cols, "[\\('\\)]", "")
  cols <- stringi::stri_replace_all_regex(cols, "[[:space:]" %s+%
                                            rawToChar(as.raw(c(0xe2, 0x80, 0x93))) %s+% "-]+",
                                          "_")
  out <- setNames(as_data_frame(res$data), cols)
  out <- dplyr::mutate_all(out, "clean_cols")
  out <- suppressMessages(readr::type_convert(out))

  show_citation(res)

  out

}


#' Retreive the percent by which hourly wages of college graduates exceed those of otherwise
#' equivalent high school graduates
#'
#' A regression-based college wage premium of 56.1 percent means that on average workers
#' with a college degree are paid 56.1 percent more per hour than workers whose highest
#' education credential is a high school diploma, all else held equal (controlling for
#' gender, race and ethnicity, education, experience, and geographic location).
#'
#' @param by \code{NULL} or \code{g} for a parition by gender
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @export
#' @examples
#' if (not_dos()) get_college_wage_premium()
#'
#' if (not_dos()) get_college_wage_premium("g")
get_college_wage_premium <- function(by=NULL) {

  params <- list(subject="wagegap-coll")

  if (!is.null(by)) params <- make_params(params, by, c("g"))

  res <- epi_query(params)
  if (is.null(res)) return(data.frame())

  cols <- stringi::stri_trans_tolower(res$columns$name)
  cols <- stringi::stri_replace_all_regex(cols, "[\\('\\)]", "")
  cols <- stringi::stri_replace_all_regex(cols, "[[:space:]" %s+%
                                            rawToChar(as.raw(c(0xe2, 0x80, 0x93))) %s+% "-]+",
                                          "_")
  out <- setNames(as_data_frame(res$data), cols)
  out <- dplyr::mutate_all(out, "clean_cols")
  out <- suppressMessages(readr::type_convert(out))

  show_citation(res)

  out

}

#' Retreive the percent by which hourly wages of workers without a high school diploma
#' (or equivalent) are less than wages of otherwise equivalent workers who have graduated
#' from high school
#'
#' A regression-based non-high school wage penalty of 21.8 percent means that on average
#' workers without a high school diploma are paid 21.8 percent less per hour than workers
#' with a high school diploma, all else held equal (controlling for gender, race and
#' ethnicity, education, experience, and geographic location).
#'
#' @param by \code{NULL} or \code{g} for a parition by gender
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @export
#' @examples \dontrun{
#' if (not_dos()) get_non_high_school_wage_penalty()
#'
#' if (not_dos()) get_non_high_school_wage_penalty("g")
#' }
get_non_high_school_wage_penalty <- function(by=NULL) {

  params <- list(subject="wagegap-hs")

  if (!is.null(by)) params <- make_params(params, by, c("g"))

  res <- epi_query(params)
  if (is.null(res)) return(data.frame())

  cols <- stringi::stri_trans_tolower(res$columns$name)
  cols <- stringi::stri_replace_all_regex(cols, "[\\('\\)]", "")
  cols <- stringi::stri_replace_all_regex(cols, "[[:space:]" %s+%
                                            rawToChar(as.raw(c(0xe2, 0x80, 0x93))) %s+% "-]+",
                                          "_")
  out <- setNames(as_data_frame(res$data), cols)
  out <- dplyr::mutate_all(out, "clean_cols")
  out <- suppressMessages(readr::type_convert(out))

  show_citation(res)

  out

}

