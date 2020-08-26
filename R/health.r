#' Retreive Health Insurance Coverage
#'
#' Employer-sponsored health insurance (ESI) coverage shows the share of workers who
#' received health insurance from their own job for which their employer paid for at
#' least some of their health insurance coverage.
#'
#' Population sample: Private-sector workers age 18–64 & at least 20 hours/week and 26 weeks/year
#'
#' @param by \code{NULL} or character string with any combination of \code{g} (Gender),
#'   \code{r} (Race), \code{e} (Education), \code{d} (Percentile), \code{l} (Entry-level)
#'   i.e. if you want to retrieve unemployment data by gender and race, you would set this
#'   parameter to "\code{gr}".
#' @return \code{tbl_df} with data filtered by the selected criteria.
#' @references \href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
#' @note Data source: CPS ASEC
#' @return data frame
#' @export
#' @examples
#' if (not_dos()) get_health_insurance_coverage()
#'
#' if (not_dos()) get_health_insurance_coverage("r")
#'
#' if (not_dos()) get_health_insurance_coverage("gr")
get_health_insurance_coverage <- function(by=NULL) {

  params <- list(subject="healthcov")

  if (!is.null(by)) {
    params <- make_params(params, by, c("g", "r", "e", "d", "l"))
  }
  names(params) <- gsub("^l$", "el", names(params))

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