globalVariables(c("date", "region", "value"))

as_data_frame <- function(x) {
  dplyr::as_tibble(as.data.frame(x))
}

httr::user_agent(
  sprintf(
    "epidata package v%s: (<%s>)",
    utils::packageVersion("epidata"),
    utils::packageDescription("epidata")$URL
  )
) -> .EPIDATA_UA

show_citation <- function(res) {

  if (!getOption("epidata.show.citation", TRUE)) return()

  cite <- html_text(read_html(res$meta$source %||% "<p>Economic Policy Institute</p>"))
  notes <- "None"
  if (length(res$meta$notes) > 0) notes <- html_text(read_html(res$meta$notes))
  message(sprintf('Note: %s\nCitation: "%s"', notes, cite))

}

#' Not DoS'ing EPI/Cloudflare
#'
#' The EPI site/API is protected by Cloudflare. CF thinks CRAN is DoS'ing
#' it when it runs examples and tests. This function is being used to
#' prevent tests and examples from failing on CRAN.
#'
#' @return logical
#' @export
not_dos <- function() {
  utils::getFromNamespace("interactive", "base")()
}