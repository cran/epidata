% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.r
\name{not_dos}
\alias{not_dos}
\title{Not DoS'ing EPI/Cloudflare}
\usage{
not_dos()
}
\value{
logical
}
\description{
The EPI site/API is protected by Cloudflare. CF thinks CRAN is DoS'ing
it when it runs examples and tests. This function is being used to
prevent tests and examples from failing on CRAN.
}
