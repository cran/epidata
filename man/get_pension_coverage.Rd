% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pension.r
\name{get_pension_coverage}
\alias{get_pension_coverage}
\title{Retreive Pension Coverage}
\usage{
get_pension_coverage(by = NULL)
}
\arguments{
\item{by}{\code{NULL} or character string with any combination of \code{g} (Gender),
\code{r} (Race), \code{e} (Education), \code{d} (Percentile), \code{l} (Entry-level)
i.e. if you want to retrieve pension data by gender and race, you would set this
parameter to "\code{gr}".}
}
\value{
\code{tbl_df} with data filtered by the selected criteria.

data frame
}
\description{
Employer-provided pension coverage shows the share of workers included in an
employer-provided plan for which the employer paid for at least some of their pension
coverage.
}
\details{
Population sample: Private-sector workers age 18–64 & at least 20 hours/week and 26 weeks/year
}
\note{
Data source: CPS ASEC
}
\examples{
if (not_dos()) get_health_insurance_coverage()

if (not_dos()) get_health_insurance_coverage("r")

if (not_dos()) get_health_insurance_coverage("gr")
}
\references{
\href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
}
