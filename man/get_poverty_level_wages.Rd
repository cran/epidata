% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poverty.R
\name{get_poverty_level_wages}
\alias{get_poverty_level_wages}
\title{Poverty-level wages}
\usage{
get_poverty_level_wages(by = NULL)
}
\arguments{
\item{by}{\code{NULL} or character string with any combination of \code{g} (Gender) or
\code{r} (Race), i.e. if you want to retrieve
unemployment data by gender and race, you would set this parameter to "\code{gr}".}
}
\value{
\code{tbl_df} with data filtered by the selected criteria.

data frame
}
\description{
Return the share of workers earning equal to or less than the poverty-level wage, or
the hourly wage that a full-time, year-round worker must earn to sustain a family of
four with two children at the official poverty threshold.
}
\details{
Population sample: Wage and salary workers age 18–64. Data source: CPS ORG | Census
Bureau (poverty threshold)
}
\examples{
if (not_dos()) get_poverty_level_wages()

if (not_dos()) get_poverty_level_wages("r")

if (not_dos()) get_poverty_level_wages("gr")
}
\references{
\href{https://www.epi.org/data/}{Economic Policy Institute Data Library}
}
