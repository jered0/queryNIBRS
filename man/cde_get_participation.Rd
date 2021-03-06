% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_functions.R
\name{cde_get_participation}
\alias{cde_get_participation}
\title{Retrieve data on states that participate in UCR.}
\usage{
cde_get_participation(state = "", fields = "")
}
\arguments{
\item{state}{Two-letter abbreviation of the state.}

\item{fields}{Limit the response to these specific fields.}
}
\value{
Data set returned from the API query.
}
\description{
Omitting the state will fetch national data.
}
\examples{
cde_get_participation()
cde_get_participation("FL")
}
