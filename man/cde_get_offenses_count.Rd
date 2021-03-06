% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_functions.R
\name{cde_get_offenses_count}
\alias{cde_get_offenses_count}
\title{Retrieve a count of offenses committed from UCR.}
\usage{
cde_get_offenses_count(state = "", offense = "", variable = "weapon_name",
  fields = "")
}
\arguments{
\item{state}{Two-letter abbreviation of the state.}

\item{variable}{A variable used to group the crimes reported.}

\item{fields}{Limit the response to these specific fields.}
}
\value{
Data set returned from the API query.
}
\description{
Omitting the state will fetch national data.
}
\examples{
cde_get_offenses_count()
cde_get_offenses_count(variable="offense_name")
cde_get_offenses_count("FL", offense="larceny", variable="num_premises_entered")
}
