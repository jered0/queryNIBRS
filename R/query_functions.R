# These functions are the main abstractions for querying the Crime Data
#   Explorer. They call the connection and query-building functions with
#   specialized arguments.

#' Retrieve a count of cargo theft crimes from UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @param variable A variable used to group the crimes reported.
#' @return Data set returned from the API query.
#' @examples
#' cde_get_ct_count()
#' cde_get_ct_count(variable="victim_type_name")
#' cde_get_ct_count("FL", offense="rape", variable="victim_type_name")
cde_get_ct_count <- function(state='', offense='', variable='location_name', fields='') {
  variable_list <- c('location_name', 'victim_type_name', 'prop_desc_name', 'offense_name')
  cde_make_count_query('ct', variable, variable_list, offense_list, state, offense, fields)
}

#' Retrieve a count of hate crimes from UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @param variable A variable used to group the crimes reported.
#' @return Data set returned from the API query.
#' @examples
#' cde_get_hc_count()
#' cde_get_hc_count("FL", offense="arson")
cde_get_hc_count <- function(state='', offense='', variable='bias_name', fields='') {
  variable_list <- c('bias_name')
  cde_make_count_query('hc', variable, variable_list, offense_list, state, offense, fields)
}

#' Retrieve a count of offenders from UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @param variable A variable used to group the crimes reported.
#' @return Data set returned from the API query.
#' @examples
#' cde_get_offenders_count()
#' cde_get_offenders_count(variable="offense_name")
#' cde_get_offenders_count("FL", offense="homicide", variable="ethnicity")
cde_get_offenders_count <- function(state='', offense='', variable='ethnicity', fields='') {
  variable_list <- c('ethnicity', 'location_name', 'offense_name', 'prop_desc_name', 'race_code', 'age_num', 'sex_code')
  cde_make_count_query('offenders', variable, variable_list, offense_list, state, offense, fields)
}

#' Retrieve a count of offenses committed from UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @param variable A variable used to group the crimes reported.
#' @return Data set returned from the API query.
#' @examples
#' cde_get_offenses_count()
#' cde_get_offenses_count(variable="offense_name")
#' cde_get_offenses_count("FL", offense="larceny", variable="num_premises_entered")
cde_get_offenses_count <- function(state='', offense='', variable='weapon_name', fields='') {
  variable_list <- c('weapon_name', 'method_entry_code', 'num_premises_entered', 'location_name', 'offense_name')
  cde_make_count_query('offenses', variable, variable_list, offense_list, state, offense, fields)
}

#' Retrieve a count of victims from UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @param variable A variable used to group the crimes reported.
#' @return Data set returned from the API query.
#' @examples
#' cde_get_victims_count()
#' cde_get_victims_count(variable="ethnicity")
#' cde_get_victims_count("FL", offense="larceny", variable="offender_relationship")
cde_get_victims_count <- function(state='', offense='', variable='prop_desc_name', fields='') {
  variable_list <- c('prop_desc_name', 'location_name', 'offense_name', 'ethnicity', 'resident_status_code', 'offender_relationship', 'circumstance_name', 'race_code', 'age_num', 'sex_code')
  cde_make_count_query('victims', variable, variable_list, offense_list, state, offense, fields)
}

#' Retrieve data on agencies reporting data on human trafficking to UCR.
#'
#' Omitting the state will fetch national data, but I don't recommend it
#'   with this function - lots of data.
#'
#' @inheritParams cde_get_participation_agencies
#' @return Data set returned from the API query.
#' @examples
#' cde_get_ht_agencies("FL")
cde_get_ht_agencies <- function(state='', agency_ori='', agency_name='', year='') {
  cde_make_ht_query('agencies', state, agency_ori, agency_name, year)
}

#' Retrieve data on human trafficking from the UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @return Data set returned from the API query.
#' @examples
#' cde_get_ht_states()
#' cde_get_ht_states("FL")
cde_get_ht_states <- function(state='', year='') {
  cde_make_ht_query('states', state, year=year)
}

#' Retrieve NIBRS estimates of crime totals.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @return Data set returned from the API query.
#' @examples
#' cde_get_estimates()
#' cde_get_estimates("FL")
#'
#' x <- cde_get_estimates()
#' plot(x$year, x$homicide, type="b")
cde_get_estimates <- function(state='', fields='') {
  cde_make_scoped_query('estimates', state=state, fields=fields)
}

#' Retrieve data on states that participate in UCR.
#'
#' Omitting the state will fetch national data.
#'
#' @inheritParams cde_get_participation_agencies
#' @return Data set returned from the API query.
#' @examples
#' cde_get_participation()
#' cde_get_participation("FL")
cde_get_participation <- function(state='', fields='') {
  cde_make_scoped_query('participation', state=state, fields=fields)
}

#' Retrieve data on agencies that participate in UCR.
#'
#' Omitting the state will fetch national data.
#'
#' Note that I won't provide an example for this one because I don't
#'   know the agency names or ORIs required for a query.  Just included this
#'   as a function because it's one of the possible queries.
#'
#' @param state Two-letter abbreviation of the state.
#' @param state_name Name of the state.
#' @param fields Limit the response to these specific fields.
#' @param year Limit data to this year or year range.
#' @param agency_ori The ORI of the agency to be queried.
#' @param agency_name Name of the agency (exact match only).
#' @param reported Whether the agency reported data.
#' @param reported_nibrs Whether the agency reported data for NIBRS.
#' @param months_reported Months when agency reported data.
#' @param nibrs_months_reported Months when agency reported data for NIBRS.
#' @return Data set returned from the API query.
cde_get_participation_agencies <- function(state='', state_name='', fields='', year='', agency_ori='', agency_name='', reported='', reported_nibrs='', months_reported='', nibrs_months_reported='') {
  new_query <- '/participation/agencies'
  new_options <- NULL
  if (state != '')
    new_options <- append(new_options, paste('state=', state, sep=''))
  if (state_name != '')
    new_options <- append(new_options, paste('state_name=', state_name, sep=''))
  if (fields != '')
    new_options <- append(new_options, paste('fields=', fields, sep=''))
  if (year != '')
    new_options <- append(new_options, paste('year=', year, sep=''))
  if (agency_ori != '')
    new_options <- append(new_options, paste('agency_ori=', agency_ori, sep=''))
  if (agency_name != '')
    new_options <- append(new_options, paste('agency_name=', agency_name, sep=''))
  if (reported != '')
    new_options <- append(new_options, paste('reported=', reported, sep=''))
  if (reported_nibrs != '')
    new_options <- append(new_options, paste('reported_nibrs=', reported_nibrs, sep=''))
  if (months_reported != '')
    new_options <- append(new_options, paste('months_reported=', months_reported, sep=''))
  if (nibrs_months_reported != '')
    new_options <- append(new_options, paste('nibrs_months_reported=', nibrs_months_reported, sep=''))
  cde_get_data(new_query, new_options)
}
