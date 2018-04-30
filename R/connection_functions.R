.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Remember to use set_ucr_api_key() to set your UCR API key.")
}

# These functions are largely metafunctions used by the query functions.  Users
# shouldn't call these, apart from the set_ucr_api_key function - the rest are
# utility functions.  Use the query functions instead.

# TODO: Account for year queries being able to use formats like
#   "year>1999", which I wasn't able to successfully test anyway.
# TODO: Support queries for multiple states at once
# TODO: Support ORIs in part. agencies
# TODO: Currently "offense" is actually using explorer_offense, try
#   to support offense listings better

# Set API key value (should be blank for general release)
ucr_api_key <- "wD7QRfAGjIdojtrdNgBk51BIYVMmbZaqsb3qGQKC"

# Set the endpoint the queries will be sent to
endpoint <- "https://api.usa.gov/crime/fbi/ucr"

# A list of offenses available for queries
offense_list <- c('aggravated-assault', 'arson', 'burglary', 'larceny', 'motor-vehicle-theft', 'homicide', 'rape', 'robbery')

#' Set the Crime Data API key.
#'
#' You can get an API key from \url{https://api.data.gov/signup/}.
#'
#' @param new_api_key The new API key value.
#' @seealso \url{https://api.data.gov/signup/}
set_ucr_api_key <- function(new_api_key) {
  assign("ucr_api_key", new_api_key, envir = .GlobalEnv)
}

#' Determine whether the argument represents a state.
#'
#' The function returns either /national if it's not a state, or
#'   /states/<abbr> if it is a state.
#' @param state A two-letter state abbreviation or an empty string.
#' @return String that represents either national or state-level scope
#'   for a query URL.
cde_get_scope <- function(state) {
  state <- toupper(state)
  if (is.element(state, state.abb))
    scope <- paste('/states/', state, sep='')
  else
    scope <- '/national'
  return(scope)
}

#' Construct the URL that will be used for an API call.
#'
#' This function stitches together the endpoint, API key, and various options
#'   to create the URL that will be accessed to make the query.
#' @param query The path part of the URL that will be appended to the endpoint URL.
#' @param options A list of options to be included, like "year=2008".
#' @param page Which page of the query results will be retrieved.
#' @param per_page The maximum number of results that will be retrieved
#'   with this query.
#' @return A URL that will query the Crime Database when accessed.
cde_build_url <- function(query, options=NULL, page="1", per_page="100") {
  if (ucr_api_key == '')
    stop("API key is not set.")
  if (is.character(options))
    options <- paste('&', paste(options, collapse='&'), sep='')
  else
    options <- ''
  paste(endpoint, query, '?page=', page, '&per_page=', per_page, '&output=json&api_key=', ucr_api_key, options, sep='')
}

#' Retrieve data via the Crime Data API.
#'
#' Uses \code{cde_build_url} to build a query URL, then runs the query until
#'   all results have been fetched.
#' @inheritParams cde_build_url
#' @return A data set containing the results of the query.
cde_get_data <- function(query, options=NULL) {
  # Make initial query
  raw_data <- jsonlite::fromJSON(url(cde_build_url(query, options)))
  crime_data <- raw_data$results
  current_page <- raw_data$pagination$page
  num_pages <- raw_data$pagination$pages

  # If there's more than one page, retrieve the others and append
  while (current_page < num_pages) {
    raw_data <- jsonlite::fromJSON(url(cde_build_url(query, options, page=current_page+1)))
    crime_data <- Map(c, crime_data, raw_data$results)
    current_page <- raw_data$pagination$page
  }
  return(crime_data)
}

#' Fetch either national or state-level data.
#'
#' Uses \code{cde_get_scope} to determine the scoped portion of the URL, then uses
#' \code{cde_get_data} to retrieve the data via the API.
#' @param category The category for the query, used in the base URL.
#' @param variable A variable to group results.
#' @param variable_list A list of allowed variable settings for the
#'   function that called this one.
#' @param offense_list A list of allowed offense values for the function
#'   that called this one.
#' @param state A two-letter state abbreviation or an empty string for a national
#'   query.
#' @param offense Limits the results to matches for this offense.
#' @param fields Limits results to specific fields.
#' @return A data set containing the results of the query.
cde_make_scoped_query <- function(category, variable='', variable_list=NULL, offense_list=NULL, state='', offense='', fields='') {
  if (! is.null(variable_list) && ! is.element(variable, variable_list))
    return(paste("Error. For", category, "variable must be one of:", paste(variable_list, collapse=' ')))
  # Stitch arguments together to construct a query
  new_query <- paste('/', category, cde_get_scope(state), ifelse(variable != '', paste('/', variable, sep=''), ''), sep='')
  new_options <- NULL
  if (is.element(offense, offense_list)) {
    new_query <- paste(new_query, "/offenses", sep='')
    new_options <- append(new_options, paste("explorer_offense=", offense, sep=''))
  }
  if (fields != '')
    new_options <- append(new_options, paste('fields=', fields, sep=''))
  cde_get_data(new_query, new_options)
}

#' Fetch data for a query that asks for a count.
#'
#' Queries for various crimes against property use "/count" in the URL.
#' @inheritParams cde_make_scoped_query
#' @return A data set containing the results of the query.
cde_make_count_query <- function(category, variable, variable_list=NULL, offense_list=NULL, state='', offense='', fields='') {
  cde_make_scoped_query(paste(category, '/count', sep=''), variable, variable_list, offense_list, state, offense, fields)
}

#' Construct a query for human trafficking data from the UCR.
#'
#' Omitting the state will fetch national data.
#'
#' Note that you should use \code{cde_get_ht_states} or \code{cde_get_ht_agencies}
#'   instead of calling this function directly.
#'
#' @inheritParams cde_get_participation_agencies
#' @return Data set returned from the API query.
cde_make_ht_query <- function(category, state='', ori='', agency_name='', year='') {
  new_query <- paste('/ht/', category, sep='')
  new_options <- NULL
  if (state != '')
    new_options <- append(new_options, paste('state_abbr=', state, sep=''))
  if (ori != '')
    new_options <- append(new_options, paste('ori=', ori, sep=''))
  if (agency_name != '')
    new_options <- append(new_options, paste('agency_name=', agency_name, sep=''))
  if (year != '')
    new_options <- append(new_options, paste('year=', year, sep=''))
  cde_get_data(new_query, new_options)
}
