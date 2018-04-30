# queryNIBRS

## A package for querying NIBRS crime data summaries.

This package retrieves data from the FBI's Uniform Crime Reporting program (UCR) using their [Crime Data Explorer (CDE) API](https://crime-data-explorer.fr.cloud.gov/api).  Both instance data and summary data from the National Incident Based Reporting System (NIBRS) can be retrieved.

Accessing data will require the use of an API key that can be obtained from [https://api.data.gov/signup/](https://api.data.gov/signup/).

## Warning:
The API is considered to be in development, so some functions may not retrieve data or work as advertised.  Some legitimate queries can return errors.

Note that the summary data can be explored visually on the FBI's [Crime Data Explorer website](https://crime-data-explorer.fr.cloud.gov/).

The functions in the query_functions.R file abstract queries to a convenient level, while the functions in connection_functions.R are the metafunctions that create and execute queries.  The functions in query_functions.R are the ones you'll want to call to retrieve data.

Calling a function with an invalid variable or offense name set will return a list of valid values for that function.  Some are self-explanatory (like the offense "homicide") while others are more arcane and will require digging around in the FBI's documentation to interpret (like "age_num").
