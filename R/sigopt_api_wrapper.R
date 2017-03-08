#' Create an experiment
#'
#' @param body POST body of create request
#' @return experiment created by SigOpt
#' @export
#' @examples
#' env <- Sys.getenv("NOT_CRAN")
#' if (!identical(env, "true")) {
#' 0
#' } else {
#' create_experiment(list(
#'   name="R test experiment",
#'   parameters=list(
#'     list(name="x1", type="double", bounds=list(min=0, max=100)),
#'     list(name="x2", type="double", bounds=list(min=0, max=100))
#'   )
#' ))}
create_experiment <- function(body) {
  req <- sigopt_POST("v1/experiments", body)
  sigopt_parse(req)
}

#' Fetch an experiment
#'
#' @param experiment_id the id of an experiment to fetch
#' @param body Url params of GET request
#' @return SigOpt experiment with id experiment_id
#' @export
#' @examples
#' env <- Sys.getenv("NOT_CRAN")
#' if (!identical(env, "true")) {
#' 0
#' } else {
#' experiment <- create_experiment(list(
#'   name="R test experiment",
#'   parameters=list(
#'     list(name="x1", type="double", bounds=list(min=0, max=100)),
#'     list(name="x2", type="double", bounds=list(min=0, max=100))
#'   )
#' ))
#' fetch_experiment(experiment$id)}
fetch_experiment <- function(experiment_id, body=NULL) {
  req <- sigopt_GET(paste("v1/experiments", experiment_id, sep="/"), body)
  sigopt_parse(req)
}

#' Create a suggestion for an experiment
#'
#' @param experiment_id the id of an experiment to create an suggestion for
#' @param body POST body of create request
#' @return suggestion created by SigOpt
#' @export
#' @examples
#' env <- Sys.getenv("NOT_CRAN")
#' if (!identical(env, "true")) {
#' 0
#' } else {
#' experiment <- create_experiment(list(
#'   name="R test experiment",
#'   parameters=list(
#'     list(name="x1", type="double", bounds=list(min=0, max=100)),
#'     list(name="x2", type="double", bounds=list(min=0, max=100))
#'   )
#' ))
#' create_suggestion(experiment$id)}
create_suggestion <- function(experiment_id, body=NULL) {
  req <- sigopt_POST(paste("v1/experiments", experiment_id, "suggestions", sep="/"), body)
  sigopt_parse(req)
}

#' Create an observation for an experiment
#'
#' @param experiment_id the id of an experiment to create an observation for
#' @param body POST body of create request
#' @return observation created by SigOpt
#' @export
#' @examples
#' env <- Sys.getenv("NOT_CRAN")
#' if (!identical(env, "true")) {
#' 0
#' } else {
#' experiment <- create_experiment(list(
#'   name="R test experiment",
#'   parameters=list(
#'     list(name="x1", type="double", bounds=list(min=0, max=100)),
#'     list(name="x2", type="double", bounds=list(min=0, max=100))
#'   )
#' ))
#' suggestion <- create_suggestion(experiment$id)
#' create_observation(experiment$id, list(suggestion=suggestion$id, value=99.08))
#' create_observation(experiment$id, list(suggestion=suggestion$id, value=99.58, value_stddev=0.1))}
create_observation <- function(experiment_id, body) {
  req <- sigopt_POST(paste("v1/experiments", experiment_id, "observations", sep="/"), body)
  sigopt_parse(req)
}

#' GET request to SigOpt API path, with json encoded body
#'
#' @param path path of SigOpt API url to POST to
#' @param query list of query parameters to be url-encoded
#' @param api_token SigOpt api token
#' @return result of request to SigOpt API, needs to be JSON decoded
#' @seealso \code{\link{sigopt_parse}}, which parses the result of this function
#' @export
sigopt_GET <- function(path, query = NULL, api_token = sigopt_api_token()) {
  auth <- sigopt_auth(api_token)
  req <- httr::GET(sigopt_api_url(), path = path, query = query, auth, sigopt_api_user_agent())
  sigopt_check(req)

  req
}

#' POST request to SigOpt API path, with json encoded body
#'
#' @param path path of SigOpt API url to POST to
#' @param body POST body, will be json-encoded
#' @param api_token SigOpt api token
#' @return result of request to SigOpt API, needs to be JSON decoded
#' @seealso \code{\link{sigopt_parse}}, which parses the result of this function
#' @export
sigopt_POST <- function(path, body, api_token = sigopt_api_token()) {
  auth <- sigopt_auth(api_token)

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  req <- httr::POST(sigopt_api_url(), path = path, body = body_json, encode = "json", auth, sigopt_api_user_agent())
  sigopt_check(req)

  req
}

#' Create authentication for SigOpt API, using HTTP Basic Auth
#'
#' @param api_token SigOpt API token
#' @return http basic authentiation with api_token as username and no password
sigopt_auth <- function(api_token = sigopt_api_token()) {
  httr::authenticate(api_token, "")
}

#' Check content returned by the SigOpt API
#'
#' @param req result of request to the SigOpt API
#' @return invisible(), stops if there is an error status code
#' @seealso \code{\link{sigopt_GET}} and \code{\link{sigopt_POST}}, which call this function interally
sigopt_check <- function(req) {
  if (req$status_code < 400)return(invisible())

  message <- sigopt_parse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' Parse content returned by the SigOpt API
#'
#' @param req result of request to the SigOpt API
#' @return json decoding of request object
#' @seealso \code{\link{sigopt_GET}} and \code{\link{sigopt_POST}}, which call this function interally
sigopt_parse <- function(req) {
  text <- httr::content(req, as="text", encoding="UTF-8")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Get the SigOpt API token from the SIGOPT_API_TOKEN environment variable or user input
#'
#' @param force force entry of SigOpt API token, even if present
#' @return SigOpt API token
sigopt_api_token <- function(force = FALSE) {
  env <- Sys.getenv("SIGOPT_API_TOKEN")
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var SIGOPT_API_TOKEN to your SigOpt API token",
      call. = FALSE)
  }

  message("Couldn't find env var SIGOPT_API_TOKEN. See ?sigopt_api_token for more details.")
  message("Please enter your API_TOKEN and press enter:")
  api_token <- readline(": ")

  if (identical(api_token, "")) {
    stop("SigOpt API token entry failed", call. = FALSE)
  }

  message("Updating SIGOPT_API_TOKEN env var to API_TOKEN")
  Sys.setenv(SIGOPT_API_TOKEN = api_token)

  api_token
}

#' Get the SigOpt API url from the SIGOPT_API_URL environment variable or use default
#' Most users will be ok with the default value
#'
#' @return Base url for SigOpt API requests
#' @seealso \code{\link{sigopt_GET}} and \code{\link{sigopt_POST}}, which perform the HTTP requests
sigopt_api_url <- function() {
  env <- Sys.getenv("SIGOPT_API_URL")

  if (!identical(env, "")) return(env)

  "https://api.sigopt.com"
}

#' User agent for current version of SigOpt R API Client
#'
#' @return User agent
#' @seealso \code{\link{sigopt_GET}} and \code{\link{sigopt_POST}}, which perform the HTTP requests
sigopt_api_user_agent <- function() {
  httr::user_agent("SigOptR/0.0.1")
}
