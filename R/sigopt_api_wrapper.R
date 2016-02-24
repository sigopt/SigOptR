install.packages(c("httr", "jsonlite"), repos="http://cran.us.r-project.org")

library(httr)
library(jsonlite)

#' Create an experiment
#'
#' @param body POST body of create request
#' @return experiment created by SigOpt
#' @export
#' @examples
#' create_experiment(list(
#'   name="R test experiment",
#'   parameters=list(
#'     list(name="x1", type="double", bounds=list(min=0, max=100)),
#'     list(name="x2", type="double", bounds=list(min=0, max=100))
#'   )
#' ))
create_experiment <- function(body) {
  req <- sigopt_POST("v1/experiments", body)
  sigopt_parse(req)
}

#' Create a suggestion for an experiment
#'
#' @param experiment_id the id of an experiment to create an suggestion for
#' @param body POST body of create request
#' @return suggestion created by SigOpt
#' @export
#' @examples
#' create_suggestion(2269)
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
#' create_observation(2269, list(suggestion=483696, value=99.08))
#' create_observation(2269, list(suggestion=483696, value=99.58, value_stddev=0.1))
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
#' @examples
#' sigopt_GET('/v1/experiments')
#' sigopt_GET('/v1/experiments/2269/suggestions', query=list(state="open"))
sigopt_GET <- function(path, query = NULL, ..., api_token = sigopt_api_token()) {
  auth <- sigopt_auth(api_token)
  req <- httr::GET("https://api.sigopt.com", path = path, query = query, auth)
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
#' @examples
#' sigopt_POST("v1/experiments", NULL)
sigopt_POST <- function(path, body, ..., api_token = sigopt_api_token()) {
  auth <- sigopt_auth(api_token)

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  req <- httr::POST("https://api.sigopt.com", path = path, body = body_json, encode = "json", auth, ...)
  sigopt_check(req)

  req
}

#' Create authentication for SigOpt API, using HTTP Basic Auth
#'
#' @param api_token SigOpt API token
#' @return http basic authentiation with api_token as username and no password
#' @examples
#' sigopt_auth()
#' sigopt_auth("client_token")
sigopt_auth <- function(api_token = sigopt_api_token()) {
  httr::authenticate(api_token, "")
}

#' Check content returned by the SigOpt API
#'
#' @param req result of request to the SigOpt API
#' @return invisible(), stops if there is an error status code
#' @seealso \code{\link{sigopt_GET}} and \code{\link{sigopt_POST}}, which call this function interally
#' @examples
#' sigopt_check(GET("https://api.sigopt.com", path="v1/experiments", sigopt_auth(sigopt_api_token())))
#' sigopt_check(POST("https://api.sigopt.com", path="v1/experiments", sigopt_auth(sigopt_api_token())))
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
#' @examples
#' sigopt_parse(GET("https://api.sigopt.com", path="v1/experiments", sigopt_auth(sigopt_api_token())))
#' sigopt_parse(POST("https://api.sigopt.com", path="v1/experiments", sigopt_auth(sigopt_api_token())))
sigopt_parse <- function(req) {
  text <- httr::content(req, as="text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Get the SigOpt API token from the SIGOPT_API_TOKEN environment variable or user input
#'
#' @param force force entry of SigOpt API token, even if present
#' @return SigOpt API token
#' @examples
#' sigopt_api_token()
#' sigopt_api_token(TRUE)
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

#' Whether or not the SigOpt API token is properly configured
#' @return TRUE or FALSE
#' @seealso \code{\link{sigopt_api_token}}, which this function uses to get the SigOpt API token
#' @export
#' @examples
#' sigopt_has_api_token()
sigopt_has_api_token <- function() !identical(sigopt_api_token(), "")

