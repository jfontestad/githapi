.onLoad <- function(libname, pkgname)
{
  # Read configuration file
  config <- jsonlite::read_json(system.file("config.json", package = "githapi"), simplifyVector = TRUE)
  config_env <- if (Sys.getenv("ENVIRONMENT") == "") "prod" else Sys.getenv("ENVIRONMENT")

  if (config_env %in% names(config)) {
    config <- config[[config_env]]
  } else {
    config <- config[[1]]
  }

  # Set github token
  tokens <- c(
    Sys.getenv("GITHAPI_TOKEN"),
    Sys.getenv("GITHUB_TOKEN"),
    Sys.getenv("GITHUB_PAT"))

  if (all(tokens == "")) {
    config$token <- NULL
  }
  else {
    config$token <- tokens[tokens != ""][[1]]
  }

  # Set R options
  options(
    # GitHub API
    github.api           = config$api,           # The base address of the GitHub API
    github.oauth         = config$oauth,         # The base address of the GitHub OAuth URL
    github.proxy         = config$proxy,         # The proxy to use to access GitHub
    github.token         = config$token,         # The GitHub token

    # githapi application
    githapi.key          = config$key,           # The application ID for accessing GitHub
    githapi.secret       = config$secret,        # The secret for the application to access GitHub
    githapi.redirect_uri = config$redirect_uri,  # The URI to return the GitHub token to
    githapi.cache        = config$cache          # Location for cached token - FALSE means do not cache
  )
}
