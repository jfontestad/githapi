.onLoad <- function(libname, pkgname)
{
  # Read configuration file
  if (Sys.getenv("GITHAPI_CONFIG") != "" && file.exists(Sys.getenv("GITHAPI_CONFIG"))) {
    config_path <- Sys.getenv("GITHAPI_CONFIG")
  } else {
    config_path <- system.file("config.json", package = "githapi")
  }
  config <- jsonlite::read_json(config_path, simplifyVector = TRUE)

  if (Sys.getenv("ENVIRONMENT") %in% names(config)) {
    config <- config[[Sys.getenv("ENVIRONMENT")]]
  } else {
    config <- config[[1]]
  }

  if ("github" %in% names(config)) {
    gh_cfg <- config[["github"]]
  } else {
    warning("GitHub configuration not set in specified configuration file")
  }

  if (Sys.getenv("GITHAPI_APP") %in% names(config)) {
    app_cfg <- config[[Sys.getenv("GITHAPI_APP")]]
  } else {
    app_cfg <- config[names(config) != "github"][[1]]
  }

  # Set github token
  tokens <- c(
    Sys.getenv("GITHAPI_TOKEN"),
    Sys.getenv("GITHUB_TOKEN"),
    Sys.getenv("GITHUB_PAT"))

  if (any(tokens != "")) {
    gh_cfg$token <- tokens[tokens != ""][[1]]
  }

  # Set R options
  options(
    # GitHub API
    github.api       = gh_cfg$api,        # The base address of the GitHub API
    github.oauth     = gh_cfg$oauth,      # The base address of the GitHub OAuth URL
    github.token     = gh_cfg$token,      # The GitHub token
    github.proxy     = gh_cfg$proxy,      # The proxy to use to access GitHub

    # githapi application
    githapi.key      = app_cfg$key,       # The application ID for accessing GitHub
    githapi.secret   = app_cfg$secret,    # The secret for the application to access GitHub
    githapi.cache    = app_cfg$cache      # Location for cached token - FALSE means do not cache
  )
}
