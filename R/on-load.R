.onLoad <- function(libname, pkgname) {
  token <- Sys.getenv("GITHUB_TOKEN")
  if (identical(token, ""))
    token <- Sys.getenv("GITHUB_PAT")
  if (identical(token, ""))
    packageStartupMessage("Cannot find a GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")

  github_env <- list(
    GITHUB_API   = "https://api.github.com",
    GITHUB_TOKEN = token,
    GITHAPI_LOGS = "")

  toset <- sapply(names(github_env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, github_env[toset])

  github_options <- as.list(Sys.getenv(names(github_env)))
  names(github_options) <- tolower(sub("_", "\\.", names(github_env)))
  do.call(options, github_options)

  options(
    githapi.debug_types  = c("INFO", "WARNING", "ERROR"),  # The type of debug messages to report
    githapi.debug_level = 1)                              # The level of debug messages to report
}
