.onLoad <- function(libname, pkgname)
{
  # Set environment variables that do not already exist
  githapi_env <- list(
    GITHUB_API        = "https://api.github.com",
    GITHUB_TOKEN      = "",
    GITHUB_PAT        = "",
    GITHAPI_MSG_LEVEL = "1",
    GITHAPI_MSG_TYPES = "INFO|WARNING|ERROR",
    GITHAPI_LOGS      = ""
  )

  toset <- sapply(names(githapi_env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, githapi_env[toset])

  # Read environment variables, to set as options
  githapi_env <- as.list(Sys.getenv(names(githapi_env)))

  # Set github token
  if (identical(githapi_env$GITHUB_TOKEN, ""))
  {
    githapi_env$GITHUB_TOKEN <- githapi_env$GITHUB_PAT
  }

  # Set log file name
  if (!identical(githapi_env$GITHAPI_LOGS, ""))
  {
    githapi_env$GITHAPI_LOGS <- paste0(githapi_env$GITHAPI_LOGS, "/", "githapi-", Sys.info()[["login"]], "-", Sys.Date(), ".log")
  }

  # Set R options
  options(
    # Base Configuration
    scipen                    = 999,                         # Prevent printing in scientific notation
    warning.length            = 8170,                        # Set warning length to maximum

    # GITHUB API
    github.api                = githapi_env$GITHUB_API,      # The base address of the GitHub API
    github.token              = githapi_env$GITHUB_TOKEN,    # The GitHub token

    # Message Configuration
    msgr.msg_level            = suppressWarnings(as.integer(githapi_env$GITHAPI_MSG_LEVEL)),
    msgr.msg_types            = strsplit(githapi_env$GITHAPI_MSG_TYPES, split = "\\|")[[1]],
    msgr.log_path             = normalizePath(githapi_env$GITHAPI_LOGS, winslash = "/", mustWork = FALSE)
  )
}
