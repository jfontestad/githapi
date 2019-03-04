.onLoad <- function(libname, pkgname) {

  token <- Sys.getenv("GITHUB_TOKEN")
  if (identical(token, ""))
    token <- Sys.getenv("GITHUB_PAT")
  if (identical(token, ""))
    packageStartupMessage("Cannot find a GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")

  github_env <- list(
    GITHUB_API        = "https://api.github.com",
    GITHUB_TOKEN      = token,
    GITHAPI_MSG_LEVEL = "1",
    GITHAPI_MSG_TYPES = "INFO|WARNING|ERROR",
    GITHAPI_LOGS      = "")

  toset <- sapply(names(github_env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, github_env[toset])
  githapi_env <- as.list(Sys.getenv(names(github_env)))

  if (!identical(githapi_env$GITHAPI_LOGS, "")) {
    githapi_env$GITHAPI_LOGS <- paste0(githapi_env$GITHAPI_LOGS, "/", "githapi-", Sys.info()[["login"]], "-", Sys.Date(), ".log")
  }

  options(
    github.api    = githapi_env$GITHUB_API,
    github.token  = githapi_env$GITHUB_TOKEN,
    msgr.level    = suppressWarnings(as.integer(githapi_env$GITHAPI_MSG_LEVEL)),
    msgr.types    = strsplit(githapi_env$GITHAPI_MSG_TYPES, split = "\\|")[[1]],
    msgr.log_path = normalizePath(githapi_env$GITHAPI_LOGS, winslash = "/", mustWork = FALSE))

}
