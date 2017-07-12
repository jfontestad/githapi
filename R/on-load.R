.onLoad <- function(libname, pkgname) {
  token <- Sys.getenv("GITHUB_TOKEN")
  if (identical(token, ""))
    token <- Sys.getenv("GITHUB_PAT")
  if (identical(token, ""))
    message("Cannot find a GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")

  github_env <- list(
    GITHUB_URL = "https://github.com",
    GITHUB_API = "https://api.github.com",
    GITHUB_TOKEN = token)

  toset <- sapply(names(github_env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) do.call(Sys.setenv, github_env[toset])

  github_options <- as.list(Sys.getenv(names(github_env)))
  names(github_options) <- tolower(sub("_", "\\.", names(github_env)))
  do.call(options, github_options)
}
