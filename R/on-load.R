.onLoad <- function(libname, pkgname) {
  url <- Sys.getenv("GITHUB_URL")
  if (identical(url, ""))
    url <- "https://github.com"

  api <- Sys.getenv("GITHUB_API")
  if (identical(api, ""))
    api <- "https://api.github.com"

  token <- Sys.getenv("GITHUB_TOKEN")
  if (identical(token, ""))
    token <- Sys.getenv("GITHUB_PAT")
  if (identical(token, ""))
    message("Cannot find a GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")

  options(
    github.url = url,
    github.api = api)
}
