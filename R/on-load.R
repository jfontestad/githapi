.onLoad <- function(libname, pkgname)
{
  # Set environment variables that do not already exist
  githapi_env <- list(
    GITHUB_API           = "https://api.github.com",
    GITHUB_OAUTH         = "https://github.com/login/oauth",
    GITHUB_PROXY         = "",
    GITHUB_TOKEN         = "",
    GITHUB_PAT           = "",
    GITHAPI_TOKEN        = "",
    GITHAPI_KEY          = "07f9a4157365992e4db8",
    GITHAPI_SECRET       = "4d705eb68ac93e524ba71872a8b8f2bae802d870",
    GITHAPI_REDIRECT_URI = "http://localhost:1410",
    GITHAPI_CACHE        = ""
  )

  toset <- sapply(names(githapi_env), function(e) identical(Sys.getenv(e), ""))
  if (any(toset)) {
    do.call(Sys.setenv, githapi_env[toset])
  }

  # Read environment variables, to set as options
  githapi_env <- as.list(Sys.getenv(names(githapi_env)))

  # Set github proxy
  if (identical(githapi_env$GITHUB_PROXY, "")) {
    githapi_env$GITHUB_PROXY <- NULL
  }

  # Set github token
  tokens <- c(
    githapi_env$GITHAPI_TOKEN,
    githapi_env$GITHUB_TOKEN,
    githapi_env$GITHUB_PAT)

  if (all(tokens == "")) {
    githapi_env$GITHUB_TOKEN <- NULL
  }
  else {
    githapi_env$GITHUB_TOKEN <- tokens[tokens != ""][[1]]
  }

  # Set github token cache
  if (identical(githapi_env$GITHAPI_CACHE, "")) {
    githapi_env$GITHAPI_CACHE <- FALSE
  }

  # Set R options
  options(
    # GitHub API
    github.api                = githapi_env$GITHUB_API,            # The base address of the GitHub API
    github.oauth              = githapi_env$GITHUB_OAUTH,          # The base address of the GitHub OAuth URL
    github.proxy              = githapi_env$GITHUB_PROXY,          # The proxy to use to access GitHub
    github.token              = githapi_env$GITHUB_TOKEN,          # The GitHub token

    # githapi application
    githapi.key               = githapi_env$GITHAPI_KEY,           # The application ID for accessing GitHub
    githapi.secret            = githapi_env$GITHAPI_SECRET,        # The secret for the application to access GitHub
    githapi.redirect_uri      = githapi_env$GITHAPI_REDIRECT_URI,  # The URI to return the GitHub token to
    githapi.cache             = githapi_env$GITHAPI_CACHE          # Location for cached token - blank means do not cache
  )
}
