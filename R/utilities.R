# FUNCTION: flatten_ --------------------------------------------------------------------------
# Similar to purrr::flatten, but joins names with specified separater
flatten_ <- function(x, ..., sep = "_") {
  unlist(x, ...) %>% as.list %>% set_names(str_replace_all(names(.), "\\.", sep))
}

# FUNCTION: gh_token --------------------------------------------------------------------------
# Retrieve the GitHub personal access token from environment variables
gh_token <- function() {
  token <- Sys.getenv("GITHUB_TOKEN")
  if (identical(token, "")) {
    token <- Sys.getenv("GITHUB_PAT")
  }
  if (identical(token, "")) {
    stop("Cannot find GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")
  }
  token
}

# FUNCTION: gh_api_url ------------------------------------------------------------------------
# Retrieve the GitHub API URL from environment variable or use default
gh_api_url <- function() {
  api_url <- Sys.getenv("GITHUB_API_URL")
  if (identical(api_url, "")) {
    api_url <- "https://api.github.com"
  }
  api_url
}
