# FUNCTION: flatten_ --------------------------------------------------------------------------
# Similar to purrr::flatten, but joins names with specified separater
flatten_ <- function(x, ..., sep = "_") {
  unlist(x, ...) %>% as.list %>% set_names(str_replace_all(names(.), "\\.", sep))
}

# FUNCTION: gh_token --------------------------------------------------------------------------
# Retrieve the GitHub personal access token from the following locations:
#  - github.token option
#  - GITHUB_TOKEN environment variable
#  - GITHUB_PAT environment variable
gh_token <- function() {
  token <- getOption("github.token")
  if (is.null(token)) {
    token <- Sys.getenv("GITHUB_TOKEN")
  }
  if (identical(token, "")) {
    token <- Sys.getenv("GITHUB_PAT")
  }
  if (identical(token, "")) {
    stop("Cannot find GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")
  }
  token
}
