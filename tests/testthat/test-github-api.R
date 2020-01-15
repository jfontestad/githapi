context("github api")


# TEST: gh_token ------------------------------------------------------------------------------

test_that("gh_token returns a valid GitHub personal access token", {

  existing_msgr_level <- getOption("msgr.level")
  on.exit(options(msgr.level = existing_msgr_level), add = TRUE)
  options(msgr.level = 10)

  token1 <- sample(c(0:9, letters[1:6]), size = 40, replace = TRUE) %>% paste(collapse = "")

  expect_message(
    token1_result <- gh_token(token = token1),
    "Using supplied token")

  expect_identical(token1_result, token1)

  token2 <- sample(c(0:9, letters[1:6]), size = 40, replace = TRUE) %>% paste(collapse = "")

  existing_token <- getOption("github.token")
  on.exit(options(github.token = existing_token), add = TRUE)
  options(github.token = token2)

  expect_message(
    token2_result <- gh_token(),
    "Using supplied token")

  expect_identical(token2_result, token2)

})

test_that("gh_token returns a valid GitHub OAuth token", {

  skip_if_not(interactive(), "OAuth authentication must be run manually")

  existing_msgr_level    <- getOption("msgr.level")
  existing_githapi_cache <- getOption("githapi.cache")
  options(msgr.level = 10, githapi.cache = "~/.githapi.oauth")
  on.exit(options(msgr.level = existing_msgr_level, githapi.cache = existing_githapi_cache))

  expect_error(
    gh_token(token = NULL, secret = "suhfdieudhisauhf"),
    "incorrect client credentials")

  expect_message(
    new_token <- gh_token(token = NULL),
    "Retrieving new token")

  expect_is(new_token, "Token")

  expect_message(
    cached_token <- gh_token(token = NULL),
    "Retrieving cached token")

  expect_is(cached_token, "Token")

})

test_that("gh_token throws an error if an invalid token is specified", {

  expect_error(gh_token(token = "Bob"), "'token' must be a SHA or a Token object")

})


# TEST: gh_url --------------------------------------------------------------------------------

test_that("gh_url returns a valid URL for the GitHub API", {

  expect_identical(
    gh_url("repos"),
    file.path(getOption("github.api"), "repos"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/refs", "heads/master"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/refs", "heads/master")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1)),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1, type = "bob"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1, type = "bob")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list()),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384"))

})


# TEST: gh_request ----------------------------------------------------------------------------

test_that("gh_request can GET, POST, PATCH and DELETE a tag in the specified repository", {

  master_sha <- view_shas(refs = "master", repo = "ChadGoymer/test-githapi")[[1]]
  test_sha   <- view_shas(refs = "test-files", repo = "ChadGoymer/test-githapi")[[1]]

  test_tag <- str_c("refs/tags/test-gh-request-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))

  created_tag <- gh_request(
    url     = "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs",
    type    = "POST",
    payload = list(ref = test_tag, sha = master_sha))

  expect_is(created_tag, "list")
  expect_identical(created_tag$ref, test_tag)
  expect_identical(created_tag$object$sha, master_sha)

  expect_identical(attr(created_tag, "url"), "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs")
  expect_identical(attr(created_tag, "request"), "POST")
  expect_identical(attr(created_tag, "status"), 201L)
  expect_true(length(attr(created_tag, "header")) > 1)

  viewed_tag <- gh_request(str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag), "GET")

  expect_is(viewed_tag, "list")
  expect_identical(viewed_tag$ref, test_tag)
  expect_identical(viewed_tag$object$sha, master_sha)

  expect_identical(attr(viewed_tag, "url"), str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag))
  expect_identical(attr(viewed_tag, "request"), "GET")
  expect_identical(attr(viewed_tag, "status"), 200L)
  expect_true(length(attr(viewed_tag, "header")) > 1)

  updated_tag <- gh_request(
    url     = str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag),
    type    = "PATCH",
    payload = list(sha = test_sha))

  expect_is(updated_tag, "list")
  expect_identical(updated_tag$ref, test_tag)
  expect_identical(updated_tag$object$sha, test_sha)

  expect_identical(attr(updated_tag, "url"), str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag))
  expect_identical(attr(updated_tag, "request"), "PATCH")
  expect_identical(attr(updated_tag, "status"), 200L)
  expect_true(length(attr(updated_tag, "header")) > 1)

  deleted_tag <- gh_request(str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag), "DELETE")

  expect_is(deleted_tag, "list")

  expect_identical(attr(deleted_tag, "url"), str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag))
  expect_identical(attr(deleted_tag, "request"), "DELETE")
  expect_identical(attr(deleted_tag, "status"), 204L)
  expect_true(length(attr(deleted_tag, "header")) > 1)

  expect_error(gh_request(str_c("https://api.github.com/repos/ChadGoymer/test-githapi/git/", test_tag), "GET"))

})

test_that("gh_request can make a request using an OAuth token", {

  skip_if_not(interactive(), "OAuth authentication must be run manually")

  existing_msgr_level    <- getOption("msgr.level")
  existing_githapi_cache <- getOption("githapi.cache")
  options(msgr.level = 10, githapi.cache = "~/.githapi.oauth")
  on.exit(options(msgr.level = existing_msgr_level, githapi.cache = existing_githapi_cache))

  master <- "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs/heads/master" %>%
    gh_request("GET", token = NULL)

  expect_is(master, "list")
  expect_identical(master$ref, "refs/heads/master")

  expect_identical(attr(master, "url"), "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs/heads/master")
  expect_identical(attr(master, "request"), "GET")
  expect_identical(attr(master, "status"), 200L)
  expect_true(length(attr(master, "header")) > 1)

})


# TEST: gh_page -------------------------------------------------------------------------------

test_that("gh_page returns a list of specified length", {

  users_20 <- gh_page(
    url   = file.path(getOption("github.api"), "users"),
    n_max = 20)

  expect_is(users_20, "list")
  expect_identical(length(users_20), 20L)

  expect_identical(attr(users_20, "url"), "https://api.github.com/users?per_page=20")
  expect_identical(attr(users_20, "request"), "GET")
  expect_identical(attr(users_20, "status"), 200L)
  expect_true(length(attr(users_20, "header")) > 1)

  users_150 <- gh_page(
    url   = file.path(getOption("github.api"), "users"),
    n_max = 150)

  expect_is(users_150, "list")
  expect_identical(length(users_150), 150L)

  expect_identical(
    attr(users_150, "url"),
    c("https://api.github.com/users?per_page=100", "https://api.github.com/users?per_page=50&since=135"))
  expect_identical(attr(users_150, "request"), "GET")
  expect_identical(attr(users_150, "status"), c(200L, 200L))
  expect_true(length(attr(users_150, "header")) > 1)

})

test_that("gh_page still works when the endpoint returns a singular response rather than a collection", {

  master <- gh_page("https://api.github.com/repos/ChadGoymer/test-githapi/git/refs/heads/master")

  expect_is(master, "list")
  expect_identical(master$ref, "refs/heads/master")

  expect_identical(attr(master, "url"), "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs/heads/master?per_page=100")
  expect_identical(attr(master, "request"), "GET")
  expect_identical(attr(master, "status"), 200L)
  expect_true(length(attr(master, "header")) > 1)

})


# TEST: gh_find -------------------------------------------------------------------------------

test_that("gh_find locate an entity with the specified property value", {

  users_150 <- gh_page(
    url   = file.path(getOption("github.api"), "users"),
    n_max = 150)

  user_25 <- gh_find(
    url      = file.path(getOption("github.api"), "users"),
    property = "login",
    value    = users_150[[25]]$login)

  expect_is(user_25, "list")
  expect_identical(user_25$login, users_150[[25]]$login)

  expect_identical(attr(user_25, "url"), "https://api.github.com/users?per_page=100")
  expect_identical(attr(user_25, "request"), "GET")
  expect_identical(attr(user_25, "status"), 200L)
  expect_true(length(attr(user_25, "header")) > 1)

  user_125 <- gh_find(
    url      = file.path(getOption("github.api"), "users"),
    property = "login",
    value    = users_150[[125]]$login)

  expect_is(user_125, "list")
  expect_identical(user_125$login, users_150[[125]]$login)

  expect_identical(attr(user_125, "url"), "https://api.github.com/users?per_page=100&since=135")
  expect_identical(attr(user_125, "request"), "GET")
  expect_identical(attr(user_125, "status"), 200L)
  expect_true(length(attr(user_125, "header")) > 1)

})

test_that("gh_find throws an error if it cannot find the specified property value", {

  expect_error(
    gh_find(
      url      = "https://api.github.com/repos/ChadGoymer/test-githapi/git/refs/heads",
      property = "ref",
      value    = "refs/heads/bob"),
    "Could not find an entity with 'ref' equal to 'refs/heads/bob'")

})


# TEST: print.github --------------------------------------------------------------------------

test_that("print.github correctly prints lists", {

  test_github_list <- structure(
    list(Name = "Bob"),
    class   = c("github", "list"),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff"))

  list_output <- utils::capture.output(test_github_list)

  expect_identical(
    list_output,
    c("\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39mList of 1",
      "  Name: chr \"Bob\""))

})

test_that("print.github correctly prints vectors", {

  test_github_string <- structure(
    "Bob",
    class   = c("github", "character"),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff"))

  string_output <- utils::capture.output(test_github_string)

  expect_identical(
    string_output,
    c("\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39m[1] \"Bob\""))

})

test_that("print.github correctly prints data.frames", {

  df <- tibble(Name = "Bob", role = "Developer")
  test_github_df <- structure(
    df,
    class   = c("github", class(df)),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff"))

  df_output <- utils::capture.output(test_github_df)

  expect_identical(
    df_output,
    c("\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39m# A tibble: 1 x 2",
      "  Name  role     ",
      "* <chr> <chr>    ",
      "1 Bob   Developer"))

})

test_that("print.github correctly prints multiple URLs", {

  test_github_urls <- structure(
    "Bob",
    class   = c("github", "character"),
    url     = c("https://somwhere/Bob1", "https://somwhere/Bob2", "https://somwhere/Bob3"),
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff"))

  urls_output <- utils::capture.output(test_github_urls)

  expect_identical(
    urls_output,
    c("\033[34m# GET \033[4mhttps://somwhere/Bob1\033[24m",
      "# GET \033[4mhttps://somwhere/Bob2\033[24m",
      "# GET \033[4m...\033[24m",
      "\033[39m[1] \"Bob\""))

})
