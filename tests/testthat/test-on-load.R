context("on load")


# SETUP ------------------------------------------------------------------------

temp_path <- tempfile("")

setup(suppressMessages({

  dir.create(temp_path)

  test_config <- list(
    prod = list(
      github = list(
        api   = "https://prod-api.github.com",
        oauth = "https://prod-github.com/login/oauth"
      ),
      githapi = list(
        id     = "1329854079b03237d8a4",
        secret = "76c1a1be132ac20e2dc7431c2363b849a3b94eed",
        cache  = "~/.prod-githapi.oauth"
      ),
      newapp = list(
        id     = "b17156cbd901aa75e41c",
        secret = "dd771c62bcb66834cab30225c34d010c4ca9423d",
        cache  = "~/.prod-newapp.oauth"
      )
    ),
    test = list(
      github = list(
        api   = "https://test-api.github.com",
        oauth = "https://test-github.com/login/oauth"
      ),
      githapi = list(
        id     = "748012285b47946a70e1",
        secret = "d2e69443e73b38821b602235b582e2ac4eb1a815",
        cache  = "~/.test-githapi.oauth"
      )
    )
  )

  jsonlite::write_json(
    test_config,
    path       = file.path(temp_path, "test-config.json"),
    pretty     = TRUE,
    auto_unbox = TRUE
  )

  invalid_config <- list(
    prod = list(
      githapi = list(
        api    = "https://prod-api.github.com",
        oauth  = "https://prod-github.com/login/oauth",
        id     = "1329854079b03237d8a4",
        secret = "76c1a1be132ac20e2dc7431c2363b849a3b94eed",
        cache  = "~/.prod-githapi.oauth"
      )
    )
  )

  jsonlite::write_json(
    invalid_config,
    path       = file.path(temp_path, "invalid-config.json"),
    pretty     = TRUE,
    auto_unbox = TRUE
  )

}))

teardown(suppressMessages({

  if (dir.exists(temp_path)) {
    unlink(temp_path, recursive = TRUE)
  }

}))


test_that("Options are  set correctly on load", {

  expect_identical(
    getOption("github.api"),
    "https://api.github.com"
  )
  expect_identical(
    getOption("github.oauth"),
    "https://github.com/login/oauth"
  )

  expect_null(getOption("github.proxy"))

  expect_identical(
    getOption("githapi.id"),
    "07f9a4157365992e4db8"
  )
  expect_identical(
    getOption("githapi.secret"),
    "4d705eb68ac93e524ba71872a8b8f2bae802d870"
  )
  expect_identical(
    getOption("githapi.cache"),
    "~/.githapi.oauth"
  )

})


test_that("Specifying environment variables overrides default values", {

  original_env_vars <- list(
    GITHUB_API     = Sys.getenv("GITHUB_API"),
    GITHUB_OAUTH   = Sys.getenv("GITHUB_OAUTH"),
    GITHUB_PROXY   = Sys.getenv("GITHUB_PROXY"),
    GITHAPI_TOKEN  = Sys.getenv("GITHAPI_TOKEN"),
    GITHAPI_ID     = Sys.getenv("GITHAPI_ID"),
    GITHAPI_SECRET = Sys.getenv("GITHAPI_SECRET"),
    GITHAPI_CACHE  = Sys.getenv("GITHAPI_CACHE")
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    .onLoad()
  })

  Sys.setenv(GITHUB_API     = "https://github.acme.com/v3/api")
  Sys.setenv(GITHUB_OAUTH   = "https://github.acme.com/login/oauth")
  Sys.setenv(GITHUB_PROXY   = "https://proxy.acme.com")
  Sys.setenv(GITHAPI_TOKEN  = "0cbe856d67619782748953e40ee97940b80b368a")
  Sys.setenv(GITHAPI_ID     = "1d78e5299e53e92d9289")
  Sys.setenv(GITHAPI_SECRET = "3596b93db4ba7a75a642a064709b2bd1350a68a5")
  Sys.setenv(GITHAPI_CACHE  = "~/.httr.oauth")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://github.acme.com/v3/api"
  )
  expect_identical(
    getOption("github.oauth"),
    "https://github.acme.com/login/oauth"
  )
  expect_identical(
    getOption("github.proxy"),
    "https://proxy.acme.com"
  )
  expect_identical(
    getOption("github.token"),
    "0cbe856d67619782748953e40ee97940b80b368a"
  )
  expect_identical(
    getOption("githapi.id"),
    "1d78e5299e53e92d9289"
  )
  expect_identical(
    getOption("githapi.secret"),
    "3596b93db4ba7a75a642a064709b2bd1350a68a5"
  )
  expect_identical(
    getOption("githapi.cache"),
    "~/.httr.oauth"
  )

})


test_that("Setting the token in different ways works correctly", {

  original_tokens <- list(
    GITHAPI_TOKEN = Sys.getenv("GITHAPI_TOKEN"),
    GITHUB_PAT    = Sys.getenv("GITHUB_PAT"),
    GITHUB_TOKEN  = Sys.getenv("GITHUB_TOKEN")
  )

  on.exit({
    do.call(Sys.setenv, original_tokens)
    .onLoad()
  })

  Sys.setenv(GITHAPI_TOKEN = "")
  Sys.setenv(GITHUB_PAT    = "")
  Sys.setenv(GITHUB_TOKEN  = "a338135a3e73529240626790332b42461c377621")

  .onLoad()

  expect_identical(
    getOption("github.token"),
    "a338135a3e73529240626790332b42461c377621"
  )

  Sys.setenv(GITHUB_PAT = "8255b036710a9a2a23e4ae807096e1b239b284b1")

  .onLoad()

  expect_identical(
    getOption("github.token"),
    "8255b036710a9a2a23e4ae807096e1b239b284b1"
  )

  Sys.setenv(GITHAPI_TOKEN = "6a0aa8e47d7c30207480c60beaa377d01e003727")

  .onLoad()

  expect_identical(
    getOption("github.token"),
    "6a0aa8e47d7c30207480c60beaa377d01e003727"
  )

})


test_that("Setting the API URL in different ways works correctly", {

  original_apis <- list(
    GITHUB_API     = Sys.getenv("GITHUB_API"),
    GITHUB_API_URL = Sys.getenv("GITHUB_API_URL")
  )

  on.exit({
    do.call(Sys.setenv, original_apis)
    .onLoad()
  })

  Sys.setenv(GITHUB_API     = "")
  Sys.setenv(GITHUB_API_URL = "https://github-api-url.acme.com/login/oauth")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://github-api-url.acme.com/login/oauth"
  )

  Sys.setenv(GITHUB_API = "https://github-api.acme.com/login/oauth")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://github-api.acme.com/login/oauth"
  )

})


test_that("Setting the GITHAPI_CONFIG reads the config file", {

  original_env_vars <- list(
    GITHAPI_CONFIG = Sys.getenv("GITHAPI_CONFIG"),
    GITHUB_API     = Sys.getenv("GITHUB_API"),
    GITHUB_API_URL = Sys.getenv("GITHUB_API_URL"),
    GITHUB_OAUTH   = Sys.getenv("GITHUB_OAUTH"),
    GITHAPI_ID     = Sys.getenv("GITHAPI_ID"),
    GITHAPI_SECRET = Sys.getenv("GITHAPI_SECRET"),
    GITHAPI_CACHE  = Sys.getenv("GITHAPI_CACHE")
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    .onLoad()
  })

  Sys.setenv(GITHAPI_CONFIG = file.path(temp_path, "test-config.json"))
  Sys.setenv(GITHUB_API     = "")
  Sys.setenv(GITHUB_API_URL = "")
  Sys.setenv(GITHUB_OAUTH   = "")
  Sys.setenv(GITHAPI_ID     = "")
  Sys.setenv(GITHAPI_SECRET = "")
  Sys.setenv(GITHAPI_CACHE  = "")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://prod-api.github.com"
  )
  expect_identical(
    getOption("github.oauth"),
    "https://prod-github.com/login/oauth"
  )
  expect_identical(
    getOption("githapi.id"),
    "1329854079b03237d8a4"
  )
  expect_identical(
    getOption("githapi.secret"),
    "76c1a1be132ac20e2dc7431c2363b849a3b94eed"
  )
  expect_identical(
    getOption("githapi.cache"),
    "~/.prod-githapi.oauth"
  )

})


test_that("Setting a different app sets the configuration correctly", {

  original_env_vars <- list(
    GITHAPI_CONFIG = Sys.getenv("GITHAPI_CONFIG"),
    GITHAPI_APP    = Sys.getenv("GITHAPI_APP"),
    GITHUB_API     = Sys.getenv("GITHUB_API"),
    GITHUB_API_URL = Sys.getenv("GITHUB_API_URL"),
    GITHUB_OAUTH   = Sys.getenv("GITHUB_OAUTH"),
    GITHAPI_ID     = Sys.getenv("GITHAPI_ID"),
    GITHAPI_SECRET = Sys.getenv("GITHAPI_SECRET"),
    GITHAPI_CACHE  = Sys.getenv("GITHAPI_CACHE")
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    .onLoad()
  })

  Sys.setenv(GITHAPI_CONFIG = file.path(temp_path, "test-config.json"))
  Sys.setenv(GITHAPI_APP  = "newapp")
  Sys.setenv(GITHUB_API     = "")
  Sys.setenv(GITHUB_API_URL = "")
  Sys.setenv(GITHUB_OAUTH   = "")
  Sys.setenv(GITHAPI_ID     = "")
  Sys.setenv(GITHAPI_SECRET = "")
  Sys.setenv(GITHAPI_CACHE  = "")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://prod-api.github.com"
  )
  expect_identical(
    getOption("github.oauth"),
    "https://prod-github.com/login/oauth"
  )
  expect_identical(
    getOption("githapi.id"),
    "b17156cbd901aa75e41c"
  )
  expect_identical(
    getOption("githapi.secret"),
    "dd771c62bcb66834cab30225c34d010c4ca9423d"
  )
  expect_identical(
    getOption("githapi.cache"),
    "~/.prod-newapp.oauth"
  )

})


test_that("Setting the ENVIRONMENT sets the configuration correctly", {

  original_env_vars <- list(
    GITHAPI_CONFIG = Sys.getenv("GITHAPI_CONFIG"),
    ENVIRONMENT    = Sys.getenv("ENVIRONMENT"),
    GITHUB_API     = Sys.getenv("GITHUB_API"),
    GITHUB_API_URL = Sys.getenv("GITHUB_API_URL"),
    GITHUB_OAUTH   = Sys.getenv("GITHUB_OAUTH"),
    GITHAPI_ID     = Sys.getenv("GITHAPI_ID"),
    GITHAPI_SECRET = Sys.getenv("GITHAPI_SECRET"),
    GITHAPI_CACHE  = Sys.getenv("GITHAPI_CACHE")
  )

  on.exit({
    do.call(Sys.setenv, original_env_vars)
    .onLoad()
  })

  Sys.setenv(GITHAPI_CONFIG = file.path(temp_path, "test-config.json"))
  Sys.setenv(ENVIRONMENT = "test")
  Sys.setenv(GITHUB_API     = "")
  Sys.setenv(GITHUB_API_URL = "")
  Sys.setenv(GITHUB_OAUTH   = "")
  Sys.setenv(GITHAPI_ID     = "")
  Sys.setenv(GITHAPI_SECRET = "")
  Sys.setenv(GITHAPI_CACHE  = "")

  .onLoad()

  expect_identical(
    getOption("github.api"),
    "https://test-api.github.com"
  )
  expect_identical(
    getOption("github.oauth"),
    "https://test-github.com/login/oauth"
  )
  expect_identical(
    getOption("githapi.id"),
    "748012285b47946a70e1"
  )
  expect_identical(
    getOption("githapi.secret"),
    "d2e69443e73b38821b602235b582e2ac4eb1a815"
  )
  expect_identical(
    getOption("githapi.cache"),
    "~/.test-githapi.oauth"
  )

})


test_that("An invalid configuration file gives a warning", {

  original_config = Sys.getenv("GITHAPI_CONFIG")

  on.exit({
    Sys.setenv(GITHAPI_CONFIG = original_config)
    .onLoad()
  })

  Sys.setenv(GITHAPI_CONFIG = file.path(temp_path, "invalid-config.json"))

  expect_warning(
    .onLoad(),
    "GitHub configuration not set in specified configuration file"
  )

})
