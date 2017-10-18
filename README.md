githapi
=======

User-friendly access to the GitHub API for R, consistent with the
tidyverse.

Overview
--------

GitHub is a wildly popular host for version controlled code, but also
data. This package provides a user-friendly way to access content within
GitHub through v3 of its [API](https://developer.github.com/v3/). The
functions are also consistent with the
[tidyverse](http://www.tidyverse.org/) approach and return tibbles where
possible.

Installation
------------

The easiest way to install githapi is with the devtools package:

    library(devtools)
    install_github("ChadGoymer/githapi")

Authorisation
-------------

In order to access repositories in GitHub authentication is required, by
setting a [Personal Access
Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/).
githapi identifies the personal token in two possible ways:

1.  On package load - if set as an environment variable named either
    "GITHUB\_TOKEN" or "GITHUB\_PAT"
2.  At any time - if set as an R option called "github.token"

Setting an R option will override the environment variable.

Usage
-----

Most of the functions in the package are prefixed by `gh_`, except for
predicate functions which are prefixed with `is_`.

    library(tidyverse)
    library(githapi)

### Getting information about a user's repositories

The main reason for using githapi is to extract information from GitHub.
For example to find all the repositories a user has use the function
`gh_repositories()`:

    gh_repositories("ChadGoymer")

    ## # A tibble: 13 x 11
    ##                    name owner_login owner_type
    ##                   <chr>       <chr>      <chr>
    ##  1 chadgoymer.github.io  ChadGoymer       User
    ##  2         git-training  ChadGoymer       User
    ##  3              githapi  ChadGoymer       User
    ##  4               githug  ChadGoymer       User
    ##  5               inputs  ChadGoymer       User
    ##  6                 logr  ChadGoymer       User
    ##  7                model  ChadGoymer       User
    ##  8              outputs  ChadGoymer       User
    ##  9            r-project  ChadGoymer       User
    ## 10    r-python-workshop  ChadGoymer       User
    ## 11           r-training  ChadGoymer       User
    ## 12               rfiles  ChadGoymer       User
    ## 13       shiny-workshop  ChadGoymer       User
    ## # ... with 8 more variables: description <chr>, html_url <chr>, url <chr>,
    ## #   created_at <dttm>, updated_at <dttm>, size <int>, open_issues <int>,
    ## #   default_branch <chr>

Information on the branches in a particular repository can then be
obtained using `gh_branches()`:

    gh_branches("ChadGoymer/githapi")

    ## # A tibble: 3 x 3
    ##            name                               commit_sha
    ##           <chr>                                    <chr>
    ## 1 0-test-branch e93edf23e95b59d08853e2515ce9296c77d09712
    ## 2       develop dcf71d189010f4ad7ef9630200cf432c61ffa04b
    ## 3        master dcf71d189010f4ad7ef9630200cf432c61ffa04b
    ## # ... with 1 more variables: commit_url <chr>

The history of a branch can be obtained using `gh_commits()`:

    gh_commits("master", "ChadGoymer/githapi") %>% head()

    ## # A tibble: 6 x 10
    ##                                        sha                date
    ##                                      <chr>              <dttm>
    ## 1 dcf71d189010f4ad7ef9630200cf432c61ffa04b 2017-10-17 07:41:53
    ## 2 129bb138feeaef8707a293c3da679ef88e142443 2017-10-17 07:36:29
    ## 3 467d3edf21dd83d333081aab2d84fbf1424c57cb 2017-10-17 07:34:54
    ## 4 7f6e7287e25a42d0d65d8275e1362b2b0ab8c5bc 2017-10-17 07:34:19
    ## 5 01aea2754a11e8f28bf391e95c1bacf8cf93c840 2017-10-12 17:23:49
    ## 6 8efe2a88da3eb015254110b122235263e2ac5e53 2017-10-12 17:20:05
    ## # ... with 8 more variables: message <chr>, url <chr>, author_name <chr>,
    ## #   author_email <chr>, committer_name <chr>, committer_email <chr>,
    ## #   tree_sha <chr>, tree_url <chr>

### Getting the contents of files

There are a few ways of getting the content of a text file in GitHub.
For files of less than 1 MB, you can use `gh_contents()`:

    gh_contents("DESCRIPTION", "master", "ChadGoymer/githapi") %>% cat()

    ## Package: githapi
    ## Title: User-friendly access to the GitHub API for R, consistent with the tidyverse.
    ## Version: 0.5.0
    ## Authors@R: person("Chad", "Goymer", email = "chad.goymer@lloyds.com", role = c("aut", "cre"))
    ## Description: Provides a suite of functions which simplify working with GitHub's API.
    ## Imports:
    ##     assertthat,
    ##     httr,
    ##     jsonlite,
    ##     stringr,
    ##     tibble,
    ##     readr,
    ##     purrr,
    ##     rlang,
    ##     dplyr
    ## License: None
    ## Encoding: UTF-8
    ## LazyData: true
    ## Suggests: testthat
    ## RoxygenNote: 6.0.1

The README file can be downloaded using `gh_readme()`:

    gh_readme("master", "ChadGoymer/githapi")

For larger files, up to 100 MB, you can use the `gh_git_blob()`
function, or the move convenient `gh_save()`, which downloads the file
as a binary blob and writes it to disk:

    gh_save("DESCRIPTION", "ChadGoymer/githapi", "develop", path = "/tmp")

If you wish to read an R script from GitHub you can use the
`gh_source()` which downloads and `source`'s the script.

    gh_source("inst/test-data/test-source.R", "ChadGoymer/githapi", "develop")

Finally, you can download the entire contents of a commit using the
`gh_download()` function:

    gh_download("master", "ChadGoymer/githapi", "/tmp")

Futher information
------------------

Please see the help files for each function in the package for more
details.
