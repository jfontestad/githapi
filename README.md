githapi
=======

[![Build
Status](https://travis-ci.org/ChadGoymer/githapi.svg?branch=master)](https://travis-ci.org/ChadGoymer/githapi)

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

    ## [18:12:03] > GET: https://api.github.com/users/ChadGoymer/repos?per_page=100

    ## [18:12:03] > Parsing content

    ## [18:12:03] > Done

    ## # A tibble: 9 x 11
    ##   name  description owner_login owner_type default_branch open_issues  size
    ##   <chr> <chr>       <chr>       <chr>      <chr>                <int> <int>
    ## 1 chad~ My persona~ ChadGoymer  User       master                   0   156
    ## 2 git-~ Simple gui~ ChadGoymer  User       master                   0     2
    ## 3 gith~ User-frien~ ChadGoymer  User       master                   8   724
    ## 4 logr  <NA>        ChadGoymer  User       master                   0     0
    ## 5 r-be~ A set of g~ ChadGoymer  User       master                   0 11306
    ## 6 r-py~ <NA>        ChadGoymer  User       master                   0  1172
    ## 7 r-tr~ Training m~ ChadGoymer  User       master                   0  1229
    ## 8 rfil~ An R packa~ ChadGoymer  User       master                   0     2
    ## 9 shin~ <NA>        ChadGoymer  User       master                   0 13408
    ## # ... with 4 more variables: url <chr>, html_url <chr>, created_at <dttm>,
    ## #   updated_at <dttm>

Information on the branches in a particular repository can then be
obtained using `gh_branches()`:

    gh_branches("ChadGoymer/githapi")

    ## [18:12:04] > GET: https://api.github.com/repos/ChadGoymer/githapi/branches?per_page=100

    ## [18:12:05] > Parsing content

    ## [18:12:05] > Done

    ## # A tibble: 4 x 3
    ##   name        commit_sha             commit_url                           
    ##   <chr>       <chr>                  <chr>                                
    ## 1 0-test-bra~ e93edf23e95b59d08853e~ https://api.github.com/repos/ChadGoy~
    ## 2 65-hook-up~ 5db96593332e004c30122~ https://api.github.com/repos/ChadGoy~
    ## 3 develop     27097e72c1cba202c2390~ https://api.github.com/repos/ChadGoy~
    ## 4 master      dc7da99ef557f362d5f2c~ https://api.github.com/repos/ChadGoy~

The history of a branch can be obtained using `gh_commits()`:

    gh_commits("master", "ChadGoymer/githapi") %>% head()

    ## [18:12:05] > GET: https://api.github.com/repos/ChadGoymer/githapi/commits?sha=master&per_page=100

    ## [18:12:14] > Parsing content

    ## [18:12:14] > Done

    ## [18:12:14] > GET: https://api.github.com/repositories/93376315/commits?sha=master&per_page=100&page=2

    ## [18:12:15] > Parsing content

    ## [18:12:15] > Done

    ## # A tibble: 6 x 10
    ##   sha   message author_name author_email committer_name committer_email
    ##   <chr> <chr>   <chr>       <chr>        <chr>          <chr>          
    ## 1 dc7d~ "Merge~ Chad Goymer chad.goymer~ GitHub         noreply@github~
    ## 2 9b31~ Merge ~ Chad Goymer chad.goymer~ GitHub         noreply@github~
    ## 3 abcc~ update~ Chad        chad.goymer~ Chad           chad.goymer@gm~
    ## 4 2709~ "Merge~ Chad Goymer chad.goymer~ GitHub         noreply@github~
    ## 5 0262~ update~ Chad        chad.goymer~ Chad           chad.goymer@gm~
    ## 6 00fd~ "Merge~ Chad Goymer chad.goymer~ GitHub         noreply@github~
    ## # ... with 4 more variables: date <dttm>, url <chr>, tree_sha <chr>,
    ## #   tree_url <chr>

### Getting the contents of files

There are a few ways of getting the content of a text file in GitHub.
For files of less than 1 MB, you can use `gh_contents()`:

    gh_contents("DESCRIPTION", "master", "ChadGoymer/githapi") %>% cat()

    ## [18:12:15] > GET: https://api.github.com/repos/ChadGoymer/githapi/contents/DESCRIPTION?ref=master

    ## [18:12:15] > Parsing content

    ## [18:12:15] > Done

    ## Package: githapi
    ## Title: User-friendly access to the GitHub API for R, consistent with the tidyverse.
    ## Version: 0.6.1
    ## Authors@R: person("Chad", "Goymer", email = "chad.goymer@lloyds.com", role = c("aut", "cre"))
    ## Description: Provides a suite of functions which simplify working with GitHub's API.
    ## Imports:
    ##     curl,
    ##     jsonlite,
    ##     dplyr
    ## Roxygen: list(markdown = TRUE)
    ## License: MIT + file LICENSE
    ## Encoding: UTF-8
    ## LazyData: true
    ## Suggests: testthat
    ## RoxygenNote: 6.1.0
    ## URL: https://github.com/ChadGoymer/githapi
    ## BugReports: https://github.com/ChadGoymer/githapi/issues

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
