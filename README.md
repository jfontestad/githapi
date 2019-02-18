githapi
=======

[![Build Status](https://travis-ci.org/ChadGoymer/githapi.svg?branch=master)](https://travis-ci.org/ChadGoymer/githapi)

User-friendly access to the GitHub API for R, consistent with the tidyverse.

Overview
--------

GitHub is a wildly popular host for version controlled code, but also data. This package 
provides a user-friendly way to access content within GitHub through v3 of its 
[API](https://developer.github.com/v3/). The functions are also consistent with the
[tidyverse](http://www.tidyverse.org/) approach and return tibbles where possible.

Detailed documentation can be found at: http://www.goymer.me.uk/githapi

Installation
------------

The easiest way to install githapi is with the devtools package:

```r
library(devtools)
install_github("ChadGoymer/githapi")
```

Authorisation
-------------

In order to access repositories in GitHub authentication is required, by setting a 
[Personal Access Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/).
githapi identifies the personal token in two possible ways:

1.  On package load - if set as an environment variable named either "GITHUB_TOKEN" or "GITHUB_PAT"
2.  At any time - if set as an R option called "github.token"

Setting an R option will override the environment variable.

Usage
-----

`githapi` works best if used as part of the [tidyverse](http://www.tidyverse.org/).

```r
library(tidyverse)
library(githapi)
```

### Getting information about a repository

`githapi` provides many functions for extracting information from a repository. For example, To 
get information about the branches use 
[`view_branches()`](http://www.goymer.me.uk/githapi/reference/view_branches.html):

```r
view_branches("ChadGoymer/test-githapi")
```
```r
#> # A tibble: 4 x 6
#>   name     ref        url                 object_sha      object_type object_url               
#>   <chr>    <chr>      <chr>               <chr>           <chr>       <chr>                    
#> 1 master   refs/head~ https://api.github~ bd1be3ea52d69d~ commit      https://api.github.com/r~
#> 2 test-br~ refs/head~ https://api.github~ 244116707ad9e2~ commit      https://api.github.com/r~
#> 3 test-up~ refs/head~ https://api.github~ 61c19d6edb1b9e~ commit      https://api.github.com/r~
#> 4 unedite~ refs/head~ https://api.github~ 37d97bf9e93418~ commit      https://api.github.com/r~
```

Similarly, to get the history of commits for a particular branch use 
[`view_history()`](http://www.goymer.me.uk/githapi/reference/view_history.html):

```r
view_history("master", "ChadGoymer/test-githapi")
```
```r
#> # A tibble: 1,000 x 12
#>    sha   message author_name author_email committer_name committer_email date               
#>    <chr> <chr>   <chr>       <chr>        <chr>          <chr>           <dttm>             
#>  1 bd1b~ Testin~ Jane Jones  jane.jones@~ Bob Smith      bob.smith@acme~ 2019-02-18 07:46:08
#>  2 d12b~ Testin~ Jane Jones  jane.jones@~ Bob Smith      bob.smith@acme~ 2019-02-18 07:46:07
#>  3 9d4c~ Testin~ Bob Smith   bob.smith@a~ Jane Jones     jane.jones@acm~ 2019-02-18 07:46:06
#>  4 3cb8~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:59
#>  5 06f3~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:58
#>  6 aaec~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:58
#>  7 523c~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:57
#>  8 3a95~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:56
#>  9 a032~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:53
#> 10 9b77~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:52
#> # ... with 990 more rows, and 5 more variables: url <chr>, tree_sha <chr>, tree_url <chr>,
#> #   parent_sha <list>, parent_url <list>
```

Note that, by default, when viewing many items, only the last 1000 are returned. If you wish to
view more you can set the `n_max` parameter. To get information about the files in a particular 
commit use [`view_files()`](http://www.goymer.me.uk/githapi/reference/view_files.html):

```r
view_files(ref = "master", repo = "ChadGoymer/test-githapi")
```
```r
#> # A tibble: 4 x 9
#>   name   path   sha       size type  url          html_url     git_url        download_url     
#>   <chr>  <chr>  <chr>    <int> <chr> <chr>        <chr>        <chr>          <chr>            
#> 1 READM~ READM~ 75b08f1~    65 file  https://api~ https://git~ https://api.g~ https://raw.gith~
#> 2 test-~ test-~ bbac77b~     0 dir   https://api~ https://git~ https://api.g~ NA               
#> 3 test-~ test-~ f933c54~    22 file  https://api~ https://git~ https://api.g~ https://raw.gith~
#> 4 test-~ test-~ 68f9ad1~    63 file  https://api~ https://git~ https://api.g~ https://raw.gith~
```

Note that we have used a branch name here rather than a commit. That is because most functions
in `githapi` allow git references to identify commits. A git reference is either a branch, in 
which case the head commit on the branch is used, a tag or a SHA-1 of the commit.

Information about tags and releases can also be viewed by using 
[`view_tags()`](http://www.goymer.me.uk/githapi/reference/view_files.html) and 
[`view_releases()`](http://www.goymer.me.uk/githapi/reference/view_releases.html).

```r
view_tags("ChadGoymer/test-githapi")
```
```r
#> # A tibble: 1 x 6
#>   name  ref      url                  object_sha       object_type object_url                  
#>   <chr> <chr>    <chr>                <chr>            <chr>       <chr>                       
#> 1 0.0.0 refs/ta~ https://api.github.~ cbd94cf24a4c627~ commit      https://api.github.com/repo~
```

```r
view_releases("ChadGoymer/test-githapi")
```
```r
#> # A tibble: 1 x 13
#>       id tag_name name  body  author_login draft prerelease target_commitish
#>    <int> <chr>    <chr> <chr> <chr>        <lgl> <lgl>      <chr>           
#> 1 1.37e7 0.0.0    Init~ This~ ChadGoymer   FALSE FALSE      cbd94cf24a4c627~
#> # ... with 5 more variables: created_at <dttm>, published_at <dttm>, assets <list>,
#> #   zipball_url <chr>, url <chr>
```

### Downloading files from a repository

In addition to getting information about the contents of a repository, `githapi` can also 
download files and commits. To download individual files use
[`download_files()`](http://www.goymer.me.uk/githapi/reference/download_files.html)

```r
temp_path <- file.path(tempdir(), "githapi")

download_files(
  paths    = c("README.md", "test-file.txt"),
  location = temp_path,
  repo     = "ChadGoymer/test-githapi")
```

To download an entire commit use
[`download_commit()`](http://www.goymer.me.uk/githapi/reference/download_commit.html)

```r
download_commit(
  ref  = "master",
  path = temp_path,
  repo = "ChadGoymer/test-githapi")
```

You can also read text files from a commit directly using
[`read_files()`](http://www.goymer.me.uk/githapi/reference/read_files.html)

```r
files <- read_files(c("README.md", "test-file.txt"), "ChadGoymer/test-githapi")
```

and source R scripts directly from GitHub using
[`source_files()`](http://www.goymer.me.uk/githapi/reference/source_files.html)

```r
source_files("test-source.R", "ChadGoymer/test-githapi")
```

### Updating a repository

`githapi` also provides a set of functions for updating repsitories, for adding branches, tags as
well as new commits.

To create or update a file, or small number of text files, in a repository use
[`create_files()`](http://www.goymer.me.uk/githapi/reference/create_files.html) or
[`update_files()`](http://www.goymer.me.uk/githapi/reference/update_files.html)

```r
create_files(
  paths    = c("aaaa.txt", "bbbb.txt"),
  contents = c("Created to test:\n\n  `create_files()`", "Created to test:\n\n  `create_files()`"),
  messages = "Testing create_files()",
  repo     = "ChadGoymer/test-githapi")
```

```r
update_files(
  paths    = c("aaaa.txt", "bbbb.txt"),
  contents = c("Updated to test:\n\n  `update_files()`", "Updated to test:\n\n  `update_files()`"),
  messages = "Testing update_files()",
  repo     = "ChadGoymer/test-githapi")
```

These functions create a new commit per file and specify the contents as strings. To upload many
files, and/or binary files, use 
[`upload_commit()`](http://www.goymer.me.uk/githapi/reference/upload_commit.html)

```r
upload_commit(
  branch  = "master",
  message = "Commit made with upload_commit",
  path    = system.file("test-data/upload-tree", package = "githapi"),
  parents = "master",
  repo    = "ChadGoymer/test-githapi")
```

This function, by default, makes a new commit containing the contents of the specified directory.
If you wish to only specify the files to change then set the `replace` parameter to `FALSE` 
(see documentation for details).

Finally, tags and releases can be created using 
[`create_tags()`](http://www.goymer.me.uk/githapi/reference/create_tags.html) and
[`create_releases()`](http://www.goymer.me.uk/githapi/reference/create_releases.html) respectively.

```r
create_tags(
  tags = c("aaa", "bbb"),
  shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
  repo = "ChadGoymer/test-githapi")
```

```r
create_releases(
  tags    = c("aaa", "bbb"),
  commits = c("310c21d3f1601a46e014e68e94814b23406bf574", "32d3c5c4f6aba7ae9679480407e1b9f94ad04843"),
  names   = c("AAA", "BBB"),
  bodies  = c("Created for testing: aaa", "Created for testing: bbb"),
  repo    = "ChadGoymer/test-githapi")
```
