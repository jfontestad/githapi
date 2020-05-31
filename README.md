githapi
=======

<!-- badges: start -->
[![R build status](https://github.com/ChadGoymer/githapi/workflows/R-CMD-check/badge.svg)](https://github.com/ChadGoymer/githapi/actions) [![Coverage status](https://codecov.io/gh/ChadGoymer/githapi/branch/master/graph/badge.svg)](https://codecov.io/github/ChadGoymer/githapi?branch=master)
<!-- badges: end -->

User-friendly access to the GitHub API for R, consistent with the tidyverse.

Overview
--------

GitHub is a popular host for version controlled code, but also data. This package 
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

In order to access GitHub authentication is required. The first time you use githapi in an R
session you will be redirected to the GitHub login page. Enter your details and allow githapi
the access required. If you want to avoid authorising each time you can set a token cache
using an environment variable. The authentication token is then saved at the desired location. 
The easiest way to do that is create (or update) a file called `.Renviron` in you HOME 
directory and enter the following:

```
GITHAPI_CACHE=~/.githapi.oauth
```

Usage
-----

`githapi` works best if used as part of the [tidyverse](http://www.tidyverse.org/).

```r
library(tidyverse)
library(githapi)
```

### Viewing repositories

`githapi` provides many functions for extracting information about repositories. For example, 
To first get a summary of the repositories you can use
[`view_repositories()`](http://www.goymer.me.uk/githapi/reference/view_repositories.html):

```r
view_repositories("ChadGoymer")
```
```r
#> # GET https://api.github.com/users/ChadGoymer/repos?type=all&sort=created&direction=desc&per_page=100
#> # A tibble: 10 x 26
#>        id name  full_name description owner homepage language  size default_branch permission
#>     <int> <chr> <chr>     <chr>       <chr> <chr>    <chr>    <dbl> <chr>          <chr>     
#>  1 2.62e8 user~ ChadGoym~ This is a ~ Chad~ https:/~ NA           0 master         admin     
#>  2 2.24e8 .git~ ChadGoym~ Issue and ~ Chad~ NA       NA           1 master         admin     
#>  3 1.68e8 msgr  ChadGoym~ An R packa~ Chad~ NA       R          218 master         admin     
#>  4 1.14e8 r-be~ ChadGoym~ A set of g~ Chad~ NA       CSS      41205 master         admin     
#>  5 9.69e7 git-~ ChadGoym~ Simple gui~ Chad~ NA       NA           2 master         admin     
#>  6 9.34e7 gith~ ChadGoym~ User-frien~ Chad~ http://~ R         6124 master         admin     
#>  7 8.08e7 r-tr~ ChadGoym~ Training m~ Chad~ NA       R         1229 master         admin     
#>  8 4.24e7 r-py~ ChadGoym~ NA          Chad~ NA       HTML      1172 master         admin     
#>  9 4.24e7 shin~ ChadGoym~ NA          Chad~ NA       HTML     13408 master         admin     
#> 10 3.93e7 chad~ ChadGoym~ My persona~ Chad~ NA       HTML       156 master         admin     
#> # ... with 16 more variables: private <lgl>, has_issues <lgl>, has_projects <lgl>, has_wiki <lgl>,
#> #   has_pages <lgl>, has_downloads <lgl>, allow_squash_merge <lgl>, allow_merge_commit <lgl>,
#> #   allow_rebase_merge <lgl>, fork <lgl>, archived <lgl>, disabled <lgl>, html_url <chr>,
#> #   pushed_at <dttm>, created_at <dttm>, updated_at <dttm>
```

For a particular repository you can view a summary of the branches using 
[`view_branches()`](http://www.goymer.me.uk/githapi/reference/view_branches.html):

```r
view_branches("ChadGoymer/githapi")
```
```r
#> # GET https://api.github.com/repos/ChadGoymer/githapi/git/refs/heads?per_page=100
#> # A tibble: 3 x 3
#>   name                        ref                                  sha                              
#>   <chr>                       <chr>                                <chr>                            
#> 1 master                      refs/heads/master                    c5a5261c7bf21d3c402c7ddaf4f4454c~
#> 2 0-test-branch               refs/heads/0-test-branch             e93edf23e95b59d08853e2515ce9296c~
#> 3 177-remove-deprecated-func~ refs/heads/177-remove-deprecated-fu~ 9d0aaad0c6837388058971c92b8e6b2c~
```

Similarly, to get the history of commits for a particular branch use 
[`view_commits()`](http://www.goymer.me.uk/githapi/reference/view_commits.html):

```r
view_commits("master", "ChadGoymer/githapi")
```
```r
#> # GET https://api.github.com/repos/ChadGoymer/githapi/commits?sha=master&per_page=100
#> # GET https://api.github.com/repositories/93376315/commits?sha=master&per_page=100&page=2
#> # GET ...
#> # A tibble: 749 x 13
#>    sha   message author_login author_name author_email author_date         committer_login
#>    <chr> <chr>   <chr>        <chr>       <chr>        <dttm>              <chr>          
#>  1 c5a5~ "Merge~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 16:06:58 web-flow       
#>  2 f875~ "Regen~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 15:30:18 ChadGoymer     
#>  3 3df7~ "Updat~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 15:27:41 ChadGoymer     
#>  4 ab29~ "Corre~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 15:22:20 ChadGoymer     
#>  5 f973~ "Updat~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 15:12:08 ChadGoymer     
#>  6 c6f0~ "Repla~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 15:00:09 ChadGoymer     
#>  7 c008~ "Added~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-07 14:51:34 ChadGoymer     
#>  8 7fc1~ "Allow~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-06 18:11:37 ChadGoymer     
#>  9 eb93~ "Ensur~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-05 19:14:47 ChadGoymer     
#> 10 560b~ "Ensur~ ChadGoymer   Chad Goymer chad.goymer~ 2020-05-05 14:02:19 ChadGoymer     
#> # ... with 739 more rows, and 6 more variables: committer_name <chr>, committer_email <chr>,
#> #   committer_date <dttm>, tree_sha <chr>, parents <list>, html_url <chr>
```

Note that, by default, when viewing many items, only the last 1000 are returned. If you wish to
view more you can set the `n_max` parameter. To get information about the files in a particular 
commit use [`view_files()`](http://www.goymer.me.uk/githapi/reference/view_files.html):

```r
view_files(ref = "master", repo = "ChadGoymer/githapi")
```
```r
#> # GET https://api.github.com/repos/ChadGoymer/githapi/git/trees/16a48fab8de89c6483b21430a48fecf9cd364d43?recursive=TRUE
#> # A tibble: 583 x 4
#>    path                  sha                    size html_url                                       
#>  * <chr>                 <chr>                 <dbl> <chr>                                          
#>  1 .Rbuildignore         9a84e8afdeb40bb57685~   253 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  2 .covrignore           ec05f6ab9650fb158824~    12 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  3 .github/ISSUE_TEMPLA~ f5c658bb37d5dbbc045b~   451 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  4 .github/ISSUE_TEMPLA~ ec1f4be1460643e6b548~   479 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  5 .github/pull_request~ 7f6c4324235f6d424d10~   140 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  6 .gitignore            74b7fa994fa52cf7ecb5~   118 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  7 .travis.yml           d7548ded184873394110~   273 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  8 DESCRIPTION           9c2d45083c95864d3d6f~   726 https://github.com/ChadGoymer/githapi/blob/c5a~
#>  9 LICENSE               070a2ae0e71d920f58bd~    41 https://github.com/ChadGoymer/githapi/blob/c5a~
#> 10 LICENSE.md            6c2be93c834c1b8585c4~  1070 https://github.com/ChadGoymer/githapi/blob/c5a~
#> # ... with 573 more rows
```

Note that we have used a branch name here rather than a commit. That is because most functions
in `githapi` allow git references to identify commits. A git reference is either a branch, in 
which case the head commit on the branch is used, a tag or the SHA-1 of the commit.

Information about tags and releases can also be viewed by using 
[`view_tags()`](http://www.goymer.me.uk/githapi/reference/view_tags.html) and 
[`view_releases()`](http://www.goymer.me.uk/githapi/reference/view_releases.html).

```r
view_tags("ChadGoymer/githapi")
```
```r
#> # A tibble: 21 x 3
#>    name     ref                sha                                     
#>    <chr>    <chr>              <chr>                                   
#>  1 test-tag refs/tags/test-tag 30426b4f967d8c253b1bb5a67c5838dc306aab50
#>  2 v0.0.0   refs/tags/v0.0.0   ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964
#>  3 v0.1.0   refs/tags/v0.1.0   7ca61bb71f877f462c0b6132759d7c5e507c921f
#>  4 v0.2.0   refs/tags/v0.2.0   7ee36b18186f0c09389af0eb0e4a6843c2ac853c
#>  5 v0.3.0   refs/tags/v0.3.0   d378328243626794ca725946c4e0662622aeb933
#>  6 v0.4.0   refs/tags/v0.4.0   7a22d6dd5520f5238677483689b5f255944b6764
#>  7 v0.5.0   refs/tags/v0.5.0   dcf71d189010f4ad7ef9630200cf432c61ffa04b
#>  8 v0.5.1   refs/tags/v0.5.1   e45eb21203e2f77da8d8b81df2a1b109fae01159
#>  9 v0.5.2   refs/tags/v0.5.2   099944f501b2c2fba940f807b1028dbc5349f29c
#> 10 v0.6.0   refs/tags/v0.6.0   702c033f59081cd12b364d64763d22e12701c62f
#> # ... with 11 more rows
```

```r
view_releases("ChadGoymer/githapi")
```
```r
#> # GET https://api.github.com/repos/ChadGoymer/githapi/releases?per_page=100
#> # A tibble: 20 x 12
#>        id tag   name  body  commit draft prerelease author_login assets html_url created_at         
#>     <int> <chr> <chr> <chr> <chr>  <lgl> <lgl>      <chr>        <list> <chr>    <dttm>             
#>  1 1.67e7 0.8.7 Fixe~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-04-10 13:12:31
#>  2 1.64e7 0.8.6 Fixe~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-03-27 15:30:50
#>  3 1.59e7 0.8.5 Usin~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-03-05 18:47:00
#>  4 1.56e7 0.8.2 Depr~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-02-18 18:17:18
#>  5 1.55e7 0.7.3 Add ~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-12-06 07:33:25
#>  6 1.55e7 0.7.2 Crea~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-11-06 14:26:49
#>  7 1.37e7 0.7.1 The ~ "# R~ 31142~ FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-10-31 06:57:38
#>  8 1.37e7 0.7.0 Star~ "# R~ 59ef5~ FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-10-26 18:02:49
#>  9 1.35e7 test~ Test~ "Thi~ 9bb32~ TRUE  TRUE       ChadGoymer   <chr ~ https:/~ 2018-10-18 07:53:42
#> 10 1.26e7 0.6.3 Fixe~ "Res~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-08-29 09:03:28
#> 11 1.26e7 0.6.2 Hook~ "Add~ 47c25~ FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-08-28 19:39:10
#> 12 1.26e7 0.6.1 Buil~ "Con~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-08-24 08:55:38
#> 13 1.17e7 v0.6~ Simp~ "Thi~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-06-26 21:09:34
#> 14 8.40e6 v0.5~ Rele~ "Fix~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2017-11-06 10:09:04
#> 15 8.17e6 v0.5~ Fixe~ " - ~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2017-10-18 17:51:29
#> 16 8.14e6 v0.5~ Rele~ " - ~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2017-10-17 08:41:53
#> 17 7.72e6 v0.4~ Rele~ "add~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2017-09-12 08:21:08
#> 18 7.66e6 v0.3~ Rele~ "add~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2017-08-14 18:13:10
#> 19 7.25e6 v0.2~ Git ~ "Add~ master FALSE TRUE       ChadGoymer   <chr ~ https:/~ 2017-08-01 18:45:16
#> 20 7.21e6 v0.1~ sele~ ""    master FALSE TRUE       ChadGoymer   <chr ~ https:/~ 2017-07-28 18:04:10
#> # ... with 1 more variable: published_at <dttm>
```


### Downloading files from a repository

In addition to getting information about the contents of a repository, `githapi` can also 
download files and commits. To download individual files use
[`download_file()`](http://www.goymer.me.uk/githapi/reference/download_file.html)

```r
download_file(
  from_path = "README.md",
  to_path   = "~/githapi/README.md",
  ref       = "master",
  repo      = "ChadGoymer/githapi")
```

To download an entire commit use
[`download_commit()`](http://www.goymer.me.uk/githapi/reference/download_commit.html)

```r
download_commit(
  path = "~/githapi",
  ref  = "master",
  repo = "ChadGoymer/githapi")
```

You can also read text files from a commit directly using
[`read_github_file()`](http://www.goymer.me.uk/githapi/reference/read_github_file.html)

```r
read_github_file("README.md", ref = "master", repo = "ChadGoymer/githapi")
```

and source R scripts directly from GitHub using
[`github_source()`](http://www.goymer.me.uk/githapi/reference/github_source.html)

```r
github_source("inst/test-data/test-source.R", ref = "master", repo = "ChadGoymer/githapi")
```


### Updating a repository

`githapi` also provides a set of functions for updating repositories, for adding branches, tags as
well as new commits. To create or update a file use
[`create_file()`](http://www.goymer.me.uk/githapi/reference/create_file.html) or
[`update_file()`](http://www.goymer.me.uk/githapi/reference/update_file.html)

```r
create_file(
  content = "# This is a new file\\n\\n Created by `create_file()`",
  path    = "new-file.md",
  branch  = "master",
  message = "Created a new file with create_file()",
  repo    = "ChadGoymer/githapi")
```

```r
update_file(
  content = "# This is a file\\n\\n Updated by `update_file()`",
  path    = "new-file.md",
  branch  = "master",
  message = "Updated a file with update_file()",
  repo    = "ChadGoymer/githapi")
```

These functions create a new commit and specify the contents as strings. To upload many files, 
and/or binary files, use 
[`upload_commit()`](http://www.goymer.me.uk/githapi/reference/upload_commit.html)

```r
upload_commit(
    path    = "C:/files-to-upload",
    branch  = "master",
    message = "Commit to test upload_commit()",
    repo    = "ChadGoymer/githapi")
```

This function, by default, makes a new commit containing the contents of the specified directory 
(see documentation for details).

Finally, tags and releases can be created using 
[`create_tag()`](http://www.goymer.me.uk/githapi/reference/create_tag.html) and
[`create_release()`](http://www.goymer.me.uk/githapi/reference/create_release.html) respectively.

```r
create_tag(
  name = "new-tag",
  ref  = "master",
  repo = "ChadGoymer/githapi")
```

```r
create_release(
  tag  = "1.0.0",
  repo = "ChadGoymer/githapi",
  name = "Initial production release",
  body = "This is a release created by create_release()")
```
