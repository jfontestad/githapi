# githapi

User-friendly access to the GitHub API for R, consistent with the tidyverse.

## Overview

GitHub is a wildly popular host for version controlled code, but also data. This package 
provides a user-friendly way to access content within GitHub through v3 of its 
[API](https://developer.github.com/v3/). The functions are also consistent with the 
[tidyverse](http://www.tidyverse.org/) approach and return tibbles where possible.

## Installation

The easiest way to install githapi is with the devtools package:

```r
library(devtools)
install_github("ChadGoymer/githapi")
```

## Usage

Most of the functions in the package are prefixed by `gh_`, except for predicate functions which are 
prefixed with `is_`.

### Getting information about a user's repositories

The main reason for using githapi is to extract information from GitHub. For example to find all the 
repositories a user has use the function `gh_repositories()`:

```{r}
gh_repositories("ChadGoymer")
```
