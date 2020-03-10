
pkgdown::build_site()

devtools::check_rhub(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))
devtools::check_win_devel()
devtools::release()

