
devtools::check_rhub(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))
devtools::check_win_devel()

pkgdown::build_site()
devtools::release()

