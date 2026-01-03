############################################################
# setup.R
# Script de bootstrap do projeto (uso único / desenvolvimento)
# NÃO faz parte da pipeline nem é usado pelo app.
############################################################

usethis::use_git()
usethis::use_readme_md(open = FALSE)
usethis::use_directory("R")
usethis::use_directory("data/raw")
usethis::use_directory("data/processed")
usethis::use_directory("logs")
usethis::use_git_ignore(c("data/raw/", "data/processed/", "logs/"))
renv::init(bare = TRUE)
