
library(usethis)
use_package_doc()

library(devtools)

load_all()

document()
check_man()

check()


# usethis::use_pkgdown()
pkgdown::build_site()


# git remote -v
# git remote add gitlab git@gitlab.com:gibonet/df2px.git

# Push to github and gitlab -----------
# git push -u origin master
# git push -u gitlab master

