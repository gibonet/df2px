
library(usethis)
use_package_doc()

library(devtools)

load_all()

document()
check_man()

check()


# usethis::use_pkgdown()
pkgdown::build_site()
