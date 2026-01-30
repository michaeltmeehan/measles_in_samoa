required_packages <- c(
  "tidyverse",
  "ggsci",
  "ggplot2",
  "readr",
  "ggh4x",
  "scales"
)

to_install <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, library, character.only = TRUE))
