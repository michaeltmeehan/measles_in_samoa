
## -----------------------------------------------------------------------------
# plots.R
# Utility plotting helpers for the measles_in_samoa project
#
# - Provides a predefined ggplot2 theme (custom_theme) used across figures
# - Keep this file lightweight and dependency-free beyond ggplot2 themes
#
# Author: project contributors
# -----------------------------------------------------------------------------

#' Predefined ggplot2 theme for project figures
#'
#' A compact, project-wide theme built on theme_classic with modest axis and
#' strip text sizes, no legend background or keys, and a faint horizontal grid
#' line to aid reading y-values.
#'
#' @return A ggplot2 theme object usable with +, e.g. p + custom_theme
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + custom_theme
custom_theme =   theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.key = element_blank(),
    legend.background = element_blank(),
    # legend.position = "none",
    strip.background = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3)
  )
