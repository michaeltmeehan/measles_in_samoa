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
