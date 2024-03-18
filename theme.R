#### ggplot theme and colours shared between figures ####

# ggplot theme for figures
theme_mobility <- theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )),
    plot.caption = element_text(hjust = 0, size = 10)
  )