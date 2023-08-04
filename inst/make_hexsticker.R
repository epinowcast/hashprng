
library(data.table)
library(hexSticker)
library(sysfonts)
library(ggplot2)

.args <- if (interactive()) {
  c(
    file.path("inst", "basic_vignette.rda"),
    file.path("man", "figures", "logo.png")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

# font setup
font_add_google("Zilla Slab Highlight", "useme")

# load some example interesting stochastic model data
load(.args[1])

slice_dt <- samples_dt[,
  tm := seq_len(.N) - 1L, by = .(sample, type)
][ type == "HBM" ]

max_split <- slice_dt[, sample[which.max(not_i - withi)]]

plot_dt <- slice_dt |> melt.data.table(
  id.vars = c("sample", "type", "tm"),
  measure.vars = c("not_i", "withi")
)

# strip out most of the background
hex_plot <- plot_dt[
  between(tm, 10, 40) & sample == max_split
] |>
  ggplot() +
  aes(x = tm, y = value, fill = variable) +
  geom_hex(alpha = 0.5) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none",
        panel.background = element_blank())

# make and save hexsticker
sticker(
  hex_plot,
  package = "hbmPRNG",
  p_size = 23,
  p_color = "#646770",
  s_x = 0.95,
  s_y = 1,
  s_width = 1.35,
  s_height = 1.0,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = tail(.args, 1),
  url = "",
  u_color = "#646770",
  u_size = 3.5
)
