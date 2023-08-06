
library(hashprng)
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

# strip out most of the background
load(.args[1])

spag_plot <- reduced_dt[
  sample <= 250 & 
  !model %in% "ID" &
  time <= 40 &
  !type %in% "SMO"
][,
  type := as.factor(type)
] |> 
  setorder(type) |>
  ggplot() +
  aes(time, averted, color = type, group = interaction(type, sample)) +
  geom_line(alpha = 0.4) +
  scale_colour_manual(
    values = c("HBM" = "#2c3d70", "NON" = "#91acfc")
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none", panel.background = element_blank()
  )

# make and save hexsticker
sticker(
  spag_plot,
  package = "hashprng",
  p_size = 23,
  p_color = "#2c3d70",
  s_x = 1,
  s_y = 0.85,
  s_width = 1.8,
  s_height = 1.2,
  h_fill = "#ffffff",
  h_color = "#2c3d70",
  filename = tail(.args, 1),
  url = "hashprng.epinowcast.org",
  u_color = "#2c3d70",
  u_size = 3.5
)
