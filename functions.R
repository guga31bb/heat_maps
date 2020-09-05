library(tidyverse)
library(ggExtra)
library(patchwork)
library(paletteer)
library(scales)

not_div_5 <- function(x) {
  # select only elements of the vector not divisible by 5
  x[x %% 5 != 0]
}

max <- 40


back_col <- "white"
front_col <- "black"

heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(10)

center_df <- tibble(
  x_coord = c(rep(-3.1, 40), rep(3.1, 40)),
  y_coord = seq(-9, max-1, 1) %>% rep(2) %>% not_div_5(),
  text = "--"
)

# line labels
annotate_df <- tibble(
  x_coord = c(12.88, -12.88) %>% rep(each = 3),
  y_coord = seq(10, 30, 10) %>% rep(2),
  text = seq(10, 30, 10) %>% rep(2) %>% str_replace("(.)(.)", "\\1 \\2"),
  rotation = c(90, 270) %>% rep(each = 3)
)

# yardlines
yardline_df <- tibble(
  y = seq(-10, max, 5),
  yend = seq(-10, max, 5),
  x = rep(-56 / 2, 11),
  xend = rep(56 / 2, 11)
)

# sidelines
sideline_df <- tibble(
  y = c(-10.15, -10.15),
  yend = c(max + .15, max + .15),
  x = c(-56 / 2, 56 / 2),
  xend = c(-56 / 2, 56 / 2)
)

add_field <- function() {
  list(
    coord_cartesian(
      xlim = c(-53.333 / 2, 53.333 / 2),
      ylim = c(-10, max)
    ),
    geom_text(
      data = annotate_df, aes(label = text, angle = rotation),
      color = front_col, size = 8
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 1,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
      color = "blue", size = 1, alpha = 0.5
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}

maps <- function(df, input) {
  
  pass_map <- df %>%
    ggplot(aes(x = x_coord, y = y_coord)) +
    geom_density_2d_filled(
      aes(fill = ..level.., color = ..level..),
      contour_var = "ndensity", # normalize across facets
      breaks = seq(0.1, 1.0, length.out = 10)
    ) +
    facet_wrap(
      ~name
      )
  
  pass_map +
    add_field() +
    scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color"))
  
  
  # ggsave('C:/Users/bback/Dropbox/nfl/current_season/results/999_carr_wilson.png', dpi=800)
  
}




