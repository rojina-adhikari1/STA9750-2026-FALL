#| label: fig-sq3-event-animation
#| code-fold: true
#| message: false
#| warning: false
#| echo: false
#| eval: false

library(gganimate)
library(gifski)

# Use consistent wording in Figure 4
levels(PLOT_DATA2$outcome)[
  levels(PLOT_DATA2$outcome) == "Under-25"
] <- "Under 25"

# Build cumulative frames from 2015 to 2024
ANIM_DATA <- tidyr::expand_grid(
  frame_year = 2015:2024,
  PLOT_DATA2
) |>
  dplyr::filter(year <= frame_year)

# Prepare cumulative observations for each animation frame
ANIM_POINTS <- ANIM_DATA |>
  dplyr::mutate(
    period = if_else(year >= REC_YEAR, "post", "pre"),
    is_base = year == 2019
  )
# Show the Reconnect annotation from 2022 onward
ANIM_LABEL <- tibble::tibble(
  frame_year = 2022:2024,
  outcome = factor(
    "Adults (25+)",
    levels = levels(PLOT_DATA2$outcome)
  ),
  year = 2022,
  estimate = 27,
  label = "CUNY Reconnect\n(Fall 2022)"
)
# Create a separate animated version of Figure 4
# Build animated Figure 4 from cumulative data
p_event2_anim <- ggplot(
  ANIM_POINTS,
  aes(x = year, y = estimate)
) +
  annotate(
    "rect",
    xmin = 2021.5, xmax = 2024.5,
    ymin = -Inf, ymax = Inf,
    fill = SYS_BLUE, alpha = 0.07
  ) +
  geom_hline(yintercept = 0, color = "#64748B") +
  geom_vline(
    xintercept = 2021.5,
    linetype = "dashed",
    color = "grey35"
  ) +
  geom_text(
  data = ANIM_LABEL,
  aes(x = year, y = estimate, label = label),
  inherit.aes = FALSE,
  hjust = 0,
  size = 3.2,
  color = SYS_BLUE,
  fontface = "bold"
  ) +
  geom_line(
  data = ANIM_POINTS,
  aes(group = interaction(outcome, frame_year)),
  color = "#17324D",
  linewidth = 0.8
  ) +
  geom_linerange(
    aes(ymin = lo, ymax = hi, color = period),
    linewidth = 1.2,
    alpha = 0.45
  ) +
  geom_point(
    aes(color = period),
    size = 3.6
  ) +
  geom_point(
    data = dplyr::filter(ANIM_POINTS, is_base),
    shape = 21,
    fill = "white",
    color = "grey35",
    size = 3.6,
    stroke = 1.2
  ) +
  geom_text(
  data = dplyr::filter(ANIM_POINTS, period == "post"),
  aes(
    y = hi,
    label = sprintf("%+.0f%%", estimate),
    color = period
  ),
  vjust = -0.7,
  size = 3.3,
  fontface = "bold",
  show.legend = FALSE
) +
  scale_color_manual(
    values = c(pre = PRE_COL, post = SYS_BLUE),
    breaks = c("pre", "post"),
    labels = c("Before Reconnect", "After Reconnect"),
    name = NULL
  ) +
  facet_wrap(~ outcome) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(
  labels = scales::label_number(suffix = "%")
) +
coord_cartesian(ylim = c(-35, 32)) +
  labs(
  title = "Adult Enrollment Recovery Differs Between CUNY and SUNY",
    subtitle = "Community colleges: adults (25+) and under 25 enrollment relative to 2019 | Through {current_frame}",
  x = NULL,
  y = "CUNY–SUNY relative enrollment\ndifference (%)",
  caption = paste(
  "Burnt orange: before Reconnect | Teal: after Reconnect.",
  "Vertical bars: 95% confidence intervals; navy lines connect estimates.",
  "Hollow circles: 2019 baseline. No estimates for 2016 or 2018.",
  sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(1.2, "lines"),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    face = "bold",
    hjust = 0,
    size = 16,
    color = "#17324D",
    margin = margin(6, 6, 6, 6)
    ),
    plot.title.position = "plot",
    plot.title = element_text(
    face = "bold",
    size = 24
    ),
    plot.subtitle = element_text(
    size = 15,
    color = "#475569"
    ),
  plot.caption = element_text(
  hjust = 0,
  size = 10,
  color = "#475569",
  margin = margin(t = 30)
),
plot.caption.position = "plot",
plot.margin = margin(15, 20, 25, 35)
  ) +
  gganimate::transition_manual(
  factor(frame_year, levels = 2015:2024),
  cumulative = FALSE
  )
# Generate and save the updated Figure 4 animation
dir.create("figures", showWarnings = FALSE)

fig4_gif <- gganimate::animate(
  p_event2_anim,
  nframes = 40,
  fps = 5,
  end_pause = 15,
  width = 1000,
  height = 500,
  renderer = gganimate::gifski_renderer()
)

gganimate::anim_save(
  "figures/sq3_figure4_final_corrected.gif",
  animation = fig4_gif
)
