# Copyright (C) 2023-2023 Koen Derks

generate_instruction <- function(title, label) {
  p <- ggplot2::ggplot() +
    ggplot2::annotate(geom = "text", x = 0, y = 0.75, label = title, hjust = 0, size = 15, family = "Dancing Script", fontface = "bold", vjust = 0) +
    ggplot2::annotate(geom = "text", x = 0, y = 0.65, label = label, hjust = 0, size = 7.5, vjust = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 5)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    )
  return(p)
}
