# Copyright (C) 2023-2023 Koen Derks

extract_group <- function(tile) {
  as.data.frame(rlist::list.match(tile, "ptNum|x|y|area"))
}

generate_mandala <- function(colors = FALSE) {
  x <- y <- ptNum <- area <- bp <- NULL
  points <- sample(10:25, size = 1)
  radius <- runif(1, 1.1, 2.5)
  angles <- seq(0, 2 * pi * (1 - 1 / points), length.out = points) + pi / 2
  df <- data.frame(x = 0, y = 0)
  for (k in seq_len(3)) {
    temp <- data.frame()
    for (i in seq_len(nrow(df))) {
      new_points <- data.frame(
        x = df[i, "x"] + radius^(k - 1) * cos(angles),
        y = df[i, "y"] + radius^(k - 1) * sin(angles)
      )
      temp <- rbind(temp, new_points)
    }
    df <- temp
  }
  df_polygon <- deldir::tile.list(deldir::deldir(df, sort = TRUE))
  df_polygon <- rlist::list.filter(df_polygon, sum(bp) == 0)
  df_polygon <- rlist::list.filter(df_polygon, length(intersect(which(x == 0), which(y == 0))) == 0)
  df_polygon <- rlist::list.rbind(lapply(df_polygon, extract_group))
  if (!colors) {
    df_polygon <- do.call(rbind, lapply(unique(df_polygon$ptNum), function(pt) {
      rows <- df_polygon$ptNum == pt
      rbind(df_polygon[rows, ], df_polygon[rows[1], ])
    }))
  }
  p <- ggplot2::ggplot(df_polygon, ggplot2::aes(x = x, y = y, group = ptNum)) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(fill = area, color = area, group = ptNum), show.legend = FALSE, linewidth = 0.05) +
    ggplot2::scale_color_gradientn(colors = "#000000") +
    ggplot2::scale_x_continuous(limits = range(df_polygon$x)) +
    ggplot2::scale_y_continuous(limits = range(df_polygon$y)) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(rep(0, 4), "lines"),
    )
  if (colors) {
    p <- p + ggplot2::scale_fill_gradientn(colors = sample(c("#ffffff", paste0("#", as.character(unlist(colourlovers::clpalettes("random")$colors))))))
  } else {
    p <- p + ggplot2::scale_fill_gradientn(colors = "#ffffff")
  }
  return(p)
}
