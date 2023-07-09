# Copyright (C) 2023-2023 Koen Derks

extract_group <- function(tile) {
  as.data.frame(rlist::list.match(tile, "ptNum|x|y|area"))
}

generate_mandala <- function(colors = FALSE, labels = FALSE) {
  x <- y <- ptNum <- area <- bp <- NULL
  pts <- sample(8:11, size = 1)
  rad <- runif(1, 1.1, 2.5)
  an <- seq(0, 2 * pi * (1 - 1 / pts), length = pts) + pi / 2
  frame <- data.frame(x = 0, y = 0)
  for (i in seq_len(3)) {
    tmp <- data.frame()
    for (j in seq_len(nrow(frame))) {
      npts <- data.frame(
        x = frame[j, "x"] + rad^(i - 1) * cos(an),
        y = frame[j, "y"] + rad^(i - 1) * sin(an)
      )
      tmp <- rbind(tmp, npts)
    }
    frame <- tmp
  }
  mandala <- deldir::tile.list(deldir::deldir(frame, sort = TRUE))
  mandala <- rlist::list.filter(mandala, sum(bp) == 0)
  mandala <- rlist::list.filter(mandala, length(intersect(which(x == 0), which(y == 0))) == 0)
  mandala <- rlist::list.rbind(lapply(mandala, extract_group))
  means_x <- aggregate(mandala$x, list(mandala$ptNum), FUN = mean)[, 2]
  means_y <- aggregate(mandala$y, list(mandala$ptNum), FUN = mean)[, 2]
  p <- ggplot2::ggplot(mandala, ggplot2::aes(x = x, y = y, group = ptNum)) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(fill = area, color = area, group = ptNum), show.legend = FALSE, linewidth = 0.05) +
    ggplot2::scale_color_gradientn(colors = "#000000") +
    ggplot2::scale_fill_gradientn(colors = sample(c("#ffffff", paste0("#", as.character(unlist(colourlovers::clpalettes("random")$colors)))))) +
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
  cols <- aggregate(ggplot2::ggplot_build(p)$data[[1]]$fill, list(ggplot2::ggplot_build(p)$data[[1]]$group), unique)[, 2]
  polygonColorIndex <- as.character(as.numeric(factor(cols, levels = unique(cols))))
  polygonColorIndex[which(cols == "#ffffff")] <- ""
  if (!colors) {
    suppressMessages({
      p <- p + ggplot2::scale_fill_gradientn(colors = "#ffffff")
    })
  }
  if (labels) {
    p <- p + ggplot2::annotate(geom = "text", x = means_x, y = means_y, label = polygonColorIndex, size = 1)
  }
  return(p)
}
