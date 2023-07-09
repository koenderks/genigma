# Copyright (C) 2023-2023 Koen Derks

generate_sudoku <- function(seed, solution = FALSE) {
  blank <- sample(20:70, size = 1)
  level <- floor((blank - 10) / 10)
  if (solution) {
    blank <- 0
  }
  z <- y <- z <- NULL
  z <- c(1:9, 4:9, 1:3, 7:9, 1:6, 2:9, 1, 5:9, 1:4, 8:9, 1:7, 3:9, 1:2, 6:9, 1:5, 9, 1:8)
  z <- matrix(sample(9)[z], 9, 9)
  for (i in 1:5) {
    z <- z[replicate(3, sample(3)) + 3 * rep(sample(0:2), each = 3), replicate(3, sample(3)) + 3 * rep(sample(0:2), each = 3)]
  }
  for (bi in seq(0, 6, 3)) {
    for (bj in seq(0, 6, 3)) {
      idx <- data.matrix(expand.grid(bi + 1:3, bj + 1:3))
      z[idx[sample(1:9, blank %/% 9), ]] <- 0
    }
  }
  while (sum(!z) < blank) {
    z[matrix(sample(9, 2), 1)] <- 0
  }
  z <- as.character(c(z))
  z[z == "0"] <- ""
  canvas <- data.frame(
    x = rep(seq_len(9), times = 9),
    y = rep(seq_len(9), each = 9), z = z
  )
  p <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_rect(xmin = canvas$x - 0.5, xmax = canvas$x + 0.5, ymin = canvas$y - 0.5, ymax = canvas$y + 0.5, fill = "#ffffff", col = "black", linewidth = 0.25) +
    ggplot2::annotate(geom = "rect", xmin = rep(c(0.5, 3.5, 6.5), times = 3), xmax = rep(c(3.5, 6.5, 9.5), times = 3), ymin = rep(c(0.5, 3.5, 6.5), each = 3), ymax = rep(c(3.5, 6.5, 9.5), each = 3), fill = NA, col = "black", linewidth = 0.75) +
    ggplot2::scale_x_continuous(limits = c(0.5, 9.5)) +
    ggplot2::scale_y_continuous(limits = c(0.5, 9.5)) +
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
      plot.margin = ggplot2::unit(c(1, 0, 0, 0), "cm"),
      plot.title = ggplot2::element_text(size = 75, face = "bold", hjust = 0.5, family = getOption("book.font.type", "sans"))
    )
  if (!solution) {
    p <- p + ggplot2::ggtitle(paste0("— Sudoku ", names(seed), " ~ Level ", level, " —")) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(1, 0, 21 - 13.5 - 1, 0), "cm")) +
      ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 15)
  } else {
    p <- p + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 7.5) +
      ggplot2::ggtitle(names(seed)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5, family = getOption("book.font.type", "sans")))
  }
  return(p)
}
