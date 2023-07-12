# Copyright (C) 2023-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

sudoku <- function(seed, type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
  blank <- sample(21:70, size = 1)
  level <- ceiling((blank - 20) / 10)
  if (type == "solution") {
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
    ggplot2::annotate(geom = "rect", xmin = rep(c(0.5, 3.5, 6.5), times = 3), xmax = rep(c(3.5, 6.5, 9.5), times = 3), ymin = rep(c(0.5, 3.5, 6.5), each = 3), ymax = rep(c(3.5, 6.5, 9.5), each = 3), fill = NA, col = "black", linewidth = 1.25) +
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
  if (type == "puzzle") {
    p <- p + ggplot2::ggtitle(paste0("— Sudoku ", names(seed), " ", rep("*", level), " —")) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(1, 0, 21 - 13.5 - 1, 0), "cm")) +
      ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 15)
  } else if (type == "example") {
    p <- p + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 6.5) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
  } else {
    p <- p + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 7.5) +
      ggplot2::ggtitle(names(seed)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5, family = getOption("book.font.type", "sans")))
  }
  return(p)
}
