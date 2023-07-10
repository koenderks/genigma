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

nonogram <- function(seed, type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
  size <- 20
  pos <- c(LETTERS, letters)
  lab <- pos[sample.int(length(pos), 1)]
  im <- magick::image_read(paste0("https://latex.codecogs.com/png.image?", lab))
  im3 <- magick::image_scale(im, geometry = paste0(size, "x", size))
  im3 <- magick::image_rotate(im3, degrees = 90)
  mat <- as.integer(im3[[1]])
  mat <- mat[, , dim(mat)[3]]
  if (nrow(mat) < size) {
    mat <- rbind(mat, matrix(0, nrow = size - nrow(mat), ncol = ncol(mat)))
  }
  if (ncol(mat) < size) {
    mat <- cbind(mat, matrix(0, ncol = size - ncol(mat), nrow = nrow(mat)))
  }
  mat <- ifelse(mat == 0, 0, 1)
  nonogram <- list()
  nonogram$cols <- countNonogramGroups(mat, "row")
  nonogram$rows <- countNonogramGroups(mat, "col")
  canvas <- data.frame(
    x = rep(seq_len(size), times = size),
    y = rep(seq_len(size), each = size), z = c(mat)
  )
  p <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_rect(xmin = canvas$x - 0.5, xmax = canvas$x + 0.5, ymin = canvas$y - 0.5, ymax = canvas$y + 0.5, fill = "#ffffff", col = "black", linewidth = 0.25) +
    ggplot2::geom_rect(xmin = 0.5, xmax = size + 0.5, ymin = 0.5, ymax = size + 0.5, fill = NA, col = "black", linewidth = 0.5) +
    ggplot2::scale_x_continuous(limits = c(-3.5, size + 1.5)) +
    ggplot2::scale_y_continuous(limits = c(-0.5, size + 3.5)) +
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
    p <- p + ggplot2::ggtitle(paste0("— Nonogram ", names(seed), " —")) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(1, 0, 21 - 13.5 - 1, 0), "cm")) +
      ggplot2::annotate(geom = "text", x = 1:size, y = size + 1, label = nonogram$rows, size = 7.5, vjust = 0) +
      ggplot2::annotate(geom = "text", x = 0, y = 1:size, label = nonogram$cols, hjust = 1, size = 7.5)
  } else if (type == "solution" || type == "example") {
    for (i in 1:nrow(canvas)) {
      if (canvas$z[i] == 1) {
        row <- canvas[i, ]
        p <- p + ggplot2::geom_rect(xmin = row$x - 0.5, xmax = row$x + 0.5, ymin = row$y - 0.5, ymax = row$y + 0.5, fill = "black", col = NA, linewidth = 0.5)
      }
    }
    p <- p + ggplot2::ggtitle(names(seed)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5, family = getOption("book.font.type", "sans")))
    if (type == "example") {
      p <- p + ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) +
        ggplot2::annotate(geom = "text", x = 1:size, y = size + 1, label = nonogram$rows, size = 2, vjust = 0) +
        ggplot2::annotate(geom = "text", x = 0, y = 1:size, label = nonogram$cols, hjust = 1, size = 2)
    } else {
      p <- p + ggplot2::annotate(geom = "text", x = 1:size, y = size + 1, label = nonogram$rows, size = 3, vjust = 0) +
        ggplot2::annotate(geom = "text", x = 0, y = 1:size, label = nonogram$cols, hjust = 1, size = 3)
    }
  }
  return(p)
}

countNonogramGroups <- function(mat, type = c("row", "col")) {
  counts <- list()
  if (type == "row") {
    mat <- t(mat)
  }
  for (i in 1:nrow(mat)) {
    counts[[i]] <- 0
    index <- 1
    for (j in 1:ncol(mat)) {
      if (mat[i, j] == 1) {
        counts[[i]][index] <- counts[[i]][index] + 1
      } else {
        index <- index + 1
        counts[[i]][index] <- 0
      }
    }
    sep <- switch(type,
      "row" = " ",
      "col" = "\n"
    )
    counts[[i]] <- counts[[i]][which(counts[[i]] != 0)]
    if (type == "col") {
      counts[[i]] <- rev(counts[[i]])
    }
    if (length(counts[[i]]) == 0) {
      counts[[i]] <- 0
    }
    counts[[i]] <- paste0(counts[[i]], collapse = sep)
  }
  counts <- unlist(counts)
  return(counts)
}
