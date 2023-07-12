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

mandala <- function(type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
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
  mandala <- rlist::list.rbind(lapply(mandala, .extractGroup))
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
      plot.margin = ggplot2::unit(c(2, 0, 0, 2), "cm")
    )
  if (type == "puzzle" || type == "example") {
    suppressMessages({
      p <- p + ggplot2::scale_fill_gradientn(colors = "#ffffff") +
        ggplot2::theme(plot.margin = ggplot2::unit(rep(0.2, 4), "cm"))
    })
  }
  return(p)
}

.extractGroup <- function(tile) {
  as.data.frame(rlist::list.match(tile, "ptNum|x|y|area"))
}
