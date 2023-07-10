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

tictactoe <- function(type = c("puzzle", "example")) {
  type <- match.arg(type)
  x <- c(0, 0, 1, 2)
  xend <- c(3, 3, 1, 2)
  y <- c(1, 2, 0, 0)
  yend <- c(1, 2, 3, 3)
  pd <- data.frame(x = x, y = y, xend = xend, yend = yend)
  p <- ggplot2::ggplot(data = pd) +
    ggplot2::geom_segment(x = x, y = y, xend = xend, yend = yend, linewidth = 1.25) +
    ggplot2::scale_x_continuous(limits = c(0, 3)) +
    ggplot2::scale_y_continuous(limits = c(0, 3)) +
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
    )
  if (type == "example") {
    x <- c(0.25, 0.75)
    y <- c(0.25, 0.25) + rep(0:2, each = 2)
    xend <- c(0.75, 0.25)
    yend <- c(0.75, 0.75) + rep(0:2, each = 2)
    pd <- data.frame(x = x, y = y, xend = xend, yend = yend)
    p <- p + ggplot2::geom_segment(data = pd, mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5)
    # Add circle with radius 0.5 to (1, 1), (3, 1) (2, 1)
  }
  return(p)
}
