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

.instruction <- function(title, label) {
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

.readWordList <- function() {
  wordlist <- readLines("https://www.mit.edu/~ecprice/wordlist.10000")
  return(wordlist)
}
