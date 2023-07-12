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

wordfinder <- function(seed, wordlist, type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
  x <- y <- z <- NULL
  size <- 15
  words <- wordlist[nchar(wordlist) > 3 & nchar(wordlist) < size]
  grid <- matrix("", nrow = size, ncol = size)
  usedWordsList <- list()
  while (length(usedWordsList) < 50 && length(words) > 0) {
    direction <- sample(c("horizontal-right", "horizontal-left", "vertical-down", "vertical-up", "diagonal-left-down", "diagonal-right-down", "diagonal-left-up", "diagonal-right-up"), 1)
    validPlacement <- FALSE
    while (!validPlacement && length(words) > 0) {
      index <- sample.int(length(words), size = 1)
      word <- words[index]
      words <- words[-index]
      if (direction == "horizontal-right" || direction == "horizontal-left") {
        row <- sample(1:size, 1)
        col <- sample(1:(size - nchar(word) + 1), 1)
      } else if (direction == "vertical-down" || direction == "vertical-up") {
        row <- sample(1:(size - nchar(word) + 1), 1)
        col <- sample(1:size, 1)
      } else {
        row <- sample(1:(size - nchar(word) + 1), 1)
        col <- sample(1:(size - nchar(word) + 1), 1)
      }
      validPlacement <- .checkWordFinderPlacement(word, row, col, direction, grid)
      if (validPlacement) {
        wordEntry <- list(word = word, x = numeric(), y = numeric())
        for (i in 1:nchar(word)) {
          currentCol <- switch(direction,
            "horizontal-right" = col + i - 1,
            "horizontal-left" = col - i + 1,
            "vertical-down" = col,
            "vertical-up" = col,
            "diagonal-left-down" = col - i - 1,
            "diagonal-right-down" = col + i + 1,
            "diagonal-left-up" = col - i - 1,
            "diagonal-right-up" = col + i + 1
          )
          currentRow <- switch(direction,
            "horizontal-right" = row,
            "horizontal-left" = row,
            "vertical-down" = row - i - 1,
            "vertical-up" = row + i + 1,
            "diagonal-left-down" = row - i - 1,
            "diagonal-right-down" = row - i - 1,
            "diagonal-left-up" = row + i + 1,
            "diagonal-right-up" = row + i + 1
          )
          grid[currentCol, currentRow] <- substr(word, i, i)
          wordEntry$x <- c(wordEntry$x, currentCol)
          wordEntry$y <- c(wordEntry$y, currentRow)
        }
        usedWordsList[[length(usedWordsList) + 1]] <- wordEntry
      }
    }
  }
  grid[which(grid == "")] <- sample(letters, size = length(which(grid == "")), replace = TRUE)
  canvas <- data.frame(
    x = rep(seq_len(size), times = size),
    y = rep(seq_len(size), each = size), z = c(grid)
  )
  usedWords <- sort(unlist(lapply(usedWordsList, `[[`, 1)), decreasing = FALSE)
  p1 <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_rect(xmin = canvas$x - 0.5, xmax = canvas$x + 0.5, ymin = canvas$y - 0.5, ymax = canvas$y + 0.5, fill = "#ffffff", col = "black", linewidth = 0.25) +
    ggplot2::geom_rect(xmin = 0.5, xmax = size + 0.5, ymin = 0.5, ymax = size + 0.5, fill = NA, col = "black", linewidth = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0.5, size + 0.5)) +
    ggplot2::scale_y_continuous(limits = c(0.5, size + 0.5)) +
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
  if (type == "solution") {
    p1 <- p1 + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 5) +
      ggplot2::ggtitle(names(seed)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5, family = getOption("book.font.type", "sans")))
    for (i in seq_len(length(usedWordsList))) {
      pd <- data.frame(x = usedWordsList[[i]]$x, y = usedWordsList[[i]]$y)
      p1 <- p1 + ggplot2::geom_line(data = pd, mapping = ggplot2::aes(x = x, y = y))
    }
    return(p1)
  } else if (type == "example") {
    p1 <- p1 + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 4) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
    for (i in seq_len(3)) {
      pd <- data.frame(x = usedWordsList[[i]]$x, y = usedWordsList[[i]]$y)
      p1 <- p1 + ggplot2::geom_line(data = pd, mapping = ggplot2::aes(x = y, y = x))
    }
    return(p1)
  } else {
    p1 <- p1 + ggplot2::annotate(geom = "text", x = canvas$x, y = canvas$y, label = canvas$z, size = 10)
    xs <- rep(1:4, length.out = length(usedWords))
    ys <- rep(1:max(table(xs)), each = 4, length.out = length(usedWords))
    p2 <- ggplot2::ggplot(data = data.frame(x = xs, y = max(ys) - ys, z = usedWords)) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = z), size = 6, hjust = 0) +
      ggplot2::scale_x_continuous(limits = c(min(xs), max(xs) + 1)) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      )
    title_grob <- grid::textGrob(paste0("— Word Finder ", names(seed), " —"), gp = grid::gpar(fontsize = 75, fontfamily = getOption("book.font.type", "sans"), fontface = "bold"))
    return(gridExtra::grid.arrange(p2, p1, layout_matrix = matrix(c(rep(2, 16), rep(1, 8)), byrow = TRUE, nrow = 6, ncol = 4), top = title_grob))
  }
}

.checkWordFinderPlacement <- function(word, row, col, direction, grid) {
  for (i in seq_len(nchar(word))) {
    currentCol <- switch(direction,
      "horizontal-right" = col + i - 1,
      "horizontal-left" = col - i + 1,
      "vertical-down" = col,
      "vertical-up" = col,
      "diagonal-left-down" = col - i - 1,
      "diagonal-right-down" = col + i + 1,
      "diagonal-left-up" = col - i - 1,
      "diagonal-right-up" = col + i + 1
    )
    currentRow <- switch(direction,
      "horizontal-right" = row,
      "horizontal-left" = row,
      "vertical-down" = row - i - 1,
      "vertical-up" = row + i + 1,
      "diagonal-left-down" = row - i - 1,
      "diagonal-right-down" = row - i - 1,
      "diagonal-left-up" = row + i + 1,
      "diagonal-right-up" = row + i + 1
    )
    if (currentCol < 1 || currentCol > ncol(grid) || currentRow < 1 || currentRow > nrow(grid)) {
      return(FALSE)
    }
    if (grid[currentCol, currentRow] != "" && grid[currentCol, currentRow] != substr(word, i, i)) {
      return(FALSE)
    }
  }
  return(TRUE)
}
