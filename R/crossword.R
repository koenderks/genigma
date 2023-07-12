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

crossword <- function(seed, wordlist, type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
  x <- y <- z <- NULL
  size <- 15
  words <- wordlist[nchar(wordlist) > 2 & nchar(wordlist) < size + 1]
  object <- .initCrossword(rows = size, columns = size)
  while (nrow(object[["included"]]) < 25 && length(words) > 0) {
    index <- sample.int(length(words), size = 1)
    word <- words[index]
    words <- words[-index]
    descriptions <- try(
      {
        suppressWarnings({
          dictionaRy::define(word)$definition
        })
      },
      silent = TRUE
    )
    if (inherits(descriptions, "try-error")) {
      next
    }
    descriptions <- descriptions[which(nchar(descriptions) < 100)]
    if (length(descriptions) == 0) {
      next
    }
    description <- descriptions[sample.int(length(descriptions), size = 1)]
    object <- .addCrossword(object, word, description)
  }
  grid <- object[["grid"]]
  included <- object[["included"]]
  included[["row"]] <- size + 1 - included[["row"]]
  grid <- grid[, -c(1, ncol(grid))]
  grid <- grid[-c(1, nrow(grid)), ]
  grid[grid == "#" | grid == "."] <- ""
  canvas <- data.frame(
    y = (size + 1) - rep(seq_len(size), times = size),
    x = rep(seq_len(size), each = size), z = c(grid)
  )
  descriptions <- included[["description"]]
  words <- tolower(included[["word"]])
  names(descriptions) <- seq_len(length(descriptions))
  p1 <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_rect(xmin = canvas$x - 0.5, xmax = canvas$x + 0.5, ymin = canvas$y - 0.5, ymax = canvas$y + 0.5, fill = "#ffffff", col = "black", linewidth = 0.25) +
    ggplot2::geom_rect(xmin = canvas$x - 0.5, xmax = canvas$x + 0.5, ymin = canvas$y - 0.5, ymax = canvas$y + 0.5, fill = c(ifelse(grid == "", "black", NA)), col = NA, linewidth = 0.25) +
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
    return(p1)
  } else if (type == "example") {
    p1 <- p1 + ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
    init <- included[1, ]
    xs <- switch(init$direction,
      "right" = init$col + 0:(init$length - 1),
      "down" = rep(init$col, init$length)
    )
    ys <- switch(init$direction,
      "right" = rep(init$row, init$length),
      "down" = init$row + 0:(init$length - 1)
    )
    for (i in seq_len(nrow(included))) {
      p1 <- p1 + ggplot2::annotate(geom = "text", x = included$col[i] - 0.3, y = included$row[i] + 0.3, label = i, size = 1.5) +
        ggplot2::annotate(geom = "text", x = xs, y = ys, label = strsplit(init$word, split = "")[[1]], size = 3)
    }
    return(p1)
  } else {
    for (i in seq_len(nrow(included))) {
      p1 <- p1 + ggplot2::annotate(geom = "text", x = included[i, "col"] - 0.35, y = included[i, "row"] + 0.35, label = i, size = 4)
    }
    xs <- rep(1, length(descriptions))
    ys <- rev(seq_len(length(descriptions)))
    prefix <- ifelse(names(descriptions) %in% as.character(1:9), paste0(names(descriptions), ".  "), paste0(names(descriptions), ". "))
    p2 <- ggplot2::ggplot(data = data.frame(x = xs, y = ys, z = paste0(prefix, descriptions))) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = z), size = 5.5, hjust = 0) +
      ggplot2::scale_x_continuous(limits = c(1, 5)) +
      ggplot2::scale_y_continuous(limits = c(1, length(descriptions))) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),
      )
    title_grob <- grid::textGrob(paste0("— Crossword ", names(seed), " —"), gp = grid::gpar(fontsize = 75, fontfamily = getOption("book.font.type", "sans"), fontface = "bold"))
    return(gridExtra::grid.arrange(p2, p1, layout_matrix = matrix(c(rep(2, 16), rep(1, 8)), byrow = TRUE, nrow = 6, ncol = 4), top = title_grob))
  }
}

.initCrossword <- function(rows = 10, columns = 10, verbose = FALSE) {
  object <- list()
  rows <- rows + 2
  object$rows <- rows
  columns <- columns + 2
  object$columns <- columns
  tmp <- data.frame(
    row = rep(seq_len(rows), columns),
    col = rep(seq_len(columns), each = rows),
    space_right = NA,
    space_down = NA,
    stringsAsFactors = FALSE
  )
  tmp$space_right <- columns - tmp$col + 1
  tmp$space_down <- rows - tmp$row + 1
  object$grid <- matrix(".", nrow = rows, ncol = columns)
  object$grid[1, ] <- "#"
  object$grid[dim(object$grid)[1], ] <- "#"
  object$grid[, 1] <- "#"
  object$grid[, dim(object$grid)[2]] <- "#"
  object$included <- data.frame(
    row = integer(),
    col = integer(),
    word = character(),
    direction = character(),
    description = character(),
    length = integer(),
    stringsAsFactors = FALSE
  )
  object$restrictions_right <- matrix("", nrow = rows, ncol = columns)
  object$restrictions_down <- matrix("", nrow = rows, ncol = columns)
  return(object)
}

.addCrossword <- function(object, word, description) {
  word <- paste0("#", word, "#")
  word <- word[nchar(word) <= object$columns & nchar(word) <= object$rows]
  if (length(word) == 0) {
    return(object)
  }
  object <- .updateCrossword(object)
  iffer <- .greplvCrossword(substring(object$restrictions_down$val, 1, nchar(word)), word) & nchar(object$restrictions_down$val) >= nchar(word)
  down <- object$restrictions_down |>
    dplyr::filter(iffer) |>
    dplyr::rename(length = nchar) |>
    dplyr::mutate(
      direction = "down",
      word = word,
      description = description,
      val = substring(val, 1, nchar(word))
    )
  iffer <- .greplvCrossword(substring(object$restrictions_right$val, 1, nchar(word)), word) & nchar(object$restrictions_right$val) >= nchar(word)
  right <- object$restrictions_right |>
    dplyr::filter(iffer) |>
    dplyr::rename(length = nchar) |>
    dplyr::mutate(
      direction = "right",
      word = word,
      description = description,
      val = substring(val, 1, nchar(word))
    )
  if ((nrow(right) + nrow(down)) > 0) {
    words_right <- length(object$included$direction == "right")
    words_down <- length(object$included$direction == "down")
    tmp <- rbind(right, down)
    tmp$weight <- 1
    tmp$weight[tmp$direction == "right"] <- tmp$weight[tmp$direction == "right"] + max(words_down - words_right, 0)
    tmp$weight[tmp$direction == "down"] <- tmp$weight[tmp$direction == "down"] + max(words_right - words_down, 0)
    tmp$weight <- tmp$weight + stringr::str_count(tmp$val, pattern = "[[:alpha:]]")
    tmp$weight <- tmp$weight + (abs(tmp$row - object$rows / 2) + abs(tmp$col - object$columns / 2)) / (object$rows / 2 + object$columns / 2)
    new_word <- tmp |>
      dplyr::filter(weight == max(weight)) |>
      dplyr::mutate(word = stringr::str_replace_all(word, "([^[:alpha:]])", ""), length = nchar(word)) |>
      dplyr::slice(1) |>
      dplyr::select(-val, -weight)
    object <- .injectCrossword(object, word = paste0("#", new_word$word, "#"), row = new_word$row, column = new_word$col, direction = new_word$direction)
    if (new_word$direction == "down") {
      new_word$col <- new_word$col - 1L
    } else if (new_word$direction == "right") {
      new_word$row <- new_word$row - 1L
    }
    object$included <- rbind(object$included, new_word)
  }
  return(object)
}

.updateCrossword <- function(object) {
  object$restrictions_right <- matrix("", nrow = object$rows, ncol = object$columns)
  object$restrictions_down <- matrix("", nrow = object$rows, ncol = object$columns)
  for (rowi in seq_len(object$rows)) {
    for (coli in seq_len(object$columns)) {
      object$restrictions_right[rowi, coli] <- paste(object$grid[rowi, coli:object$columns], collapse = "")
      object$restrictions_down[rowi, coli] <- paste(object$grid[rowi:object$rows, coli], collapse = "")
    }
  }
  object$restrictions_right <- .convertMatrix(object$restrictions_right)
  object$restrictions_down <- .convertMatrix(object$restrictions_down)
  object$restrictions_right$nchar <- nchar(object$restrictions_right$val)
  object$restrictions_down$nchar <- nchar(object$restrictions_down$val)
  object$restrictions_down <- dplyr::anti_join(object$restrictions_down, object$included |> dplyr::filter(direction == "down"), by = c("row", "col"))
  object$restrictions_right <- dplyr::anti_join(object$restrictions_right, object$included |> dplyr::filter(direction == "right"), by = c("row", "col"))
  return(object)
}

.injectCrossword <- function(object, word, row = 1, column = 1, direction = c("down", "right")) {
  if (direction == "right") {
    stopifnot(nchar(word) <= (object$columns - column + 1))
    object$grid[row, column:(column + nchar(word) - 1)] <- unlist(strsplit(word, ""))
  } else if (direction == "down") {
    stopifnot(nchar(word) <= (object$rows - row + 1))
    object$grid[row:(row + nchar(word) - 1), column] <- unlist(strsplit(word, ""))
  }
  return(object)
}

.greplvCrossword <- compiler::cmpfun(Vectorize(grepl, vectorize.args = "pattern"))

.convertMatrix <- function(x) {
  data.frame(
    row = as.vector(row(x)),
    col = as.vector(col(x)),
    val = as.vector(x),
    stringsAsFactors = FALSE
  )
}
