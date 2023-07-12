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
  words <- wordlist[nchar(wordlist) > 2 & nchar(wordlist) < size]
  cw <- Crossword$new(rows = size, columns = size)
  while (nrow(cw$words) < 25 && length(words) > 0) {
    index <- sample.int(length(words), size = 1)
    word <- words[index]
    words <- words[-index]
    clues <- try(
      {
        suppressWarnings({
          dictionaRy::define(word)$definition
        })
      },
      silent = TRUE
    )
    if (inherits(clues, "try-error")) {
      next
    }
    clues <- clues[which(nchar(clues) < 100)]
    if (length(clues) == 0) {
      next
    }
    clue <- clues[sample.int(length(clues), size = 1)]
    cw$add_words(words = word, clues = clue)
  }
  grid <- cw$letters
  used <- cw$words
  used$row <- size + 1 - used$row
  grid <- grid[, -1]
  grid <- grid[, -ncol(grid)]
  grid <- grid[-1, ]
  grid <- grid[-nrow(grid), ]
  grid[grid == "#" | grid == "."] <- ""
  canvas <- data.frame(
    y = (size + 1) - rep(seq_len(size), times = size),
    x = rep(seq_len(size), each = size), z = c(grid)
  )
  usedSyns <- used$clue
  usedWords <- tolower(used$word)
  names(usedSyns) <- seq_len(length(usedSyns))
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
    init <- used[1, ]
    xs <- switch(init$direction,
      "right" = init$col + 0:(init$length - 1),
      "down" = rep(init$col, init$length)
    )
    ys <- switch(init$direction,
      "right" = rep(init$row, init$length),
      "down" = init$row + 0:(init$length - 1)
    )
    for (i in seq_len(nrow(used))) {
      p1 <- p1 + ggplot2::annotate(geom = "text", x = used$col[i] - 0.3, y = used$row[i] + 0.3, label = i, size = 1.5) +
        ggplot2::annotate(geom = "text", x = xs, y = ys, label = strsplit(init$word, split = "")[[1]], size = 3)
    }
    return(p1)
  } else {
    for (i in seq_len(nrow(used))) {
      p1 <- p1 + ggplot2::annotate(geom = "text", x = used[i, "col"] - 0.35, y = used[i, "row"] + 0.35, label = i, size = 4)
    }
    xs <- rep(1, length(usedSyns))
    ys <- length(usedSyns):1
    fronts <- ifelse(names(usedSyns) %in% as.character(1:9), paste0(names(usedSyns), ".  "), paste0(names(usedSyns), ". "))
    p2 <- ggplot2::ggplot(data = data.frame(x = xs, y = ys, z = paste0(fronts, usedSyns))) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = z), size = 5.5, hjust = 0) +
      ggplot2::scale_x_continuous(limits = c(1, 5)) +
      ggplot2::scale_y_continuous(limits = c(1, length(usedSyns))) +
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

Crossword <- R6::R6Class(
  private = list(
    rows = NULL,
    columns = NULL,
    restrictions_right = NULL,
    restrictions_down = NULL,
    add_word = function(word, clue = "") {
      word <- .normalize_words(word)
      word <- word[nchar(word) <= private$columns & nchar(word) <= private$rows]
      if (length(word) == 0) {
        self$message("word does not fit at all.")
        return(self)
      }
      private$update_grid_data()
      iffer <- cw_greplv(substring(private$restrictions_down$val, 1, nchar(word)), word) & nchar(private$restrictions_down$val) >= nchar(word)
      down <- private$restrictions_down |>
        dplyr::filter(iffer) |>
        dplyr::rename(length = nchar) |>
        dplyr::mutate(
          direction = "down",
          word      = word,
          clue      = clue,
          val       = substring(val, 1, nchar(word))
        )
      iffer <- cw_greplv(substring(private$restrictions_right$val, 1, nchar(word)), word) & nchar(private$restrictions_right$val) >= nchar(word)
      right <- private$restrictions_right |>
        dplyr::filter(iffer) |>
        dplyr::rename(length = nchar) |>
        dplyr::mutate(
          direction = "right",
          word      = word,
          clue      = clue,
          val       = substring(val, 1, nchar(word))
        )
      if ((nrow(right) + nrow(down)) > 0) {
        words_right <- length(self$words$direction == "right")
        words_down <- length(self$words$direction == "down")
        tmp <- rbind(right, down)
        tmp$weight <- 1
        tmp$weight[tmp$direction == "right"] <- tmp$weight[tmp$direction == "right"] + max(words_down - words_right, 0)
        tmp$weight[tmp$direction == "down"] <- tmp$weight[tmp$direction == "down"] + max(words_right - words_down, 0)
        tmp$weight <- tmp$weight + stringr::str_count(tmp$val, pattern = "[[:alpha:]]")
        tmp$weight <- tmp$weight + (abs(tmp$row - private$rows / 2) + abs(tmp$col - private$columns / 2)) / (private$rows / 2 + private$columns / 2)
        new_word <- tmp |>
          dplyr::filter(weight == max(weight)) |>
          dplyr::mutate(word = stringr::str_replace_all(word, "([^[:alpha:]])", ""), length = nchar(word)) |>
          dplyr::slice(1) |>
          dplyr::select(-val, -weight)
        private$put_word_on_grid(word = paste0("#", new_word$word, "#"), row = new_word$row, column = new_word$col, direction = new_word$direction)
        if (new_word$direction == "down") {
          new_word$col <- new_word$col - 1L
        } else if (new_word$direction == "right") {
          new_word$row <- new_word$row - 1L
        }
        self$words <- rbind(self$words, new_word)
      } else {
        self$message("Could not place on grid - nothing that suffices restrictions")
      }
      return(self)
    },
    put_word_on_grid = function(word, row = 1, column = 1, direction = c("down", "right")) {
      self$message(c(word, row, column, direction, "\n\n", sep = " / "))
      if (direction == "right") {
        stopifnot(nchar(word) <= (private$columns - column + 1))
        self$letters[row, column:(column + nchar(word) - 1)] <- unlist(strsplit(word, ""))
      } else if (direction == "down") {
        stopifnot(nchar(word) <= (private$rows - row + 1))
        self$letters[row:(row + nchar(word) - 1), column] <- unlist(strsplit(word, ""))
      } else {
        stop("direction neither 'down' nor 'right'")
      }
      return(self)
    },
    update_grid_data = function() {
      private$restrictions_right <- matrix("", nrow = private$rows, ncol = private$columns)
      private$restrictions_down <- matrix("", nrow = private$rows, ncol = private$columns)
      for (rowi in seq_len(private$rows)) {
        for (coli in seq_len(private$columns)) {
          private$restrictions_right[rowi, coli] <- paste(self$letters[rowi, coli:private$columns], collapse = "")
          private$restrictions_down[rowi, coli] <- paste(self$letters[rowi:private$rows, coli], collapse = "")
        }
      }
      private$restrictions_right <- .matrix_to_df(private$restrictions_right)
      private$restrictions_down <- .matrix_to_df(private$restrictions_down)
      private$restrictions_right$nchar <- nchar(private$restrictions_right$val)
      private$restrictions_down$nchar <- nchar(private$restrictions_down$val)
      private$restrictions_down <- dplyr::anti_join(private$restrictions_down, self$words |> dplyr::filter(direction == "down"), by = c("row", "col"))
      private$restrictions_right <- dplyr::anti_join(private$restrictions_right, self$words |> dplyr::filter(direction == "right"), by = c("row", "col"))
      return(self)
    }
  ),
  active = NULL,
  lock_objects = TRUE,
  class = TRUE,
  portable = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  classname = "crossword",
  inherit = r6extended::r6extended,
  public = list(
    letters = NULL,
    words = NULL,
    initialize = function(rows = 10, columns = 10, verbose = FALSE) {
      self$options$verbose <- verbose
      rows <- rows + 2
      private$rows <- rows
      columns <- columns + 2
      private$columns <- columns
      tmp <- data.frame(
        row              = rep(seq_len(rows), columns),
        col              = rep(seq_len(columns), each = rows),
        space_right      = NA,
        space_down       = NA,
        stringsAsFactors = FALSE
      )
      tmp$space_right <- columns - tmp$col + 1
      tmp$space_down <- rows - tmp$row + 1
      self$letters <- matrix(".", nrow = rows, ncol = columns)
      self$letters[1, ] <- "#"
      self$letters[dim(self$letters)[1], ] <- "#"
      self$letters[, 1] <- "#"
      self$letters[, dim(self$letters)[2]] <- "#"
      self$words <- data.frame(
        row              = integer(),
        col              = integer(),
        word             = character(),
        direction        = character(),
        clue             = character(),
        length           = integer(),
        stringsAsFactors = FALSE
      )
      private$restrictions_right <- matrix("", nrow = rows, ncol = columns)
      private$restrictions_down <- matrix("", nrow = rows, ncol = columns)
      return(self)
    },
    add_words = function(words, clues = NULL) {
      if (is.null(clues)) {
        clues <- rep("", length(words))
      }
      for (i in seq_along(words)) {
        private$add_word(
          word = words[i],
          clue = clues[i]
        )
      }
    }
  )
)

.normalize_words <- function(words) {
  iffer <- stringr::str_detect(words, "\\W")
  if (sum(iffer) > 0) {
    warning(
      "There are words containing non-letters: ",
      paste(words[iffer], collapse = "; ")
    )
  }
  words <- toupper(words)
  words <- stringr::str_replace_all(words, " +", "")
  words <- stringr::str_replace_all(words, "\u00c4", "AE")
  words <- stringr::str_replace_all(words, "\u00d6", "OE")
  words <- stringr::str_replace_all(words, "\u00dc", "UE")
  words <- stringr::str_replace_all(words, "\u00df", "SS")
  words <- paste0("#", words, "#")
  return(words)
}

cw_greplv <- compiler::cmpfun(Vectorize(grepl, vectorize.args = "pattern"))

.matrix_to_df <- function(x) {
  data.frame(
    row = as.vector(row(x)),
    col = as.vector(col(x)),
    val = as.vector(x),
    stringsAsFactors = FALSE
  )
}
