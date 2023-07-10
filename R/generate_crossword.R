# Copyright (C) 2023-2023 Koen Derks

generate_crossword <- function(seed, wordlist, type = c("puzzle", "solution", "example")) {
  type <- match.arg(type)
  x <- y <- z <- NULL
  size <- 15
  words <- wordlist[nchar(wordlist) > 2 & nchar(wordlist) < 10]
  grid <- matrix("", nrow = size, ncol = size)
  usedWordsList <- list()
  reqWords <- sample(20:50, size = 1)
  level <- ceiling(reqWords / 10)
  while (length(usedWordsList) < reqWords && length(words) > 0) {
    direction <- sample(c("horizontal", "vertical"), 1)
    validPlacement <- FALSE
    while (!validPlacement && length(words) > 0) {
      index <- sample.int(length(words), size = 1)
      word <- words[index]
      words <- words[-index]
      syn <- syn::syn(word, n_words = 1)
      if (length(syn) == 0) {
        next
      }
      if (direction == "horizontal") {
        row <- sample(1:size, 1)
        col <- sample(1:(size - nchar(word) + 1), 1)
      } else if (direction == "vertical") {
        row <- sample(1:(size - nchar(word) + 1), 1)
        col <- sample(1:size, 1)
      }
      validPlacement <- checkCrossWordPlacement(word, row, col, direction, grid)
      if (validPlacement) {
        wordEntry <- list(word = word, syn = syn, x = numeric(), y = numeric())
        for (i in 1:nchar(word)) {
          currentRow <- row
          currentCol <- col
          if (direction == "horizontal") {
            currentCol <- col + i - 1
          } else if (direction == "vertical") {
            currentRow <- row + i - 1
          }
          grid[currentRow, currentCol] <- substr(word, i, i)
          wordEntry$x <- c(wordEntry$x, currentCol)
          wordEntry$y <- c(wordEntry$y, currentRow)
        }
        usedWordsList[[length(usedWordsList) + 1]] <- wordEntry
      }
    }
  }
  if (TRUE) {
    grid[which(grid == "")] <- ""
  }
  canvas <- data.frame(
    x = rep(seq_len(size), times = size),
    y = rep(seq_len(size), each = size), z = c(grid)
  )
  usedSyns <- unlist(lapply(usedWordsList, `[[`, 2))
  usedWords <- unlist(lapply(usedWordsList, `[[`, 1))
  names(usedSyns) <- seq_len(length(usedSyns))
  # Plot small numbers in the bottom right corner of each 1x1 square
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
    for (i in seq_len(length(usedWords))) {
      word <- usedWords[i]
      p1 <- p1 + ggplot2::annotate(geom = "text", x = usedWordsList[[i]]$y[1] - 0.3, y = usedWordsList[[i]]$x[1] + 0.3, label = i, size = 2) +
        ggplot2::annotate(geom = "text", x = usedWordsList[[1]]$y, y = usedWordsList[[1]]$x, label = strsplit(usedWordsList[[1]]$word, split = "")[[1]], size = 4)
    }
    return(p1)
  } else {
    for (i in seq_len(length(usedWords))) {
      word <- usedWords[i]
      p1 <- p1 + ggplot2::annotate(geom = "text", x = usedWordsList[[i]]$y[1] - 0.35, y = usedWordsList[[i]]$x[1] + 0.35, label = i, size = 4)
    }
    xs <- rep(1:4, length.out = length(usedSyns))
    ys <- rep(1:max(table(xs)), each = 4, length.out = length(usedSyns))
    p2 <- ggplot2::ggplot(data = data.frame(x = xs, y = max(ys) - ys, z = paste0(names(usedSyns), ". ", tools::toTitleCase(usedSyns)))) +
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
    title_grob <- grid::textGrob(paste0("— Crossword ", names(seed), " ~ Level ", level, " —"), gp = grid::gpar(fontsize = 75, fontfamily = getOption("book.font.type", "sans"), fontface = "bold"))
    return(gridExtra::grid.arrange(p2, p1, layout_matrix = matrix(c(rep(2, 16), rep(1, 8)), byrow = TRUE, nrow = 6, ncol = 4), top = title_grob))
  }
}

# Function to check if a word can be placed at a specific position
checkCrossWordPlacement <- function(word, row, col, direction, grid) {
  for (i in 1:nchar(word)) {
    currentRow <- row
    currentCol <- col
    if (direction == "horizontal") {
      currentCol <- col + i - 1
    } else if (direction == "vertical") {
      currentRow <- row + i - 1
    }
    if (currentCol < 1 || currentCol > ncol(grid) || currentRow < 1 || currentRow > nrow(grid)) {
      return(FALSE)
    }
    if (grid[currentRow, currentCol] != "" && grid[currentRow, currentCol] != substr(word, i, i)) {
      return(FALSE)
    }
  }
  return(TRUE)
}
