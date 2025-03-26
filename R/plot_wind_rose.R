#' @title Plot wind rose.
#' @description Plot wind rose.
#' @param data data.frame.
#' @param spd vector. Wind speed.
#' @param dir vector. Wind direction.
#' @param spdres integer. Resolution of wind speed scale.
#' @param dirres integer. Resolution of wind direction scale.
#' @param spdmin integer. Minimum of wind speed.
#' @param spdmax integer. Maximum of wind speed.
#' @param spdseq vector. Scale sequence for wind speed
#' (missing, uses spdmin, spdmax and spdres)
#' @param palette character. Palette for the wind speed.
#' @param countmax integer. y-axis adjustments.
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom latex2exp TeX
#' @importFrom stats na.omit
#' @importFrom utils packageVersion
#' @import ggplot2
#' @return a ggplot2 of the form of a wind rose
# nolint start
#' @references Code from https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
# nolint end
plot_windrose <- function(
  data,
  spd,
  dir,
  spdres = 2,
  dirres = 30,
  spdmin = 0,
  spdmax = 20,
  spdseq = NULL,
  palette = "YlGnBu",
  countmax = NA
) {
  spd_binned <- NULL
  # Look to see what data was passed in to the function
  if (is.numeric(spd) && is.numeric(dir)) {
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd, dir = dir)
    spd <- "spd"
    dir <- "dir"
  } else if (exists("data")) {
    # Assume that we've been given a data frame, and the name of the speed
    # and direction columns. This is the format we want for later use.
  }

  # Tidy up input data ----
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  if (missing(spdseq)) {
    spdseq <- seq(spdmin, spdmax, spdres)
  }
  # get some information about the number of bins, etc.
  n_spd_seq <- length(spdseq)
  n_colors_in_range <- n_spd_seq - 1

  # create the color map
  spd_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(
    min(
      max(
        3,
        n_colors_in_range
      ),
      min(
        9,
        n_colors_in_range
      )
    ),
    palette
  ))(n_colors_in_range)

  if (max(data[[spd]], na.rm = TRUE) > spdmax) {
    spd_breaks <- c(
      spdseq,
      max(data[[spd]], na.rm = TRUE)
    )
    spd_labels <- c(
      paste(
        c(spdseq[1:n_spd_seq - 1]),
        "-",
        c(spdseq[2:n_spd_seq])
      ),
      paste(
        spdmax,
        "-",
        max(data[[spd]], na.rm = TRUE)
      )
    )
    spd_colors <- c(spd_colors, "grey50")
  } else {
    spd_breaks <- spdseq
    spd_labels <- paste(
      c(spdseq[1:n_spd_seq - 1]),
      "-",
      c(spdseq[2:n_spd_seq])
    )
  }
  data$spd_binned <- cut(
    x = data[[spd]],
    breaks = spd_breaks,
    labels = spd_labels,
    ordered_result = TRUE
  )
  # clean up the data
  data <- stats::na.omit(data)
  # figure out the wind direction bins
  dir_breaks <- c(
    -dirres / 2,
    seq(dirres / 2, 360 - dirres / 2, by = dirres),
    360 + dirres / 2
  )
  dir_labels <- c(
    paste(360 - dirres / 2, "-", dirres / 2),
    paste(
      seq(dirres / 2, 360 - 3 * dirres / 2, by = dirres),
      "-",
      seq(3 * dirres / 2, 360 - dirres / 2, by = dirres)
    ),
    paste(360 - dirres / 2, "-", dirres / 2)
  )
  # assign each wind direction to a bin
  dir_binned <- cut(data[[dir]],
    breaks = dir_breaks,
    ordered_result = TRUE
  )
  levels(dir_binned) <- dir_labels
  data$dir_binned <- dir_binned

  # deal with change in ordering introduced somewhere around version 2.2
  if (utils::packageVersion("ggplot2") > "2.2") {
    data$spd_binned <- with(
      data,
      factor(spd_binned, levels = rev(levels(spd_binned)))
    )
    spd_colors <- rev(spd_colors)
  }

  # create the plot ----
  p_windrose <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = dir_binned,
      fill = spd_binned
    )
  ) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      labels = ggplot2::waiver()
    ) +
    ggplot2::coord_polar(start = -((dirres / 2) / 360) * 2 * pi) +
    ggplot2::scale_fill_manual(
      name = latex2exp::TeX("Wind Speed ($m.s^{-1}$)"),
      values = spd_colors,
      drop = FALSE
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  # adjust axes if required
  if (!is.na(countmax)) {
    p_windrose <- p_windrose +
      ggplot2::ylim(c(0, countmax))
  }

  # print the plot
  print(p_windrose)

  # return the handle to the wind rose
  return(p_windrose)
}
