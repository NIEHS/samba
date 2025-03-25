#' Load palettes
#' @description Load useful homemade palettes for plots
#' @param name character
#' @return list of colors
#' @importFrom fields tim.colors
#' @importFrom RColorBrewer brewer.pal
#' @author Eva Marques
#' @export
load_palette <- function(name) {
  stopifnot(
    "Incorrect palette name" = name %in% c(
      "covariates",
      "lcz",
      "nlcd",
      "temp",
      "temp_ipcc",
      "sw",
      "res",
      "reds",
      "prior",
      "uhi"
    )
  )
  if (name == "temp") {
    return(fields::tim.colors(n = 64, alpha = 1.0))
  } else if (name == "temp_ipcc") {
    pal_ipcc <- list(
      c(103, 0, 31),
      c(178, 24, 43),
      c(214, 96, 77),
      c(244, 165, 130),
      c(253, 219, 199),
      c(247, 247, 247),
      c(209, 229, 240),
      c(146, 197, 222),
      c(67, 147, 195),
      c(33, 102, 172),
      c(5, 48, 97)
    ) |>
      lapply(function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)) |>
      rev()
    return(pal_ipcc)
  } else if (name == "sw") {
    return(RColorBrewer::brewer.pal(10, "RdYlBu"))
  } else if (name == "res") {
    return(c("dodgerblue4", "white", "firebrick4"))
  } else if (name == "reds") {
    return(c("white", "firebrick4"))
  } else if (name == "prior") {
    return(RColorBrewer::brewer.pal(10, "RdBu"))
  } else if (name == "uhi") {
    return(c("cadetblue3", "cornsilk", "yellow", "orange", "red", "firebrick"))
  } else if (name == "lcz") {
    lcz <- rbind(
      c(1, "1", "#910613", "1-compact high-rise"),
      c(2, "2", "#D9081C", "2-compact mid-rise"),
      c(3, "3", "#FF0A22", "3-compact low-rise"),
      c(4, "4", "#C54F1E", "4-open high-rise"),
      c(5, "5", "#FF6628", "5-open mid-rise"),
      c(6, "6", "#FF985E", "6-open low-rise"),
      c(7, "7", "#FDED3F", "7-lightweight low-rise"),
      c(8, "8", "#BBBBBB", "8-large low-rise"),
      c(9, "9", "#FFCBAB", "9-sparsely built"),
      c(10, "10", "#565656", "10-heavy industry"),
      c(11, "A", "#006A18", "A-dense trees"),
      c(12, "B", "#00A926", "B-scattered trees"),
      c(13, "C", "#628432", "C-bush, scrub"),
      c(14, "D", "#B5DA7F", "D-low plants"),
      c(15, "E", "#000000", "E-bare rock or paved"),
      c(16, "F", "#FCF7B1", "F-bare soil or sand"),
      c(17, "G", "#656BFA", "G-water")
    ) |>
      as.data.frame()
    colnames(lcz) <- c("num", "class", "col", "meaning")
    return(lcz)
  } else if (name == "nlcd") {
    nlcd <- data.frame(
      class = c(
        "water",
        "water",
        "developed",
        "developed",
        "developed",
        "developed",
        "barren",
        "forest",
        "forest",
        "forest",
        "shrubland",
        "herbaceous",
        "planted",
        "planted",
        "wetlands",
        "wetlands"
      ),
      code = c(
        11,
        12,
        21,
        22,
        23,
        24,
        31,
        41,
        42,
        43,
        52,
        71,
        81,
        82,
        90,
        95
      ),
      description = c(
        "Open Water",
        "Perennial Ice/Snow",
        "Developed, Open Space",
        "Developed, Low Intensity",
        "Developed, Medium Intensity",
        "Developed, High Intensity",
        "Barren Land",
        "Deciduous Forest",
        "Evergreen Forest",
        "Mixed Forest",
        "Shrub/Scrub",
        "Herbaceous",
        "Hay/Pasture",
        "Cultivated Crops",
        "Woody Wetlands",
        "Emergent Herbaceous Wetlands"
      ),
      color = c(
        "#476BA0",
        "#D1DDF9",
        "#DDC9C9",
        "#D89382",
        "#ED0000",
        "#AA0000",
        "#B2ADA3",
        "#68AA63",
        "#1C6330",
        "#B5C98E",
        "#CCBA7C",
        "#E2E2C1",
        "#DBD83D",
        "#AA7028",
        "#BAD8EA",
        "#70A3BA"
      )
    )
    return(nlcd)
  } else if (name == "covariates") {
    covariates <- c(
      "elev" = "blue",
      "imp" = "red",
      "fch" = "green",
      "bf" = "purple",
      "tcc" = "orange"
    )
    return(covariates)
  }
}
