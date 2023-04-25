#library(readxl)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(ggpubr)
library(graphics)
library(grid)
library(rgeos)
library(rworldmap)
# library(plotly)
#extra ggplot function

#reading the datasets

country_pairs <- read.table("data/country_pairs.csv", sep=",", header=TRUE)
unique_countries <- read.table("data/unique_countries.csv", sep=",", header=TRUE)
unique_countries_first <- read.table("data/unique_countries_first.csv", sep=",", header=TRUE)
unique_countries_co <- read.table("data/unique_countries_co.csv", sep=",", header=TRUE)
unique_countries_merged <- read.table("data/unique_countries_merged.csv", sep=",", header=TRUE)

# Here I create vectors of the African and Overseas countries
overseas_countries <- unname(unlist(filter(unique_countries, origin == "O")["country"]))
african_countries <- unname(unlist(filter(unique_countries, origin == "A")["country"]))

# copied from ggplot2 `geom_curve`
geom_curve2 <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        curvature = 0.5,
                        angle = 90,
                        ncp = 5,
                        arrow = NULL,
                        lineend = "butt",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve2, # call `GeomCurve2` instead of `GeomCurve`
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

# copied from ggplot2 `GeomCurve`
GeomCurve2 <- ggproto("GeomCurve2", GeomSegment,
                      # the following `default_aes =` statement is missing in ggplot2 `GeomCurve`
                      default_aes = aes(colour = "black", fill = "black", size = 0.5, linetype = 1, alpha = NA),
                      draw_panel = function(data, panel_params, coord, curvature = 0.5, angle = 90,
                                            ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE) {
                        if (!coord$is_linear()) {
                          warning("geom_curve is not implemented for non-linear coordinates",
                                  call. = FALSE)
                        }
                        trans <- coord$transform(data, panel_params)
                        
                        curveGrob(
                          trans$x, trans$y, trans$xend, trans$yend,
                          default.units = "native",
                          curvature = curvature, angle = angle, ncp = ncp,
                          square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
                          gp = gpar(
                            col = alpha(trans$colour, trans$alpha),
                            # the following `fill = ` statement is missing in ggplot2 `GeomCurve`
                            fill = alpha(trans$fill, trans$alpha),
                            lwd = trans$size * .pt,
                            lty = trans$linetype,
                            lineend = lineend),
                          arrow = arrow
                        )
                      }
)