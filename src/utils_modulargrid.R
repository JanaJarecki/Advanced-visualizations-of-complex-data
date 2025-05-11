# Utility Functions for Grid Layout ---------------------------------------
# Helper functions to produce a grid layout
# Produces: Functions
# Author: Jana B Jarecki
# -------------------------------------------------------------------------


#' Write a text into a modular grid cell
#'
#' @param text A string
#' @param align A two-character indicator with `"bl"` = bottom-left, `"br"` =bottom-right, `"tl"` = top-left, and `"tr"` =top-right
#' @param ... optional arguments
#' @param wrap A numeric value with the number of characters after which to wrap lines in the cell
#'
#' @return
#' @export
#'
#' @examples
GrobModularGrid <- function(text, align = c("bl", "br", "tl", "tr"), 
                           ..., wrap = FALSE) {
  align <- match.arg(align)
  if (wrap != FALSE) {
    text <- stringr::str_wrap(text, width = wrap)
  }
  
  # graphics parameter
  par <- unlist(list(...))
  # ensure we give any supplied first parameter arguments priority
  par <- par[unique(names(par))]
  
  # Gets alignment of text, either 0 or 1
  halign <- as.numeric(grepl("r", align))
  valign <- as.numeric(grepl("t", align))
  
  out <- grid::grid.text(
    text,
    x = unit(halign, "npc"),
    y = unit(valign, "npc"), 
    hjust = halign,
    vjust = valign,
    just = ifelse(halign==0, "left", "right"),
    draw = FALSE,
    gp = do.call(grid::gpar, args = par)
  )
  return(patchwork::wrap_elements(out, clip = FALSE))
}

#' Layout Patchwork Plots following a Modular Grid
#' 
#' A modular grid has fields in regular rows and columns, and a gutter between fields, and is on a page with a certain page size that has a page margin.
#' 
#' @param patch a patchwork object, that is ggplot objects arranged in a patchwork
#' @param design A string with a design, see `design` in `grid_layout()`
#' @param fieldsize (optional) A two-element named numeric vector with the width and height of one individual module in the grid, `c(w = #, h = #)` for width and height
#' @param pagesize (optional) A two-element named numeric vector with the width and height dimensions, like `c(w = #, h = #)` for width and height
#' @param guttersize  (optional) A two-element named numeric vector with the size of the gutter (i.e. spacing between fields) in width and height direction, like `c(w = #, h = #)`
#' @param pagemargins (optional) A two-element named numeric vector with the page margins, will usually be automatically calculated from the field size, page size, and gutter size.
#' @param unit (optional) Default units of dimensions. Defaults to "mm". 
#' TODO: Change this to "pt" so it can be most easily scaled with the text.
#' @param par (optional) graphics parameters like bg for background colors
#'
#' @return The plot in the grid layout
#'
#' @examples
ModularGrid <- function(patch, design, pagesize = NULL, guttersize = NULL,
                        pagemargins = NULL, fieldsize = NULL, unit = "mm", 
                        par = NULL) {
  
  # Set default values for the page, field, and gutter size
  if (!length(pagesize)) pagesize <- .DefaultGridsizes("pagesize")
  if (!length(fieldsize)) fieldsize <- .DefaultGridsizes("fieldsize")
  if (!length(guttersize)) guttersize <- .DefaultGridsizes("guttersize")
  if (!length(pagemargins)) pagemargins <- .CalculatePagemargins(design, pagesize, fieldsize, guttersize)

  # arrange the patchwork
  out <- patch + 
    plot_layout(
      design = grid, 
      widths = (fieldsize["w"] + guttersize)["w"],
      heights = (fieldsize["h"] + guttersize)["h"]) &
    theme(
      plot.margin = margin(
        b = guttersize["h"], 
        r = guttesizer["w"], 
        unit = unit)
    )
  
  out +
    plot_annotation(
      theme = theme(
        plot.margin = margin(
          r = pagemargins["r"],
          l = pagemargins["l"],
          t = pagemargins["t"] * 0.9,
          b = pagemargins["b"] * 1.1, 
          unit = unit)
      ))
}

# (internal function)
# default values for the sizes in the modular grid
# namely the field's size, the gutter's size, or the page's size
# the default is a 20-field graphic design layout gold standard form the graphic design literature
.DefaultGridsizes <- function(type) {
  type <- match.arg(type, c("fieldsize", "guttersize", "pagesize"))
  # grids are often in ciceros, but we need them in mm
  # ciceros to mm
  .cicero = 4.51165812456 # mm
  # 1 points to mm,
  .point = 0.35145980 # mm^
  
  # single module/field/box, w x h:
  fieldsize <- c(w = 8 * .cicero + 10 * .point, 
                 h = 9 * .cicero + 10 * .point) #mm
  # dimensions of the empty gutter between fields in height and width direction
  guttersize <- c(w = 16 * .point, h = 14 * .point) #mm
  # dimensions of the whole page, default is A4
  pagesize <- c(w = 210, h = 297) #mm
  
  if (type == "fieldsize") {
    return(fieldsize)
  } else if (type == "guttersize") {
    return(guttersize)
  } else if (type == "pagesize") {
    return(pagesize)
  } else {
    stop(sprintf("Argument `type` must be 'fieldsize', 'guttersize', or 'pagesize', not '%s'", type))
  }
  
}

# (internal function)¨
# caldulates the margins of the page for this design in mm
.CalculatePagemargins <- function(design, pagesize, fieldsize, guttersize) {

  contentarea <- .CalculateContentarea(design, fieldsize, guttersize)
  
  # the page margins in mm
  pagemargins <- 
    c(r = (pagesize - contentarea)[["w"]] / 2 - guttersize[["w"]],
      l = (pagesize - contentarea)[["w"]] / 2,
      t = (pagesize - contentarea)[["h"]] / 2,
      b = (pagesize - contentarea)[["h"]] / 2 - guttersize[["h"]]
    )
  return(pagemargins)
}

# (internal function)
# Calculates the size of the content area given a design
.CalculateContentarea <- function(design, fieldsize = NULL, guttersize = NULL) {
  design_dim <- dim(t(.DesignToMatrix(design)))
  if (!length(fieldsize)) {
    fieldsize <- .DefaultGridsizes("fieldsize")
  }
  if (!length(guttersize)) {
    guttersize <- .DefaultGridsizes("guttersize")
  }
  contentarea <- c(
    design_dim[1] * (fieldsize + guttersize)["w"] - guttersize["w"],
    design_dim[2] * (fieldsize + guttersize)["h"] - guttersize["h"]
  )
  return(contentarea)
}


# (helper function)
# Get the dimensions of the grid x
.DesignToMatrix <- function(x) {
  x <- trimws(x)
  x <- gsub("^\n|\n$", "", x)
  x <- strsplit(x, "\n")[[1]]
  .nrow <- length(x)
  out <- unlist(strsplit(x, ""))
  out <- matrix(out, nrow = .nrow, byrow = TRUE)
  return(out)
}


# 
# # the below currently do not work
# geom_marquee_griddesign <- function(label, align = c("bl", "tl", "br", "tr"), ..., par) {
#   # Sets default arguments
#   if (missing(par)) par <- list(family = "sans", linespacing = 1.2)
#   align <- match.arg(align)
#   
#   # Gets alignment of text, either 0 or 1
#   .y <- as.numeric(grepl("t", align))
#   align <- ifelse(grepl("l", align), "left", "right")
#   
#   geom_marquee(
#     x = 0, y = .y, label = label, 
#     width = unit(1, unit = "npc"),
#     stat = "identity",
#     position = "identity",
#     size.unit = "mm",
#     family = par$family,
#     lineheight = par$linespacing,
#     style = classic_style(align = align, margin = trbl(0,0,0,0)),
#     fill = NA,
#     hjust = 0,
#     vjust = .y,
#     na.rm = FALSE,
#     show.legend = NA,
#     inherit.aes = TRUE
#   )
# }
# 
# 
# make_grid_textarea <- function(text, align = c("bl", "tl", "br", "tr"), par, ...) {
#   ggplot(data.frame(NA)) +
#     geom_marquee_grid(label = text, par = par, align = align, ...) +
#     scale_x_continuous(expand=c(0,0), limits = c(0,1)) +
#     scale_y_continuous(expand=c(0,0), limits = c(0,1)) +
#     theme_void() +
#     coord_cartesian(clip = "off")
# }