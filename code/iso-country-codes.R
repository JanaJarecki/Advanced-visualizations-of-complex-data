# setup -------------------------------------------------------------------
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# ensures `pacman` is available, which automatizes R library management
if (!require(pacman)) install.packages("pacman")
# loads R libraries, and install if needed
pacman::p_load(
  # data and processing
  tidytuesdayR, data.table, 
  # data visualization
  ggplot2, ggthemes, showtext, ggrepel, MetBrewer, ggstar, ggtext, patchwork, marquee,
  # string manipulation
  stringr, forcats, glue, lorem
  # specifically for this project: map-data visualization
  # sf, rnaturalearth, rnaturalearthdata
)
# defines the outputs' rectangular grid layout
source("utils_gridlayout.R")
# reloads the data and fonts dynamically
reload <- F

# load data ---------------------------------------------------------------
week <- 46; year <- 2024
if (reload) {
  d <- tidytuesdayR::tt_load(year, week = week) # monster movies
  D <- as.data.table(d$countries)
}
D[, n_equal := 4]
# counts the number of characters in the iso code that are equal to the 
# first letters in the country name
for (n in 1:3) {
 D[, n_equal := fifelse(
   substr(alpha_3, 1, n) == substr(toupper(name), 1, n), 
   n, 
   as.integer(n_equal))]
}
# makes helper variables for the plot
D[, name := trimws(name)]
D[, label := toupper(name)]
D[, x := 1:.N]
# Gets the frequencies and proportions
N <- nrow(D)
ns <- table(D$n_equal)
prs <- proportions(ns)


# graphics parameters -----------------------------------------------------
# sets parameters for fonts, colors, color palettes, etc.
par <- list(
  caption = "by Jana B. Jarecki | data: tidytuesday",
  fontfamily = "Lekton", #Space Grotesk",
  fontsize = 9,
  regular = 400,
  bold = 700,
  lineheight = 0.9,
  palette = "Navajo",
  theme = theme_tufte,
  black = "grey10",
  grey = "grey20",
  levels = 1:4,
  colors = NA,
  bg = "#F6F6F6",
  pagesize = .pagesize, # A4 page w x h: 210 x 297 mm
  dpi = 500,
  filename = glue("dataviz_tidytuesday_{year}-{week}"),
  filetypes = c("png")
)
par <- within(par, {
  colors = setNames(met.brewer(palette, n = length(levels)), levels)
  col = grey
  family = fontfamily
  linespacing = lineheight
})
#  customizes the fonts
font_add_google(par$family, regular.wt=par$regular, bold.wt=par$bold,
                db_cache=T)
theme_set(par$theme(base_family = par$family))
showtext_auto()
# customizes the ggtheme
theme_update(
  line = element_line(color = par$black),
  text = element_text(size = par$fontsize, color = par$grey),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  plot.title.position = "plot",
  plot.title = element_text(margin = margin(0,0,0,0), size = rel(1)),
  legend.position = "inside"
)


# The data viz ---------------------------------------.--------------------
# note: this is a hack because geom_text and geom_marquee don't save correctly
.wrap <- 25 # wrap lines after `.wrap` number of characters

# generic geom for texts
iso_geom <- function(label, color, n, end = pi, nudge_y = 0, hjust = 0) {
  geom_text(
    aes(label = {{ label }}, color = {{ color }}), 
    angle =  90,
    hjust = hjust,
    vjust = 0,
    #color = "grey40",
    family = par$family,
    size = par$fontsize / .pt * .4)
}

# Make four plots for 1, 2, 3 and 'no' identical first characters
# between the iso abbreviation and the full country node
P <- list()
for (n in unique(D$n_equal)) {
  dd <- D[n_equal == n][order(n_equal)]
  P[[n]] <- ggplot(dd, aes(
    x = seq_along(label),
    y = 1)
  ) + 
    geom_col(
      aes(fill = as.character(n_equal)),
      show.legend = F,
      width = 1,
      alpha = .3,
      linewidth = 0,
      color = NA,
      just = 0.5
    ) +
    iso_geom(label = alpha_3, color = as.character(n_equal), 
             n = nrow(dd), hjust = 1) +
    iso_geom(label = label, color = "grey40", n = nrow(dd), 
             nudge_y = unit(1, "lines")) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_color_manual(values = par$colors) +
    scale_fill_manual(values = par$colors) +
    theme(axis.text = element_blank()) +
    guides(color = "none", fill = "none") +
    coord_radial(rotate.angle = TRUE, expand = FALSE) +
    geom_text(
      x = ifelse(n != 3, 0, nrow(dd)*.6),
      y = ifelse(n != 3, 0, .55),
      angle = ifelse( n != 3, 0, 90 * .6),
      label = paste0(round(pr[n]*100), "%"),
      color = par$bg,
      size = c(15,6,23,6)[n],
      fontface = "bold")
}
# For the third plot change it to span only a quarter of the full circle
P[[3]] <- P[[3]] + coord_radial(
  start = pi, end = 1.5 * pi, rotate.angle = T, expand=F) + ylim(0,NA)

# Text elements
title <- grob_griddesign(
  text = glue("ISO\nCOUNTRY\nCODES"), 
  "bl", col = par$black,  fontface = "bold",  cex = 3.7, 
  lineheight = unit(0.8, "lines"), par)
text1 <- grob_griddesign(
  glue("{round(pr[1]*100)} % of three-letter country codes 
       begin with the same one letter as the country name ({ns[1]} of {N})"),
  align = "bl", par, wrap = .wrap)
text2 <- grob_griddesign(
  text = glue("{round(pr[2]*100)} % of three-letter country codes 
       start with the same two letters as the country name ({ns[2]} of {N})"),
  align = "bl", par, wrap = .wrap)
text3 <- grob_griddesign(
  text = glue("{round(pr[3]*100)} % of the three-letter country codes 
       equal the first three letters of the country name ({ns[3]} of {N})"), 
  align = "bl", par, wrap = .wrap)
text4 <- grob_griddesign(
  text = glue("{round(pr[4]*100)} % of three-letter country codes 
       do not start with any first letter in the country name ({ns[4]} of {N})"), 
  align = "bl", par, wrap = .wrap)
subtitle <- grob_griddesign(
  text = "Are the three letters in short country codes actually abbreviations of the full country names? (short country codes as per ISO norm 3166)", 
  align = "tl", par, wrap = .wrap)


# arrange the plot using a rectangular grid
.gridlayout <- "
I#AA
J#AA
BBD#
BBC#
EFGH
"
viz <- wrap_plots(
  #A = title, C = text1, D = text2, 
  A=P[[3]],
  B=P[[1]],
  C=P[[4]],
  D=P[[2]],
  E=text1,
  F=text3,
  G=text2,
  H=text4,
  I=title,
  J=subtitle
  ) |>  
  patchwork_griddesign(.gridlayout) +
  plot_annotation(
    caption = par$caption,
    theme = theme(plot.background = element_rect(fill = par$bg, 
                                                 color = "black"))
  )
showtext_opts(dpi = 74); viz




# save the data visualization
par$dpi <- 500
showtext_opts(dpi = par$dpi)
sapply(par$filetypes, function(i)
  ggsave(plot = viz,
         glue("{par$filename}.{i}"), 
         width = par$pagesize["w"], h = par$pagesize["h"], unit = "mm",
         scale = 1, dpi = par$dpi
         ))
# check the resulting visualization
system2("open", glue("{par$filename}.{par$filetypes[1]}"))

