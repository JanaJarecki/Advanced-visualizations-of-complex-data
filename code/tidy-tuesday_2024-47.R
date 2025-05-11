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
  ggplot2, ggthemes, showtext, ggrepel, MetBrewer, ggstar, ggtext, patchwork,
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
week <- 47; year <- 2024
if (reload) {
  d <- tidytuesdayR::tt_load(year, week = week)
  D <- as.data.table(d$episode_metrics)
}

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
  levels = NULL,
  colors = NA,
  bg = "#F6F6F6",
  pagesize = .pagesize, # A4 page w x h: 210 x 297 mm
  dpi = 500,
  filename = glue("dataviz_tidytuesday_{year}-{week}"),
  filetypes = c("png"),
  # note: this is a hack because geom_text and geom_marquee don't 
  # save correctly
  wrap = 25 # wrap lines after `.wrap` number of characters
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
stopifnot(all(D$question_ratio != D$exclamation_ratio))
D[, q_minus_e := question_ratio - exclamation_ratio]
D[, q_over_e := question_ratio / exclamation_ratio]
AG <- D[, .(n_more_q = .N, 
            m_q_over_e = mean(q_over_e)), 
        by = .(season, more_q = q_minus_e > 0)]
ggplot(AG) +
  geom_vline(xintercept = 1, size = .3, color = "grey") +
  geom_point(aes(y = season,
                 x = ifelse(more_q, 1.1, 0.9),
                 size = n_more_q),
             alpha = .5,
             show.legend = F) +
  facet_wrap(~season, ncol = 1) +
  coord_cartesian(clip = "off") +
  xlim(c(-1,1)*6) +
  scale_size_continuous(limits = c(0,25))

ggplot(D) +
  geom_point(aes(question_ratio, exclamation_ratio))

# Text elements
title <- grob_griddesign(
  text = glue("TITLE"), 
  "bl", col = par$black,  fontface = "bold",  cex = 3.7, 
  lineheight = unit(0.8, "lines"), par)
text1 <- GribGriddesign(
  glue(ipsum_words(10)),
  align = "bl", par, wrap = par$wrap)

# arrange the plot using a rectangular grid
.gridlayout <- "
##TT
##TT
####
####
####
"
viz <- wrap_plots(
  T = title
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

