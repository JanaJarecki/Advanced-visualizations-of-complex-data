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
  stringr, forcats, glue, lorem,
  # specifically for this project: map-data visualization
  sf, rnaturalearth, rnaturalearthdata
)
# defines the outputs' rectangular grid layout
source("utils_gridlayout.R")
# reloads the data and fonts dynamically
reload <- T


# load data ---------------------------------------------------------------
week <- 45; year <- 2024
if (reload) {
  d <- tidytuesdayR::tt_load(year, week = week) # monster movies
  D <- as.data.table(d$democracy_data)
}
data_year <- 2020


# graphics parameters -----------------------------------------------------
# sets parameters for fonts, colors, color palettes, etc.
par <- list(
  caption = "by Jana B. Jarecki | data: tidytuesday",
  fontfamily = "Cormorant",
  fontsize = 9,
  regular = 400,
  bold = 700,
  lineheight = 0.9,
  palette = "Demuth",
  theme = theme_tufte,
  black = "grey10",
  grey = NA,
  levels = c("No Democracy", "Democracy"),
  colors = NA,
  bg = "#F6F6F6",
  pagesize = c(w = 210, h = 297), # A4 page w x h: 210 x 297 mm
  dpi = 500,
  filename = glue("dataviz_tidytuesday_{year}-{week}"),
  filetypes = c("png")
)
par <- within(par, {
  colors = setNames(met.brewer(palette, n = length(levels)), levels)
  grey = met.brewer(palette, n = 2)[2]
  col = grey
  family = fontfamily
  linespacing = lineheight
})
#  customizes the fonts
font_add_google(par$family, regular.wt=par$regular, bold.wt=par$bold, db_cache=T)
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


# Prepare the data --------------------------------------------------------
D[, is_female := is_female_president | is_female_monarch]
D[, is_female := ifelse(is.na(is_female), F, is_female)]
# basic world-map data to plot W
W <- ne_countries(returnclass = "sf",scale = 50)
# generate an id adm0_a3_us that matches the adm0_a3_us column in `W`
D[, "adm0_a3_us" := fcase(
  country_code == "ZAR", "COD",
  country_code == "GER", "DEU",
  country_code == "GIB", NA_character_,
  country_code == "NUR", "NRU",
  country_code == "ROM", "ROU",
  country_code == "SSD", "SDS"
)]
D[, adm0_a3_us := fifelse(is.na(adm0_a3_us), country_code, adm0_a3_us)]
# merge D into W
W <- merge(W, D, on = "adm0_a3_us")
WY <- W[W$year == data_year, ]


# The data viz ---------------------------------------.--------------------
.wrap <- 27 # wrap lines after `.wrap` number of characters
# note: this is a hack because geom_text and geom_marquee don't save correctly

# Plot elements
title <- GrobGriddesign(
  text = glue("Democracies\n as of {data_year}"), 
  "br", col = par$black,  fontface = "bold",  cex = 3.7, 
  lineheight = unit(0.8, "lines"), par)
subtitle_1 <- GrobGriddesign(
  "Location of the democratic countries across the world", wrap = .wrap, 
  align = "br", par)
n <- sum(WY$is_democracy, na.rm=T)
N <- nrow(WY)
subtitle_2 <- GrobGriddesign(
  text = glue("Overall, {n} of {N} ({round(n/N*100)}%) countries were 
              democracies"), 
  wrap = .wrap, align = "bl", par)
# transformation rotating the world map coordinates
trans <- "+proj=laea +lat_0=-30 +lon_0=-15 +x_0=43210000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plots hte basic map
content_map <- WY[!is.na(WY$is_democracy), ]  |>  
  st_transform(trans) %>%
  ggplot()+
  geom_sf(
    aes(fill = c("No Democracy","Democracy")[is_democracy+1]),
    color = "white",
    linewidth = 0)+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values =par$colors, name = NULL) +
  theme(legend.justification = c(1,0),
        legend.text.position = "left",
        legend.key.width = unit(0.7, "lines"),
        legend.key.height = unit(0.7, "lines"),
        legend.margin = margin(0, unit = "lines"),
        axis.text = element_blank()); content_map

# highlight selected countries in the worldmap
AddHighlight <- function(plot, var, WY) {
  WY$tmp <- WY[[var]]
  plot + 
    geom_sf(
      data = WY,
      aes(
        alpha = ifelse(tmp == TRUE, 0, .88)
      ),
      fill = "white", color = "white", linewidth = 0) +
    geom_sf(
      data = WY[WY[[var]] == TRUE, ],
      aes(
        color = c("No Democracy","Democracy")[is_democracy+1]
      ), 
      fill = NA, alpha = 0, linewidth = 0.5) +
    scale_alpha_identity() + 
    scale_color_manual(values = par$colors) +
    scale_y_continuous(expand = c(.3,0)) +
    theme(
      legend.position  = "none",
      plot.title = element_text(
        hjust = .5, vjust = 1, margin = margin(b = 5, unit = "lines")
      )
    )
}

female_map <- content_map |> AddHighlight("is_female", WY)
monarchy_map <- content_map |> AddHighlight("is_monarchy", WY)
fairelection_map <- content_map |> AddHighlight("has_free_and_fair_election", WY)
text3 <- GrobGriddesign(
  text = glue("Elections in non-democratic countries exist: Among the {N-n} 
  non-democratic countries, 
  {sum(WY$has_free_and_fair_election & !WY$is_democracy, na.rm=T)} held elections 
  which international observers considered as free and fair elections, or at least as free 
  from major fraud. Among the democratic countries, all
  {sum(WY$has_free_and_fair_election & WY$is_democracy, na.rm=T)} of 
  {sum(WY$is_democracy, na.rm=T)} elections were considered free and fair."),
  wrap = .wrap, align = "tl", par)


# arrange the plot using a rectangular grid
.gridlayout <- "
AACD
#III
#III
#III
JKLT
"
viz <- wrap_plots(
  A = title, C = subtitle_1, D = subtitle_2, 
  I = free(content_map),
  J = free(female_map),
  K = free(monarchy_map),
  L = free(fairelection_map),
  T = text3
  ) |>  
  PatchworkGrid(.gridlayout) +
  plot_annotation(
    caption = par$caption,
    theme = theme(plot.background = element_rect(fill = par$bg))
  )
showtext_opts(dpi = 74); #viz


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
