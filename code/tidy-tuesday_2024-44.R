# setup -------------------------------------------------------------------
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# ensures `pacman` is available, which automatizes R library management
if (!require(pacman)) install.packages("pacman")
# defines the outputs' rectangular grid layout
source("utils_gridlayout.R")
# loads R libraries, and install if needed
pacman::p_load(
  # data_1ta and processing
  tidytuesdayR, data.table, 
  # data_1ta visualization
  ggplot2, ggthemes, showtext, ggrepel, MetBrewer, ggstar, ggtext, patchwork,
  # string manipulation
  stringr, forcats, glue, lorem
  )
# reloads the data_1ta dynamically if TRUE
reload <- TRUE


# load data_1ta ---------------------------------------------------------------
week <- 44; year <- 2024
if (reload) {
  # monster movies
  d <- tidytuesdayR::tt_load(year, week = week)
  # summer movies
  S <- lapply(tt_load(2024, week = 31), as.data.table)
  # holidata_1y movies
  H <- lapply(tt_load(2023, week = 50), as.data.table)
}
# extract monster data_1ta as data_1ta.table
DW <- as.data.table(d$monster_movies)
DL <- as.data.table(d$monster_movie_genres)
# merge DW into DL
DL <- DL[DW[, c("tconst","average_rating","num_votes","year","primary_title")], 
  on = "tconst"]


# graphics parameters -----------------------------------------------------
# sets the fonts, colors, and color fig_1lettes
par <- list(
  caption = "by Jana Jarecki | data: tidytuesday, IMDB, Wikipedia",
  family = "PT Sans",
  regular = 400,
  bold = 700,
  fontsize = 10,
  pagesize = c(210, 297), # A4 page w x h: 210 x 297 mm
  palette = "Wissing",
  theme = theme_tufte,
  black = "grey33",
  grey = "grey66",
  levels = DL[, .(median(average_rating)), by=genres][order(V1)]$genres,
  wrap = 30,
  dpi = 500,
  filename = glue("dataviz_tidytuesday_{year}-{week}"),
  filetypes = c("png")
  )
par <- within(par, {
  colors = setNames(met.brewer(par$palette, n = length(levels)), levels)
  accent = colors["Horror"]
})
font_add_google(par$family, regular.wt=par$regular, bold.wt=par$bold, db_cache=T)
theme_set(par$theme(base_family = par$family))
showtext_auto()
# sets the ggtheme specification
theme_update(
  line = element_line(color = par$black),
  text = element_text(size = par$fontsize, color = par$black),
  plot.title.position = "plot",
  plot.title = element_markdown(
    size = rel(1.25),
    face = "bold",
    hjust = 0,
    lineheight = unit(1.1, u = "lines"),
    margin = margin(b = 1.5, u = "lines")),
  plot.subtitle = element_textbox_simple(
    lineheight = 1.3, 
    color = par$grey,
    margin = margin(b = 2, u = "lines")),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  aspect.ratio = 1.5/1
)
# geom_text for this plot
update_geom_defaults("text", list(
  colour = theme_get()$text$colour,
  family = theme_get()$text$family,
  size = par$fontsize / .pt))
# Style the word monster for titles
MONSTER <- glue("<span style = 'color: {par$accent};'>MONSTER</span>")



# Four plots (A-D) for the data_1ta viz ---------------------------------------

# Plot A ------------------------------------------------------------------
# data_1ta for plot
music_movie_text <- glue(
"<span style='color: {par$colors['Music']};'>
{DL[average_rating == 9 & genres == 'Music']$primary_title}</span><br><br> is 
a 1992 film featuring live performances by rock and heavy metal bands AC/data_3, 
Metallica, The Black Crowes, fig_1ntera, and E.S.T. [ru] in the Tushino Airfield in
Moscow, during the dissolution of the Soviet Union.")
subtitle_text <- glue(
"{length(unique(DW$tconst))} movies with the word 'monster' in the movie 
title and the associated ratings of the movies on IMdata_2 (cinema, TV, and videos). 
The monster-titles belonged to {length(na.omit(unique(DL$genres)))} genres.
<br><br>
Here, we see which genres contained
the two top-rated movies and which genres contained
the two worst-rated movies for different production years of the 
movies. For more recent movies, the ratings seem more 
polarized than for older movies. The top-rated movie is often a 
<span style = 'color: {par$colors['Documentary']};'>Documentary</span>
(top right). And the <span style = 'color: {par$colors['Horror']};'>Horror
</span> genre polarizes, containing the worst-rated and the top-rated movie 
in recent years.")
data_1 <- DL[
  !is.na(genres), 
  year.bin := cut(year, 
    breaks = seq(1915, 2025, 10),
    labels = seq(1915 + 5, 2025 - 5, 10),
    ordered_result = T,
    include.lowest = T,
    dig.lab=10)
  ][,
  by = .(genres, year.bin),
  .(max_average_rating = max(average_rating),
    min_average_rating = min(average_rating)),
  ]
data_1[,
  c("rmax", "rmin") := .(
    rank(-max_average_rating, ties = "random"),
    rank(min_average_rating, ties = "random")),
  by = year.bin
  ][,
  rating := ifelse(rmax <= 2, max_average_rating, min_average_rating)
  ]
# Plot the first main plot
fig_1 <- ggplot(
  data_1[rmax <= 2 | rmin <= 2], 
  aes(x = year.bin,
      y = rating,
      color = genres,
      group = genres)) +
  geom_point(
    size = 6,
    alpha = .2) +
  geom_text_repel(
    aes(label = glue("{sprintf('%.1f', rating)} | {genres} ")),
    family = par$family,
    size = par$fontsize / .pt,
    hjust = 0,
    nudge_x = 0.3,
    direction = "y",
    max.overlaps = 50,
    force = 0.1) +
  geom_segment(
    aes(x = "1990",
        y = average_rating,
        yend = average_rating + 5),
    data = DL[average_rating == 9 & grepl("Music", genres)],
    color = par$colors['Music'],
    size = 0.5
  ) +
  geom_textbox(
    aes(x = "1990",
        label = music_movie_text, 
        y = DL[average_rating == 9 & grepl("Music", genres)]$average_rating),
    width = unit(5.5, "cm"),
    nudge_x = 0.13,
    nudge_y = 5,
    color = par$grey,
    family = par$family,
    size = 0.9 * par$fontsize / .pt,
    fill = NA,
    hjust = 0,
    vjust = 1,
    box.size = 0,
    box.color = NA,
    box.padding = unit(c(0, 0, 0, 3), "pt")) +
  geom_segment(
    x = "1920",
    xend = "2020",
    y = -0.8,
    color = par$black) +
  scale_y_continuous(limits = c(-1,14), expand = c(0,0), breaks = 0:10) +
  scale_x_discrete(expand = expansion(add = 1.5), breaks = seq(1900, 2025, 20)) +
  scale_color_manual(values = par$colors) +
  # Title and description
  labs(
    title = glue("MOVIES WITH THE WORD<br>{MONSTER} IN THE<br>TITLE<br>")
    ) +
  geom_textbox(
    aes(x = "1920",
        label = subtitle_text),
    width = unit(6.5, "cm"),
    y = 9,
    vjust = 0,
    hjust = 0,
    color = par$grey,
    family = par$family,
    size = par$fontsize / .pt,
    fill = NA,
    box.size = 0,
    box.color = NA
    ) +
  theme(
    plot.title = element_markdown(size = rel(3),
                                  margin = margin(b = 0.3, u = "lines")),
    axis.text = element_text(size = rel(1.2)),
    aspect.ratio = 1/1.3,
    legend.position = "none"
    ) +
  coord_cartesian(clip = "off")



# Small second plot for below
data_2 <- DL[
  !is.na(genres),
  .N, 
  by = .(genres = fct_lump_min(genres, 3))
  ][
  order(N),
  genres := fct_inorder(genres)
  ][]
# The plot
fig_2 <- ggplot(
  data_2, 
  aes(y = genres,
      color = genres %in% c("Horror", "Documentary"))) +
  geom_segment(
    aes(x = 0, 
        xend = N), 
    inherit.aes = TRUE) +
  geom_text(
    aes(x = 0, 
        label = genres), 
    nudge_x = -8,
    hjust = 1) +
  geom_text(
    aes(x = N,
        label = N),
    nudge_x = 8,
    hjust = 0) +
  scale_x_continuous(expand = c(0.2,0)) +
  scale_color_manual(values = unname(par$colors[c(15,5)])) +
  theme(
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off")
# description text for plot 2
text_2 <- grob_griddesign(
  glue("MONSTER TITLES' GENRES\n
  'Monster' appears relatively often in 
       {tolower(data_2[order(-N)][1]$genres)} movie titles, but it also often 
       appears in the titles of {tolower(data_2[order(-N)][3,]$genres)} 
       movies, surprisingly"),
  align = "bl", par, wrap = par$wrap)





# Plot for the lower middle column
data_3 <- list(
  Summer = S[[1]][S[[2]][, .(tconst, year)], on = "tconst"],
  Holiday = H[[2]][H[[1]][, .(tconst, year)], on = "tconst"],
  Monster = DL)
data_3 <- rbindlist(data_3, id = "Title with", fill=TRUE)[,
  by = .(year, `Title with`),
  .N][
  !is.na(year)
  ]
# DL |> group_by(year) |> summarise(N = n())
fig_3 <- ggplot(
  data_3[year < 2024], 
  aes(x = year, 
      y = N)) +
  geom_line(
    aes(color = `Title with`)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(1920,2020,25), 
                     limits = c(1919, 2023)) +
  scale_y_continuous(expand = c(0,0), 
                     position = "right", 
                     breaks = seq(0,400,50)) +
  scale_color_manual(values = unname(par$colors[c(20,5,13)])) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2,0.8))
# description text for plot 3
text_3 <- grob_griddesign(
  glue("MONSTER TITLES' GROWTH\n
  Increasingly, recent movie titles contain 'monster', but
       'holiday' appears much more often in movie titles"),
  align = "bl", par, wrap = par$wrap)


# Plot for the lower right column
data_4 <- DL[
  !is.na(genres)
  ][,
  genres := fct_rev(factor(genres, levels = names(par$colors)))]
fig_4 <- ggplot(
  data_4, 
  aes(x = genres, 
      y = average_rating, 
      group = genres)) +
  geom_hline(
    yintercept = 6,
    color = "grey80",
    size = 0.5) +
  geom_jitter(
    aes(size = num_votes,
        color = genres),
    width = .25, 
    alpha = .05) +
  geom_tufteboxplot(
    aes(x = genres, 
        y = average_rating, 
        group = genres,
        color = genres), 
    size = 0.5) +
  geom_point(
    aes(color = genres),
    stat = "summary",
    fun = median,
    size = 1
    ) +
  geom_text(
    data = data_4[, .(y = quantile(average_rating, 0.5)), by = genres],
    aes(y = y, 
        label = genres),
    nudge_y = 4,
    nudge_x = 0,
    angle = 90,
    hjust = 0,
    color = par$black,
    size = par$fontsize / .pt * .9
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y.left = element_text(margin = margin(r = 3, u = "lines"),
                                    hjust = 0),
    aspect.ratio = 2/1
    ) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(-2,14), 
                     breaks = c(0,10),
                     minor_breaks = seq(0,10,1), 
                     expand = c(0,0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  scale_color_manual(values = par$colors) +
  coord_radial(start = -0.01 * pi, 
               end = 1 * pi,
               rotate.angle = TRUE,
               clip = "off")
# description text for plot 4
text_4 <- grob_griddesign(
  glue("MONSTER TITLES' RATING\n
  The top-rated movie genres are music, biographies, short films, 
  and documentaries (median rating)"),
  align = "bl", par, wrap = par$wrap)

# Add the plots together
grid_layout <-"
AAAA
AAAA
AAAA
BCD#
EFG#"

showtext_opts(dpi = 72)
viz <- wrap_plots(
  A=fig_1 + theme(plot.title.position = "panel"),
  B = text_2,
  C = text_3,
  D = text_4,
  E = fig_2,
  F = fig_3, 
  G = fig_4 + theme(plot.title = element_markdown(
    margin = margin(b=1, u="lines")))
  ) |> 
  PatchworkGrid(grid = grid_layout) +
  plot_annotation(caption = par$caption)


# save data viz -----------------------------------------------------------
# set dpi, otherwise showtext renders fonts too big
showtext_opts(dpi = par$dpi)
sapply(par$filetypes, function(filetype)
  ggsave(glue("{par$filename}.{filetype}"),
         plot = viz,
         width = par$pagesize["w"], 
         height = par$pagesize["h"], 
         units = "mm",
         dpi = par$dpi
  ))
# check the resulting visualization
system2("open", glue("{par$filename}.{par$filetypes[1]}"))
# reset the dpi
showtext_opts(dpi = 72)
