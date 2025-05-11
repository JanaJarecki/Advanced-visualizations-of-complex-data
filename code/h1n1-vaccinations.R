# setup -------------------------------------------------------------------
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# ensures `pacman` is available, which automatizes R library management
if (!require(pacman)) install.packages("pacman")
# loads R libraries, and installs them if needed
pacman::p_load(
  # data and processing
  data.table, 
  # data visualization
  ggplot2, ggthemes, showtext, ggrepel, MetBrewer, ggstar, ggtext, patchwork, marquee, 
  # waffle,
  # # string manipulation
  glue
  # stringr, forcats, lorem,
  # # allows map-data visualization
  # sf, rnaturalearth, rnaturalearthdata
)
# defines the outputs' rectangular grid layout
source("../src/utils_modulargrid.R")
# reloads the data and fonts dynamically
reload <- T


# load data ---------------------------------------------------------------
dtr <- fread("data/training_set_features.csv")
dla <- fread("data/training_set_labels.csv")
D <- dtr[dla, on = "respondent_id"]
rm(dtr, dla)


# graphics parameters -----------------------------------------------------
# sets parameters for fonts, colors, color palettes, etc.
par <- list(
  caption = "by Jana B. Jarecki | data: https://www.drivendata.org/competitions/66/flu-shot-learning.",
  fontfamily = "Gidole",
  fontsize = 9,
  regular = 400,
  bold = 700,
  lineheight = 0.9,
  linewidth = .3,
  palette = "Isfahan1",
  theme = theme_tufte,
  black = "grey10",
  grey = "grey30",
  levels = c("No flu shot", "Flu shot"),
  colors = NA,
  bg = "#F6F6F6",
  pagesize = c(w = 210, h = 297), # A4 page w x h: 210 x 297 mm
  dpi = 500,
  filename = glue("flu-vaccination"),
  filetypes = c("png")
)
par <- within(par, {
  colors = setNames(met.brewer(palette, n = length(levels)), levels)
  #grey = met.brewer(palette, n = 2)[2]
  col = grey
  family = fontfamily
  linespacing = lineheight
})
#  customizes the fonts
font_add_google(par$family, regular.wt=par$regular, bold.wt=par$bold, db_cache=F)
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


# The data viz ---------------------------------------.--------------------
.wrap <- 27 # wrap lines after `.wrap` number of characters
# note: this is a hack because geom_text and geom_marquee don't save correctly

# Plot elements
title <- GrobGriddesign(
  text = glue("Flu Vaccinations"), 
  "br", col = par$black,  fontface = "bold",  cex = 3.7, 
  lineheight = unit(0.8, "lines"), par)
# subtitle_1 <- GrobGriddesign(
#   "Location of the democratic countries across the world", wrap = .wrap, 
#   align = "br", par)
# n <- sum(WY$is_democracy, na.rm=T)
# N <- nrow(WY)
# subtitle_2 <- GrobGriddesign(
#   text = glue("Overall, {n} of {N} ({round(n/N*100)}%) countries were 
#               democracies"), 
#   wrap = .wrap, align = "bl", par)
binarize <- function(x) {
  if (is.numeric(x) & length(unique(na.omit(x))) > 2) {
    return(as.numeric(x >= median(x, na.rm=T)))
  } else if (length(unique(x)) == 2) {
    return(as.numeric(x))
  } else {
    return(x)
  }
}
melt(
  D[, lapply(.SD, binarize)][
  ,
  .SD,
  .SDcols = sapply(D, is.numeric)],
  id = c("respondent_id", "h1n1_vaccine"))[
    , 
    .(m = mean(value, na.rm=T)),
    by = .(h1n1_vaccine, variable)
  ][
    ,
    .(d = diff(m)),
    by = .(variable)
  ][
    order(d)
  ]


vars <- c(
  "Has high-risk Attitude tow. Vaccines" = "opinion_h1n1_risk",
  "Works in Healthcare" = "health_worker",
  "Effectiveness belief" = "opinion_h1n1_vacc_effective")
prep_opinion_val <- function(x) {
  # 1 = Not at all effective; 2 = Not very effective; 
  # 3 = Don't know; 
  # 4 = Somewhat effective; 5 = Very effective
  return(as.numeric(ifelse(is.na(x), NA, scale(x, 
                                               center=median(x,na.rm=T),
                                               scale=F) >= 0)))
  # y <- x
  # y[x <= 2] <- 0
  # y[x >= 4] <- 1
  # y[x == 3] <- 1
  # return(y)
}
DA <- copy(D[, .SD, .SDcols = c(vars, "h1n1_vaccine")])
DA[, 
   c(vars[grepl("opinion", vars)]) := lapply(.SD, prep_opinion_val), 
   .SDcols = vars[grepl("opinion", vars)]]
DA <- melt(D,
     id = c("h1n1_vaccine"), 
     )[
      !is.na(value), 
      .(N = .N), 
      by = .(variable, value, h1n1_vaccine)
    ][
      ,
      .(p = N / sum(N), value = value),
      by = .(variable, h1n1_vaccine)
    ][
      value == 1
    ]
DA[, variable := factor(variable, levels = vars, labels = names(vars))]

ggplot(DA,
       aes(
         x = p,
         y = as.numeric(variable)
       )) +
  geom_segment(
    aes(
      y = as.numeric(variable),
      yend = as.numeric(variable)
    ),
    x = 0,
    xend = 1,
    linewidth = par$linewidth,
    color = par$grey
  ) +
  geom_point(
    data = data.frame(x = rep(seq(0,1,.1), each = length(DA$variable)),
                      y = as.numeric(DA$variable)), 
    aes(
      x = x,
      y = y
    ),
    shape = 124,
    size = par$linewidth * 7,
    color = par$grey
  ) +
  geom_point(
    aes(
      size = p,
      x = p,
      fill = factor(h1n1_vaccine, 0:1, par$levels),
    ),
    color = "white",
    shape = 21,
    stroke = .5
  ) + 
  geom_text(
    aes(
      label = paste(sprintf("%0.f", p * 100), "%"),
      x = p,
      y = as.numeric(DA$variable) + (p / pi)^2 + .2
    ),
    family = par$family,
    size = par$fontsize / 2.4
  ) +
  scale_size(range = c(1,17), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1.05)) +
  scale_y_continuous(breaks = as.numeric(unique(DA$variable)),
                     labels = levels(DA$variable),
                     limits = .5 + c(0, length(unique(DA$variable)))) +
  guides(size = "none") +
  scale_fill_manual("Vaccination", values = par$colors) +
  theme(
    legend.position = "top",
    axis.text.x = element_text()
  )

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
