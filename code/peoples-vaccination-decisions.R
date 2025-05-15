# setup -------------------------------------------------------------------
# loads data and fonts freshly, if TRUE
reload <- TRUE
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# ensures `pacman` available, automatizing package management, loads packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  # data and processing
  data.table, 
  # data visualization
  ggplot2, ggthemes, showtext, ggrepel, MetBrewer, ggtext, patchwork,
  # # string manipulation
  glue, stringr
)
# sources functions for grid layout data viz
source("../src/modular-grid-datavisualization-helpers.R")


# load data ---------------------------------------------------------------
if (reload) D <- fread("../data/vaccinations.csv")

# parameter of graphics -----------------------------------------------------
# sets parameters for fonts, colors, color palettes, etc.
par <- list(
  caption = "by Jana B. Jarecki | data: www.drivendata.org/competitions/66/flu-shot-learning",
  fontfamily = "Oswald",
  fontsize = 08,
  regular = 200,
  bold = 400,
  lineheight = 1,
  linewidth = .2,
  palette = "Manual",
  theme = theme_tufte,
  black = "grey5",
  grey = "grey30",
  levels = c("0" = "Not Vaccinated", "1" = "Vaccinated"),
  flulevels = c(
    seasonal_vaccine="Seasonal Flu",
    h1n1_vaccine="H1N1 Swine Flu"),
  colors = NA,
  bg = "#fcfcfc",
  pagesize = c(w = 210, h = 297), # A4 page w x h: 210 x 297 mm
  dpi = 500,
  filename = file.path("../visualizations/","peoples-vaccination-decisions"),
  filetypes = c("png"),
  # this is a hack because geom_text & geom_marquee don't save correctly
  wrap = 30 # wrap lines after `.wrap` number of characters
)
par <- within(par, {
  colors = list(
    "seasonal_vaccine" = setNames(c("#004747", "#00cccc"), levels),
    "h1n1_vaccine" = setNames(c("#610000", "#ff6666"), levels))
  # setNames(met.brewer(palette, n = length(levels)), levels)
  #grey = met.brewer(palette, n = 2)[2]
  col = grey
  family = fontfamily
  linespacing = lineheight
})
#  customizes the fonts
if (reload) font_add_google(par$family, reg=par$regular, bold=par$bold, db_cache=reload)
theme_set(par$theme(base_family = par$family))
showtext_auto()
showtext_opts(dpi = 74) # to display text correctly in R viewer
# customizes the ggtheme
theme_update(
  line = element_line(color = par$black),
  text = element_text(size = par$fontsize, color = par$grey,
                      lineheight=par$lineheight),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  plot.title.position = "plot",
  plot.title = element_text(margin = margin(0,0,0,0), size = rel(1)),
  legend.position = "inside"
)


# prepare the data --------------------------------------------------------
DB <- melt(
  D, id = "respondent_id", m = c("h1n1_vaccine", "seasonal_vaccine"))[
    , .N, by=.(variable, value)
    ]
DB[, p := N / sum(N), by = variable]
N <- length(unique(D$respondent_id))

# the data viz -------------------------------------------------------------------

# plotting functions ------------------------------------------------------

plotCircle <- function(d, var) {
  ggplot(
    DB[variable == var],
    aes(
      x = 2,
      y = N,
      fill = factor(value, labels = par$levels))
  ) +
    geom_col() +
    geom_text(
      aes(
        x = 3,
        label = sprintf("%0.f%%\n%s", p * 100, 
                        factor(value, labels=par$levels)),
        color = factor(value, labels = par$levels),
        hjust = 1 - value
        ),
      family = par$family,
      lineheight = par$lineheight,
      position = position_stack(vjust = 0.3),
      size = par$fontsize / .pt  * 1.35,
    ) +
    ggtitle(factor(var, names(par$flulevels), par$flulevels)) +
    scale_fill_manual("", values = par$colors[[var]]) +
    scale_color_manual("", values = par$colors[[var]]) +
    coord_polar(theta = "y", clip = "off") +
    xlim(c(.7, 3)) +
    theme(axis.text = element_blank(),
          legend.position = "none",
          panel.spacing.x = unit(0, u="pt"),
          title = element_text(size = par$fontsize * 2.5,
                               color = par$black))
}

# helper: binarizes features with > 2 levels
binarize <- function(x) {
  if (is.numeric(x) & length(unique(na.omit(x))) > 2) {
    return(prepOpinionValues(x))
  } else if (length(unique(x)) == 2) {
    return(as.numeric(x))
  } else {
    return(as.numeric(x))
  }
}

# helper: selects the top n discriminating features (simple marginal discrim.)
selectFeatures <- function(D, var, n = 6) {
  relevant_features <- if (grepl("h1n1", var)) {
    names(D)[!grepl("seas", names(D))]
  } else {
    names(D)[!grepl("h1n1", names(D))]
  }
  
  melt(
    D[, lapply(.SD, binarize)][
      ,
      .SD,
      .SDcols = sapply(D, is.numeric)],
    id = c("respondent_id", var)
    )[
      , 
      .(m = mean(value, na.rm=T)),
      by = c(var, "variable")
    ][
      ,
      .(d = abs(diff(m)), m = mean(m)),
      by = .(variable)
    ][
      order(-d)
    ][
      variable %in% relevant_features
    ][
      1:n,
    ][
      order(m),
      as.character(variable)
    ]
}

# helper: binarizes opinion values
prepOpinionValues <- function(x) {
  # 1 = Not at all effective; 2 = Not very effective; 
  # 3 = Don't know; 
  # 4 = Somewhat effective; 5 = Very effective
  return(as.numeric(ifelse(is.na(x), NA, scale(x, 
                                               center=median(x,na.rm=T),
                                               scale=F) >= 0)))
}

plotFeatures <- function(D, var, hjust, yshift = .06) {
  features <- selectFeatures(D, var)
  
  feature_map <- c(
    health_insurance     =        "have health insurance",
    doctor_recc_seasonal =        "recommended by doctor",
    opinion_seas_risk =           "perceive high infection risk",
    opinion_seas_vacc_effective = "believe the vaccine works",
    chronic_med_condition =       "have a chronic illness",
    behavioral_touch_face =       "avoid touching their face",
    behavioral_wash_hands =       "often wash their hands",
    h1n1_concern =                "are concerned about flu",
    health_worker =               "work in health care"
  )
  if (grepl("h1n1", var)) {
    names(feature_map) <- gsub("seasonal", "h1n1", names(feature_map))
    names(feature_map) <- gsub("seas_", "h1n1_", names(feature_map))
  }
  # order feature_map by order in `features`
  feature_map <- feature_map[features]
  # wrap into 2 rows
  feature_map <- setNames(str_wrap(feature_map, 18), names(feature_map))
  
  # aggregated data frame
  
  DA <- copy(D[, .SD, .SDcols = c(features, var)])
  DA[, 
     c(features[grepl("opinion", features)]) := lapply(.SD, prepOpinionValues), 
     .SDcols = features[grepl("opinion", features)]]
  DA <- melt(DA, id = var)[
    !is.na(value), 
    .(N = .N), 
    by = c("variable", "value", var)
  ][
    ,
    .(p = N / sum(N), value = value),
    by = c("variable", var)
  ][
    value == 1
  ]
  
  # prepares variables for the plot
  
  setnames(DA, var, "outcome")
  DA[, variable := factor(variable, names(feature_map), feature_map, 
                          ordered = T)]
  # plot
  barwidth <- yshift * 1.2
  
  p <- ggplot(
    DA,
    aes(
       x = p,
       y = as.numeric(variable) + ifelse(outcome, 1, -1) * yshift)
    ) +
    # bars
    geom_col(
      aes(
        x = p,
        fill = factor(outcome, 0:1, par$levels)
      ),
      width = barwidth,
      orientation = "y"
    ) +
    # 53% text
    geom_text(
      hjust = 1 - hjust,
      aes(
        label = sprintf("%0.f%%", p * 100),
        x = p,
        y = as.numeric(variable) + 
          ifelse(outcome,1,-1) * (barwidth + 2 * yshift),
        color = factor(outcome, 0:1, par$levels)
      ),
      family = par$family,
      size = par$fontsize  / .pt * .66
    ) +
    # variable label text
    geom_text(
      data = DA[order(p)][duplicated(variable)],
      aes(
        label = variable,
        x = p + .02,
        y = as.numeric(variable),
      ),
      vjust = .5,
      hjust = hjust,
      family = par$family,
      size = par$fontsize / .pt,
      color = par$grey,
      lineheight = .98
    ) +
    # scale_x_continuous(limits = c(-yshift, 1 + 2 * yshift)) +
    scale_y_continuous(
      breaks = as.numeric(unique(DA$variable)),
      labels = levels(droplevels(DA$variable)),
      limits = .5 + c(0, length(unique(DA$variable)))) +
    guides(size = "none", color = "none") +
    scale_fill_manual("", values = par$colors[[var]]) +
    scale_color_manual("", values = par$colors[[var]]) +
    coord_cartesian(clip = "off") +
    theme(
      legend.position = "inside",
      legend.direction = "horizontal",
      legend.position.inside = c(.5,1),
      legend.key.spacing = unit(1, "lines"),
      legend.key.size = unit(.5, "lines"),
      axis.text = element_blank(),
      #aspect.ratio = 1.4 / 1,
      legend.margin = margin(b=0, t=1, unit = "lines")
    ) +
    ggtitle("") # empty title for height alignments with other elements
  
  if (hjust == 1) {
    p <- p + scale_x_reverse(limits = c(1.2,0), expand = c(0,0)) +
      theme(legend.position.inside = c(.5,0))
  } else {
    p <- p + scale_x_continuous(limits = c(0,1.2), expand = c(0,0))
  }
  
  return(p)
}



# plot elements -----------------------------------------------------------
title <- GrobModularGrid(
  text = glue("{format(N, big.mark=',')} People's\nVaccination Decisions"), 
  "tl", col = par$black,  fontface = "bold",  cex = 3.3, lineheight = .97, 
  par)

subtitle <- GrobModularGrid(
  text = glue("Household survey on influenza immunization in the 2009/10 flu season (United States of America) by the National Center for Immunization and Respiratory Diseases (NCIRD), the National Center for Health Statistics (NCHS), and the Centers for Disease Control and Prevention (CDC)."), 
  "tl", col = par$grey,  cex = 1, 
  lineheight = unit(1.17, "lines"), wrap = par$wrap, par)

# graphics elements
h1n1 <- plotCircle(DB, "h1n1_vaccine"); h1n1
h1n1_factors <- plotFeatures(D, "h1n1_vaccine", 1); h1n1_factors
seasonal <- plotCircle(DB, "seasonal_vaccine"); seasonal
seasonal_factors <- plotFeatures(D, "seasonal_vaccine", 0); seasonal_factors


# arrange the plot using a rectangular grid
.gridlayout <- "
SAAA
BBCC
BBCC
EEDD
EEDD
"
viz <- list(
  # use `free(x)` to allow grids to overlap
  A = title, #C = subtitle_1, D = subtitle_2, 2
  S = subtitle,
  B = seasonal + theme(aspect.ratio = 1),
  C = seasonal_factors,
  D = h1n1 + theme(aspect.ratio = 1),
  E = h1n1_factors
  ) |>  
  ModularGrid(.gridlayout) +
  plot_annotation(
    caption = par$caption,
    theme = theme(plot.background = element_rect(fill = par$bg))
  ); showtext_opts(dpi = 74); viz


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
