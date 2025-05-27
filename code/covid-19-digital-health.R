# setup -------------------------------------------------------------------
# loads data and fonts freshly, if TRUE
reload <- T
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# sources functions for grid layout data viz and loads packages
source("../src/modular-grid-datavisualization-helpers.R")
pacman::p_load(
  # data and processing
  data.table, 
  # data visualization
  ggplot2, ggthemes, showtext, ggrepel, ggtext, ggdist, patchwork, ggridges,
  # Bayesian model prediction
  tidybayes, bayesplot, bayestestR, brms,
  # else
  glue
)

# parameter of graphics -----------------------------------------------------
# sets parameters for fonts, colors, color palettes, etc.
par <- list()
par <- base::within(par, {
  caption = "by Jana B. Jarecki | data: Albrecht, Jarecki, Meier, & Rieskamp (2021)"
  fontfamily = "Noto Sans"
  fontsize = 09 # px
  fontmult = 1.4 # scaling +/- for titles relative to fontsize
  lineheight = 1.3
  regular = 200
  bold = 700
  linewidth = .4
  pointsize = .9
  theme = theme_tufte
  black = "grey5"
  grey = "grey30"
  salient = "#00BB77"
  colors = NA
  palette = "Manual"
  bg = "#F8F8F8" ##fcfcfc"
  pagesize = c(w = 210, h = 297) # A4 page w x h: 210 x 297 mm
  dpi = 500
  filename = file.path("../visualizations/", "covid-19-digital-health")
  filetypes = c("png")
  # this is a hack because geom_text & geom_marquee don't save correctly
  wrap = 27 # wrap lines after `.wrap` number of characters
  # for passing arguments to functions
  col = grey
  family = fontfamily
  linespacing = lineheight
})
#  customizes the fonts
if (reload) {
  font_add_google(par$family, reg=par$regular, bold=par$bold, db_cache=!reload)
}
theme_set(par$theme(base_family = par$family))
showtext_auto()
# customizes the ggtheme
theme_update(
  rect = element_rect(color = "transparent"),
  line = element_line(color = par$black),
  text = element_text(size = par$fontsize,
                      color = par$grey,
                      lineheight = par$lineheight,
                      family = par$family),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  plot.title.position = "plot",
  plot.title = element_text(margin = margin(0,0,0,0), 
                            size = rel(par$fontmult)),
  legend.position = "inside"
)



# load data ---------------------------------------------------------------
if (reload) {
  model_path <- "https://raw.githubusercontent.com/JanaJarecki/Risk-preferences-and-risk-perception-affect-the-acceptance-of-digital-contact-tracing/b3f29ec7e065a5854b9ea276cee353a3a368bfa9/analyses/code/fitted_models/accept_index_fit_reduced_no_social.rds"
  # reads fitted Bayesian model, after variable selection & data
  download.file(model_path, "../data/covid-19-digital-health-bayesian-model.rds", method = "curl")
  fit_reduced_no_social <- readRDS("../data/covid-19-digital-health-bayesian-model.rds")
  
  data_path <- "https://raw.githubusercontent.com/JanaJarecki/Risk-preferences-and-risk-perception-affect-the-acceptance-of-digital-contact-tracing/master/data/processed/data.csv"
  df <- fread(data_path)
}

# get draws from posterior coefficient distribution to plot their density
draws <- fit_reduced_no_social |> 
  gather_draws(
    b_perc_risk_health, b_perc_risk_data, b_perc_risk_econ, 
    b_seek_risk_general, b_seek_risk_health, b_seek_risk_data, 
    b_seek_risk_econ, b_compreh_score, b_policy_score, b_tech_score) |> 
  as.data.table()
# summarize the draws
draws_summary <- draws |> 
  dplyr::group_by(.variable) |> 
  dplyr::summarise(median_hdi(.value)) |> 
  as.data.table()
ylevels <- draws_summary[order(draws_summary$y), ][[".variable"]]
# add the summary to the individual draws
draws <- draws[draws_summary[, .(y, .variable)], on = ".variable"]


# Prepare Data ----------------------------------------------------------------
df[, f_female := factor(female, 0:2, labels = c("Men", "Women", "NA"))]
df[, f_lostwork := has_work == 1 & had_work != has_work]
df[, f_lostwork := factor(f_lostwork, c(F,T), labels = c("Works", "Lost Work"))]
df[, f_infected := was_infected | is_infected]
df[, f_infected := factor(f_infected, c(F,T), labels = c("Never Sick", "Was/Is Sick"))]


# Plots -------------------------------------------------------------------
# Define labels for the predictors
ylabels <- c(
  b_perc_risk_health =  "Perceiving Covid-19\nas Risk for Health",
  b_perc_risk_data =    "Perceiving the Apps\nas Risk for Data Security",
  b_perc_risk_econ =    "Perceiving Covid-19\nas Risk for the Economy",
  b_seek_risk_general = "Seeking Risks\nin General",
  b_seek_risk_health =  "Seeking Risks\nfor Health",
  b_seek_risk_data =    "Seeking Risks\nfor Data Security",
  b_seek_risk_econ =    "Seeking Risks\nfor Finances",
  b_compreh_score =     "Understanding\nthe Digital Technology",
  b_policy_score =      "Supporting\nGovernment Covid-19 Policies",
  b_tech_score =        "Interest in \nNew Digital Technologies"
)

# Make the Bayesian posterior estimate plots
posterior_plot <- ggplot(
  draws,
  aes(
    y = factor(
      .variable, 
      levels = ylevels, 
      labels = ylabels[ylevels]),
    x = .value)) +
  geom_segment(
    x = 0,
    xend = 0,
    y = 0,
    yend = Inf,
    color = par$grey,
    linewidth = par$linewidth / 3,
    alpha = 0.60) +
  geom_density_ridges(
    aes(
      fill = abs(y)),
    alpha = .30, #.20,
    linewidth = 0,
    scale = .8, #1.4, # scaling factor for top of ridge
    panel_scaling = FALSE, # scale globally, rather than per y-panel
    rel_min_height = .005) + # cuts off drawing of long tails
  geom_errorbar(
    aes(
      x = y,
      xmin = ymin,
      xmax = ymax,
      color = abs(y)),
    data = draws_summary,
    linewidth = par$linewidth,
    width = .16) +
  geom_point(
    aes(
      x = y,
      color = abs(y)),
    data = draws_summary,
    size = 3,
    shape = 21,
    fill = "white",
    stroke = par$linewidth * 1.4
    ) +
  geom_point(
    aes(
      x = y,
      color = abs(y)),
    data = draws_summary,
    size = par$pointsize
  ) +
  # the arrows
  geom_segment(
    data = data.frame(
      x = c(-1,1) * .03,
      xend = c(-1,1) * .25,
      y = 10.45),
    aes(
      x = x, 
      xend = xend,
      y = y,
      yend = y),
      arrow = arrow(length = unit(.06, "cm")),
    color = par$salient,
    linewidth = par$linewidth / 3,
    inherit.aes = F) +
  # the arrow texts
  geom_text(
    data = data.frame(
      x = c(-1,1) * .03,
      y = 10.45,
      l = paste(c("reduces", "increases"), "acceptance"),
      hjust = c(1,0)),
    aes(
      x = x,
      y = y,
      label = l,
      hjust = hjust),
    nudge_y = .21,
    color = par$salient,
    family = par$family,
    size = par$fontsize / .pt / par$fontmult * .8
  ) +
  scale_x_continuous(
    "Posterior Coefficient Estimates (Median, HDI, Distribution)",
    limits = c(-.6, .6),
    breaks = seq(-.6, .6, .2),
    labels = function(x) sprintf("%.2f", x),
    expand = c(0,0)
    ) +
  scale_fill_gradient2(
    low = par$salient,
    mid = par$grey,
    high = par$salient,
    limits = c(-.12,.12),
    oob = scales::squish
    ) +
  scale_color_gradient2(
    low = par$salient,
    mid = par$grey,
    high = par$salient,
    limits = c(-.12,.12),
    oob = scales::squish
    ) +
  expand_limits(
    x = c(-.6, .6),
    y = 9.5
    ) +
  guides(
    fill = "none",
    color = "none"
    ) +
  theme(
    #plot.title.position = "plot",
    text = element_text(color = par$bg),
    axis.text = element_text(
      size = par$fontsize / par$fontmult,
      lineheight = 1),
    axis.title.x = element_text(
      color = par$grey,
      size = par$fontsize / par$fontmult
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      vjust = 0, 
      color = par$black
      ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(
      linetype = 3,
      color = par$grey,
      linewidth = par$linewidth / 2),
    panel.grid.major.y = element_line(
      linetype = 3,
      color = par$grey,
      linewidth = par$linewidth / 2),
    plot.margin = margin(1, 1, 1, 1, unit = "lines"),
    plot.background = element_rect(
      colour = NA,
      fill = NA)
    ); posterior_plot

# Make the donut plots with percentages in the middle
plot_donut_var <- function(df, variable, title) {
  # prepares and aggregates the data
  da <- as.data.table(df)[
    , .N, by = variable][
      , N0 := cumsum(N) - N][
        , N1 := cumsum(N) ][
          , P := N / sum(N)
        ][
          , y := 0
        ][]
  setnames(da, variable, "variable")
  vlevels <- levels(da$variable)
  vlevels[1:2] <- vlevels[2:1]
  da$variable <- factor(da$variable, levels = vlevels, ordered = TRUE)

  # graphics parameters
  mult0 = c(-1, rep(1, nrow(da)-1)) # y-axis multiplier to achieve the gap
  mult1 = c(rep(-1, nrow(da)-1), 1)
  gap_size <- 24 # the bigger the larger the gap between round ends
  color_values <- c(par$salient, par$grey, par$black)[1:length(vlevels)]
  
  
  # make the plot
  ggplot(da,
    aes(
      y = 1,
      yend = 1,
      x =    N0 + mult0 * gap_size,
      xend = N1 + mult1 * gap_size,
      color = variable,
      fill = variable)
    ) +
    # outer darker segement
    geom_segment(
      linewidth = 2.35,
      lineend = 'round',
      position = "fill",
      show.legend = FALSE) +
    # inner lighter segment
    geom_segment(
      linewidth = 2.3,
      lineend = 'round', 
      color = "white",
      alpha = .7,
      position = "fill",
      show.legend = FALSE) +
    # small point in rounded lines
    geom_point(
      size = par$pointsize) +
    # text in circle (like 48%)
    geom_text(
      x = 0, # 0 starts at the top at 0 degrees
      hjust = 0.6, # higher moves more to the right
      y = 0, # bottom
      vjust = 2.0, # > 0 moves more towards the middle, scale unclear
      label = da[variable == levels(variable)[1], sprintf("%.0f%%", P*100)],
      color = par$salient,
      family = par$family,
      size = par$fontsize / .pt * 1.3) +
    coord_radial(expand = FALSE, inner.radius = 0.5, r.axis.inside = FALSE) +
    scale_y_continuous(
      expand = c(0,0), 
      limits = c(0, 2.1)) +
    scale_x_continuous(
      limits = c(-gap_size, max(da$N1) + gap_size * 3)) +
    ggtitle(title) +
    guides(
      count = "none",
      color = guide_legend(
        "",
        override.aes = list(
          geom = "point",
          shape = 21,
          stroke = par$linewidth * 1.5,
          size = par$pointsize,
          alpha = .4,
          stroke.alpha = 1)),
      fill = guide_legend("")) +
    scale_color_manual(NA, 
      values = color_values) +
    scale_fill_manual(NA,
      values = color_values) +
    theme(
      # smaller base size
      text = element_text(
        size = par$fontsize / par$fontmult,
        color = par$black),
      # title
      plot.title.position = "plot",
      plot.title = element_text(
        hjust = 0, # left
        vjust = 1, # top
        margin = margin(b = 0, t = 1, unit = "line")),
      # legend
      legend.position = "top",
      legend.direction = "horizontal",
      legend.location = "plot",
      legend.justification = "left",
      legend.text = element_text(
        hjust = 0,
        vjust = 0.5,
        margin = margin(l=0.2, b=0, unit = "lines")),
      legend.box.margin = margin(l=-0.7, b = -1.2, unit = "lines"),
      legend.key.height = unit(.3, "lines"),
      legend.key.width = unit(.2, "lines"),
      legend.key.spacing.x = unit(.2, "lines"),
      axis.text = element_blank()
    )
  }
# apply function to make plots
gender_plot <- plot_donut_var(df, "f_female", "Gender Female")
infection_plot <- plot_donut_var(df, "f_infected", "Covid-19 Infections")
work_plot <- plot_donut_var(df, "f_lostwork", "Covid-19 Work Loss")
# Text elements for the data viszalization
N <- nrow(fit_reduced_no_social$data)
mmyyyy <- unique(format(range(df$startdate), "%B %Y"))
title <- GrobModularGrid(
  align = "bl",
  text = "Covid-19\nDigital Health", 
  col = par$black,  fontface = "bold",  
  cex = 3.6 * par$fontmult,
  lineheight = 1,
  family = "bold", par)
subtitle <- GrobModularGrid(
  align = "tl",
  text = "Bayesian Sparse Modeling of Covid-19 Digital Health Tool Acceptance in Switzerland", 
  col = par$black,
  cex = par$fontmult,
  par)
description <- GrobModularGrid(
  align = "tl",
  text = glue("The acceptance of digital health-tracking tools to mitigate the spread of Covid-19 -- a machine-learning analysis. Comparing the influence of risk perceptions, risk preferences, social preferences, and social values in a preregistered analysis of a representative German-speaking Swiss sample (N={N}, {mmyyyy}). Best multivariate Bayesian predictive model given covariate feature selection using Bayesian projective predictive selection (Vehtari and Ojanen, 2012) and Lasso-type L1-penalisation."),
  wrap = par$wrap, par)

# arrange the plot using a rectangular grid
grid_layout <- "
TTTTT#
TTTTT#
SSSSSS
PPPPPD
PPPPPD
PPPPPD
PPPPP#
GIW###
"

# use `free(x)` to allow grids to overlap, i.e. to not align on the axes,
#      use side argument to control which sides to not align
viz <- list(
  #Z = ggplot() + theme_void(),
  D = free(description),
  T = free(title),
  S = free(subtitle),
  P = free(posterior_plot),
  G = gender_plot,
  I = infection_plot,
  W = work_plot
  ) |>
  ModularGrid(
    trace = T,
    design = grid_layout,
    pagesize = par$pagesize,
    guttersize = c(w = 6, h = 8), #mm
    pagemargins = c(l = 20, r = 20, t = 25, b = 20), #mm
    fieldsize = c(w = 23.33, h = 18) #mm
    ) +
  plot_annotation(
    caption = par$caption,
    theme = theme(plot.background = element_rect(fill = par$bg))
  )
# reset showtext dpi to see the viszalization
#showtext_opts(dpi = 74); #viz

# Save the data visualization
showtext_opts(dpi = par$dpi)
sapply(par$filetypes, function(i)
  ggsave(plot = viz,
         glue("{par$filename}.{i}"), 
         width = par$pagesize["w"],
         h = par$pagesize["h"], 
         unit = "mm",
         scale = 1, dpi = par$dpi
         )); showtext_opts(dpi = 74)
# Check the resulting visualization
system2("open", glue("{par$filename}.{par$filetypes[1]}"))

