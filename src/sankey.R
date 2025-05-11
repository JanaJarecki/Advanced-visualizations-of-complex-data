pacman::p_load_gh("davidsjoberg/ggsankey")
pacman::p_load(ggplot2, ggtext, forcats, dplyr)

#' Plots a Sankey Diagram
#'
#' @param df A data.frame with columns named `"x", "node", "next_x", "next_node"`
#' @param orderby (default = "freq") Order the sankeys by frequency (`"freq"`), `FALSE` orders by factor levels or alphabetically
#' @param labeller (default = `label_sankey_Vp`) a function name, specifying how to label the branches using the node title (e.g., fruit), the value (e.g. apple), a percent (23.7%), and the number (n=453)
#' * `label_sankey_Vp` (the default) means _APPLE 23.7%_ (capitalized)
#' * `label_sankey_vp` means _apple 23.7%_
#' * `label_sankey_V` means _APPLE_ (capitalized)
#' * `label_sankey_v` means _apple_
#' * `label_sankey_pbrV` means _23.7%_ linebreak _APPLE_ (capitalized)
#' * `label_sankey_nbrV` means _453_ linebreak _APPLE_ (capitalized)
#' * `label_sankey_Vnp` means _APPLE, n=453 (23.7%)_
#' * `label_sankey_vnp` means _apple, n=453 (23.7%)_
#' * `label_sankey_tvnp` means _fruit: apply, n=453 (23.7%)_
#' * `label_sankey_pbrTV` means _23.7%_ linebreak _FRUIT: APPLE_ (capitalized)
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot_sankey <- function(df, orderby = FALSE, labeller = label_sankey, label = "pv", 
                        na.rm = TRUE, ...) {
  df <- df[, c("x", "node", "next_x", "next_node")]
  if (!is.factor(df$x)) df$x <- factor(df$x)
  if (!is.factor(df$next_x)) df$next_x <- factor(df$next_x)
  if (is.ordered(df$node) != is.ordered(df$next_node)) {
    df$node <- factor(df$node, ordered=T)
    df$next_node <- factor(df$next_node, ordered=T)
  }
  
  # Set order of nodes
  if (orderby == TRUE || orderby == "freq") {
    df$node <- fct_rev(fct_infreq(as.character(df$node)))
    df$next_node <- factor(df$next_node)
  }
  unique_nodes <- if(is.factor(df$node)) { levels(df$node) } else { unique(df$node)}
  
  df <- add_label_sankey_cols(df)
  
  # Make color values
  colorvals <- NULL
  if (!is.null(getOption('ggplot2.discrete.colour'))) {
    colorvals <- getOption('ggplot2.discrete.colour')()$palette(length(unique_nodes))
    names(colorvals) <- unique_nodes
  }
  
  params <- c(list(v
                   =df$node, n=df$n, p=df$p, t=df$x),
                  type = label, list(...),
                  get_sankey_beau_params("labeller"))
  df$lab <- do.call(labeller, params[!duplicated(names(params))])

  ggplot(df,
    aes(
      x = x, next_x = next_x, 
      node = node, next_node = next_node,
      fill = factor(node),
      label = lab)) + 
    geom_sankey_beau(...) +
    geom_sankey_text_beau(...) +
    coord_cartesian(clip = 'off')
}

#' Sankey geom with aesthetic defaults
#'
#' @param type A character, `"label"` is for text parameters, `"sankey"` for flow and node parameters 
#'
#' @return
#' @export
#'
#' @examples
get_sankey_beau_params <- function(type = c("label", "sankey", "labeller")) {
  family <- theme_get()$text$family
  node.color <- theme_get()$panel.background$fill
  if (type == "label") {
    list(fill = NA,
         size = calc_element("text", theme_get())$size /.pt,
         family = ifelse(length(family), theme_get()$text$family,""),
         lineheight = 0.7,
         width = 1,
         show.legend = FALSE,
         label.padding = margin(r=0.2, l=.001, t=.001, b=.001, "lines"),
         label.r = unit(0, "lines")         )
  } else if (type == "sankey") {
    list(width = 0.03,
         flow.alpha = 0.17,
         linewidth = 1.1,
         node.color = ifelse(length(node.color), node.color, NA),
         show.legend = FALSE,
         space = NULL,
         na.rm = TRUE
         )
  } else if (type == "labeller") {
    list(
      scale = .82,
      size = calc_element("axis.text.x.bottom", theme_get())$size,
      color = "grey40"
      )
  }
}

geom_sankey_beau <- function(...) {
  params <- c(list(...), get_sankey_beau_params("sankey"))
  # clean parameters that are for the label or text
  params[grep("^label|^text", names(params), value=T)] <- NULL
  do.call(geom_sankey, params[!duplicated(names(params))])
}


geom_sankey_text_beau <- function(...) {
  params <- c(list(...), get_sankey_beau_params("label"), label.colour = NA)
  # clean parameters that are for the flow or node
  params[grep("^flow|^node|linewidth|smooth", names(params), value=T)] <- NULL
  do.call(geom_sankey_richtext, params[!duplicated(names(params))])
}


#' Adaptation of geom_sankey to handle richtext
#'
#' @param mapping 
#' @param data 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param space 
#' @param type 
#' @param width 
#' @param inherit.aes 
#' @param ... 
#'
#' @return
#'
#' @examples
geom_sankey_richtext <- function(mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 show.legend = FALSE,
                                 inherit.aes = TRUE,
                                 na.rm = TRUE,
                                 type = "sankey",
                                 width = .1,
                                 ...) {
  
  # Position
  if (!length(mapping)) {
    mapping <- aes(
      x = stage(x,
                after_stat = x + 0.02 *
                  dplyr::case_when(
                    x == 1 ~ -1,
                    x == length(unique(x)) ~ 1,
                    .default = 1)
      ),
      hjust = dplyr::case_when(
        x == first(levels(x)) ~ 1,
        x == last(levels(x)) ~ 0,
        .default = 0
      )
    )
  }
  # Prepare aesthics for label
  params <- list(...)
  
  list(
    label = ggplot2::layer(
      stat = ggsankey:::StatSankeyText,
      data = data,
      mapping = mapping,
      geom = "richtext",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(
        list(
          na.rm = na.rm,
          type = type,
          width = width,
          params
        )
      )
    )
  )
}




#' Adds colunmns with aggregated statistics (N, p, etc.) for sankey diagram
#'
#' @param df A data.frame with columns named _node_, _next_node_, _x_, _next_x_.
#'
#' @return The input data frame `df` with new columns named and `p` (the proportion), `n` (the number of observations pre node), `N` (the number of observations per x)
#' 
#' @export
#'
#' @examples
add_label_sankey_cols <- function(df) {
  # Get Aggregate numbers
  dfnp <- df |> 
    group_by(x) |> 
    mutate(N = n()) |> 
    dplyr::group_by(x, node, N) |> 
    tally() |> 
    mutate(p = n / N)
  merge(df, dfnp, by.x = c('x', 'node'), by.y = c('x','node'), all.x = TRUE)
}

#' Fancy label for sankeys
#'
#' @param x A vector with the values of each node (source and target values)
#' @param n A vector with the number of occurrences of each `node` (absolute frequencies)
#' @param p A vector with the proportion of occurrence of each `node` (relative frequencies)
#' @param t A vector with variable names associated to each `node`
#'
#' @return
#' @export
#'
#' @examples
label_sankey <- function(v = NA, n = NA, p = NA, t = NA, type = "np", 
                         p.bold = T, v.format = T, scale = 1, color="black", 
                         ...) {
  if (!is.factor(t)) t <- factor(t)
  
  if (!all(is.na(p))) {
    p <- format_sankey_p(p = p)
  }
  if (grepl("vnp", type)) {
    p.bold <- FALSE
    p <- sprintf("(%s)", p)
    n <- sprintf("n=%s", n)
  }
  if (p.bold) {
    p <- sprintf("<b>%s</b>", p)
  }
  
  M <- cbind(t=t, v=v, n=n, p=p, V=toupper(v), T=toupper(t), `_`="<br>", `,`=",")
  cols <- strsplit(type, "")[[1]]
  M <- M[, cols, drop = F]
  
  colnames(M) <- tolower(colnames(M))
  cols <- tolower(cols)

  if (grepl("tv", tolower(type))) {
    M[, "t"] <- sprintf("%s: ", M[, "t"])
  }
  if (v.format ==TRUE) {
    if (any(cols == "v"))
      M[, "v"] <- format_sankey_text(M[, "v"], scale=scale, color=color, ...)
    if (any(cols == "t"))
      M[, "t"] <- format_sankey_text(M[, "t"], scale=scale, color=color, ...)
  }
  
  # If label starts with n or p we want the % first % ...) in the first state and
  # last (... %) in the next states
  if (nchar(type) > 1) {
    # flags the first node
    flag <- t == levels(t)[1]
    j <- c("p")
    if (grepl("pn", type)) {
      j <- c("p", "n")
    } else if (grepl("np", type)) {
      j <- c("n", "p")
    } else if (grepl("n", type) & nchar(type) == 1) {
      colnames(M) <- sub("n", "p", colnames(M))
      cols <- colnames(M)
    }
    if (length(j) == 2) {
      # paste np together
      M[, "p"] <- apply(M[, j, drop=F], 1, paste, collapse="")
      M[, "n"] <- NULL
    }
    if (cols[1] == "p" & !grepl(sprintf("%s_", j[1]), type)) {
      M[flag, ] <- M[flag, c(cols[-1], "p"), drop = F]
      M[, "p"] <- ifelse(flag,
                         sprintf("%s<span style='font-size:9pt;'> </span>",  M[, "p"]),
                         sprintf("<span style='font-size:6pt;'> </span>%s",  M[, "p"])
      )
    }
    if (cols[length(cols)] == "p" & !grepl(sprintf("_%s", j[1]), type)) {
      M[flag, ] <- M[flag, c("p", cols[-length(cols)]), drop = F]
      M[, "p"] <- ifelse(!flag,
                         sprintf("%s<span style='font-size:9pt;'> </span>",  M[, "p"]),
                         sprintf("<span style='font-size:6pt;'> </span>%s",  M[, "p"])
      )
    }
  }
  
  return(apply(M, 1, paste, collapse=""))
}

format_sankey_p <- function(p){
  if (!is.numeric(p) || any(p < 0 | p > 1)) {
    stop("Percentage p for the label must be a number between 0 and 1 but isn't.")
  }
  digits <- 0
  max.digits <- 2
  p <- p*100
  # if we have a rounded probability of 0, we add another digit
  while(any(as.numeric(sprintf(sprintf('%%.%if', digits), p)) == 0) &
        digits < max.digits) {
    digits <- digits + 1
  }
  return(sprintf("%s%%", sprintf(sprintf('%%.%if', digits), p)))
}

format_sankey_text <- function(x, size, color, scale, ...) {
  size <- size * scale
  fmt <- sprintf('color:%s; font-size:%spt; line-height:%spt;', color, size, size)
  sprintf("<span style='%s'>%s</span>", fmt, x)
}


