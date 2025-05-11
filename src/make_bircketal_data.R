pacman::p_load(jsonlite)

make_bircketal_data <- function() {
  # Birck MG, Ferreira R, Curi M, Krueger WS, Julian GS, Liede A. Real-world treatment patterns of rheumatoid arthritis in Brazil: analysis of DATASUS national administrative claims data for pharmacoepidemiology studies (2010-2020). Sci Rep. 2023 Oct 18;13(1):17739. doi: 10.1038/s41598-023-44389-9. PMID: 37853013; PMCID: PMC10584810.
  

  sourcejson <- '[8, 8, 8, 8, 8, 8, 8, 8, 18, 18, 18, 18, 18, 18, 18, 18, 6, 6, 6, 6, 6, 6, 6, 6, 15, 15, 15, 15, 15, 15, 15, 15, 7, 7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 17, 17, 17, 17, 17, 17, 17, 17, 1, 1, 1, 1, 1, 1, 1, 1, 13, 13, 13, 13, 13, 13, 13, 13, 4, 4, 4, 4, 4, 4, 4, 4, 11, 11, 11, 11, 11, 11, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 16, 16, 16, 16, 16, 2, 2, 2, 2, 2, 2, 2, 2, 12, 12, 12, 12, 12, 12, 12, 12, 5, 5, 5, 5, 5, 5, 5, 5, 14, 14, 14, 14, 14, 14, 14, 14]'
  targetjson <- '[15, 10, 17, 13, 11, 16, 12, 14, 27, 21, 24, 23, 20, 19, 25, 26, 18, 10, 17, 13, 11, 16, 12, 14, 22, 21, 24, 23, 20, 19, 25, 26, 18, 15, 17, 13, 11, 16, 12, 14, 22, 27, 24, 23, 20, 19, 25, 26, 18, 15, 10, 13, 11, 16, 12, 14, 22, 27, 21, 23, 20, 19, 25, 26, 18, 15, 10, 17, 11, 16, 12, 14, 22, 27, 21, 24, 20, 19, 25, 26, 18, 15, 10, 17, 13, 16, 12, 14, 22, 27, 21, 24, 23, 19, 25, 26, 18, 15, 10, 17, 13, 11, 12, 14, 22, 27, 21, 24, 23, 20, 25, 26, 18, 15, 10, 17, 13, 11, 16, 14, 22, 27, 21, 24, 23, 20, 19, 26, 18, 15, 10, 17, 13, 11, 16, 12, 22, 27, 21, 24, 23, 20, 19, 25]'
  njson <- '[89, 57, 71, 46, 29, 119, 336, 97, 98, 96, 63, 69, 54, 160, 573, 183, 1551, 1029, 3885, 1622, 758, 597, 1667, 670, 431, 130, 313, 229, 107, 138, 484, 152, 207, 311, 159, 125, 83, 36, 202, 157, 137, 87, 49, 30, 28, 39, 123, 97, 1218, 3857, 716, 1105, 514, 483, 1216, 425, 433, 361, 159, 251, 87, 155, 437, 152, 370, 523, 210, 296, 71, 92, 337, 172, 257, 110, 76, 105, 21, 69, 264, 97, 591, 1458, 172, 884, 380, 200, 821, 95, 111, 116, 27, 56, 40, 42, 174, 33, 79, 23, 12, 21, 10, 8, 82, 49, 73, 29, 18, 23, 9, 9, 93, 61, 192, 74, 71, 61, 34, 36, 121, 101, 353, 115, 71, 63, 69, 56, 219, 171, 8, 56, 23, 8, 25, 4, 3, 22, 18, 27, 21, 10, 10, 3, 8, 22]'
  
  labeljson <- '["Rituximab", "Golimumab", "Tocilizumab", "Etanercept", "Infliximab", "Tofacitinib", "Adalimumab", "Certolizumab", "Abatacept", "", "Certolizumab", "Infliximab", "Tocilizumab", "Golimumab", "Tofacitinib", "Adalimumab", "Rituximab", "Etanercept", "Abatacept", "Rituximab", "Infliximab", "Certolizumab", "Abatacept", "Golimumab", "Etanercept", "Tocilizumab", "Tofacitinib", "Adalimumab", ""]'
  
  labels <- fromJSON(labeljson)
  labels <- replace(labels, labels == "", NA)
  source <- fromJSON(sourcejson) + 1
  target <- fromJSON(targetjson) + 1
  n <- fromJSON(njson)
  
  df <- data.frame(source, target, first = labels[source], second = labels[target], n)
  ll <- lapply(1:2, function(i) {
    xx <- c("first", "second", "third")
    d <- df |> 
      mutate(id = paste(first, second)) |>
      group_by(id) |> 
      filter((1:n()) == i) |> 
      ungroup()
    
    d <- d[rep(1:nrow(d), d$n), c("first", "second")]
    names(d) <- c(xx[i], xx[i+1])
    d
  })
  
  ll[[1]] <- ll[[1]]|> group_by(second) |> mutate(id = 1:n()) |> ungroup()
  ll[[2]] <- ll[[2]] |> group_by(second) |> mutate(id = 1:n()) |> ungroup()
  
  
  ll[[1]] |> left_join(ll[[2]], by = c("second", "id")) |> 
    select(-id) |>
    make_long(first, second, third) |> 
    filter(!(x == "third" & is.na(node)))
}