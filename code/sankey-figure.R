rm(list=ls())
# sets working directory to source file location
if (require(rstudioapi)) setwd(dirname(getActiveDocumentContext()$path))
# ensures `pacman` is available to automatize library management
if (!require(pacman)) install.packages("pacman")
# Setup
pacman::p_load(showtext, MetBrewer)
source("../src/sankey.R")
# Customizes
font_add_google("Saira Extra Condensed", regular.wt=300, bold.wt=600, db_cache=T)
showtext_auto()

# Set ggtheme with the custom font
theme_set(theme_sankey(
  base_family = "Saira Extra Condensed",
  base_size = 20
))
theme_update(
  text = element_text(),
  axis.title.x = element_blank())

mydf <- as.data.frame(HairEyeColor)[rep(1:32, as.data.frame(HairEyeColor)$Freq),]
mydf <- make_long(mydf, Sex, Hair, Eye)
plot_sankey(mydf, flow.alpha = .2, space = 80, smooth=10) +
  scale_fill_manual(values = met.brewer("Peru1", 9, type = "c"))


showtext_opts(dpi = 300); ggsave("sankey.png"); showtext_opts(dpi = 72)



ggplot(mydf, aes(x=x,next_x=next_x,node=node,next_node=next_node,label=node,fill=node)) +
  geom_sankey() +
  geom_sankey_text() +
  scale_fill_manual(values = met.brewer("Peru1", 9, type = "c")) +
  theme(legend.position = "none")

ggsave("sankey2.png")

# A sankey from a publication
source("utils/make_bircketal_data.R")
df <- make_bircketal_data()
plot_sankey(df,
            label="pV",
            space = 1500, 
            flow.alpha = .55,
            width = .01,
            smooth = 5) +
  scale_fill_manual(values = met.brewer("Peru1", 9)) +
  labs(
    title = "Sankey Diagrams More Beautiful",
    subtitle = "Real-world treatment patterns of rheumatoid arthritis in Brazil: analysis of DATASUS national administrative claims data for pharmacoepidemiology studies (2010-2020)",
    caption = "Data from Birck et al. (2023)"
  )

pacman::p_load(bigrquery, dplyr, DBI, data.table, rrapply)
project <- "avid-stratum-187112" # your project ID
sql <- "SELECT
    fullVisitorId,
    visitNumber,
    totals,
    trafficSource,
    trafficSource.adContent,
    device,
    hits
  FROM
    `bigquery-public-data.google_analytics_sample.ga_sessions_*`
  WHERE
    _TABLE_SUFFIX BETWEEN '20170701'
    AND '20170731'"
tb <- bq_project_query(project, sql)
d <- bq_table_download(tb, page_size = 400)



saveRDS(d, "google_analytics_sample.RDS")
d <- readRDS("google_analytics_sample.RDS")
setnames(d, "fullVisitorId", "vid")

# Codebook
# https://support.google.com/analytics/answer/3437719?hl=en

# hits.eCommerceAction.action_type
setDT(d)
getrapply <- function(v, data = d) {
  rrapply(data, how = "flatten", condition = function(x, .xname) .xname==v)
}
dl <- d[, .(vid, visitNumber)]
for (v in c("action_type")) {
  dl[, c(v) := getrapply(..v)]
  
}
dl[, minOnSite := timeOnSite / 60]
dl[, minOnSiteCat := cut(minOnSite, c(0,1,10,17,60))]
dl[, transactions := factor(transactions,
                            c(NA,1:2),
                            c("no purchase", "1 purchase", paste(2, "purchases")),
                            exclude = NULL)]
dl |> make_long(deviceCategory, minOnSiteCat, transactions) |> 
  plot_sankey(label = "nV")
dl[is.na(timeOnSite)]


da <- rrapply(d, how="melt", 
        condition = function(x, .xname) 
          .xname=="action_type")
setDT(da)
setnames(da, "value", "actiontype")
da[, 
   vid := factor(L2, labels = paste(d$vid, d$visitNumber))
  ][,
  c("vid", "visitNumber") := tstrsplit(as.character(vid), " ")
  ][,
    actiontype := as.numeric(actiontype)
  ][,
    actiontype.f := factor(actiontype, action.dic, names(action.dic))
  ]

da[actiontype %in% c(1,2,3,5,6), .N, by = .(actiontype.f)]





action.dic = c(
  "Click throu)gh of product lists" = 1, 
  "Product detail views" = 2, 
  "Add product(s) to cart" = 3, 
  "Remove product(s) from cart" = 4, 
  "Check out" = 5, 
  "Completed purchase" = 6, 
  "Refund of purchase" = 7, 
  "Checkout options" = 8, 
  "Unknown" = 0)

action_type <- lapply(d[["hits"]], function(x) lapply(x[["eCommerceAction"]], `[`, "action_type"))
action_type <- lapply(action_type, function(x) data.frame(actionTypeNum = seq_along(x), action_type = unlist(x)))
names(action_type) <- d$fullVisitorId
lapply(d[["hits"]], function(x) length(x[["isExit"]]))
d$hits[[500]]$eCommerceAction$action_type
hits <- data.table:::rbindlist(action_type, id = "vid")
hits[, action_type := factor(dd$action_type, levels, names(levels))]
hits <- dd[action_type != "Unknown", ]
hits[order(actionTypeNum), actionTypeNum := 1:.N, by = vid]
table(dd$actionTypeNum)
dd[, .(
  Purchase := grepl("Complete Purchase", action_type)), by = vid]

exit <- lapply(d[["hits"]], function(x) data.frame(isExit = x[["isExit"]]))
exit <- rbindlist(setNames(exit, d$fullVisitorId), id = "vid")

d[vid %in% d$vid[47],]

d[, .N, by = .(vid, visitNumber)][, max(N)]

lapply(d[1:2,][["trafficSource"]], `[[`, "source")
d[1,]$trafficSource[[1]]

varnames <- c("vid", "visitNumber", "deviceCategory", "source")

setDT(dl)
dl
dl[, L2 := factor(L2, levels = tstrsplit(value[1], ", "))]


d[["hits"]][[8]][["eCommerceAction"]][[3]][c("action_type")][1]

# hits.eCommerceAction.step
# This field is populated when a checkout step is specified with the hit.
# hits.product.productListName
# Name of the list in which the product is shown, or in which a click occurred. For example, "Home Page Promotion", "Also Viewed", "Recommended For You", "Search Results List", etc.


# Use your project ID here
# Example query - select copies of files with content containing "TODO"
# Execute the query and store the result
res <- query_exec(sql, project = project, useLegacySql = FALSE)
