# Byrne, 2026
# Interactive mapping with leaflet

# Load packages
library(tidyverse)
library(leaflet)
library(leaflet.extras)  # install.packages("leaflet.extras") if needed

# ── Colour palette for locationID_short groups ───────────────────────────────
groups <- unique(locations$locationID_short)


# ── Colour palette ────────────────────────────────────────────────────────────
pal <- colorNumeric(
  palette = rev(c("red", "orange", "yellow", "cyan", "blue")),
  domain  = locations$decimalLatitude,
  reverse = FALSE
)

# ── Base map ──────────────────────────────────────────────────────────────────
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$CartoDB.Positron,  group = "Light") %>%
  addLayersControl(
    baseGroups    = c("Satellite", "Light"),
    overlayGroups = c("Original points", "Cluster midpoints", "Merge lines"),
    options       = layersControlOptions(collapsed = FALSE)
  )

# ── Lines connecting original points to their cluster midpoint ────────────────
multi <- clustered_locations %>% filter(n_merged > 1)

for (i in seq_len(nrow(multi))) {
  orig_pts <- locations %>% filter(clusterID == multi$clusterID[i])
  for (j in seq_len(nrow(orig_pts))) {
    m <- m %>%
      addPolylines(
        lng    = c(orig_pts$decimalLongitude[j], multi$clust_decimalLongitude[i]),
        lat    = c(orig_pts$decimalLatitude[j],  multi$clust_decimalLatitude[i]),
        color  = "orange", weight = 1.5, opacity = 0.7,
        group  = "Merge lines"
      )
  }
}

# ── Original unique points ────────────────────────────────────────────────────
m <- m %>%
  addCircleMarkers(
    data        = locations,
    lng         = ~decimalLongitude,
    lat         = ~decimalLatitude,
    radius      = 5,
    color       = ~pal(decimalLatitude),   # <-- fixed
    fillOpacity = 0.8,
    stroke      = FALSE,
    group       = "Original points",
    popup       = ~paste0(
      "<b>locationID:</b> ", locationID, "<br>",
      "<b>clusterID:</b> ",  clusterID,  "<br>",
      "<b>locationID_short:</b> ", locationID_short, "<br>",
      "<b>Lat:</b> ", round(decimalLatitude, 5),  "<br>",
      "<b>Lon:</b> ", round(decimalLongitude, 5)
    )
  )

# ── Cluster midpoints ─────────────────────────────────────────────────────────
m <- m %>%
  addCircleMarkers(
    data        = clustered_locations,
    lng         = ~clust_decimalLongitude,
    lat         = ~clust_decimalLatitude,
    radius      = 7,
    color       = ~pal(clust_decimalLatitude),
    fillOpacity = 1,
    stroke      = TRUE,
    weight      = 2,
    group       = "Cluster midpoints",
    popup       = ~paste0(
      "<b>clusterID:</b> ",   clusterID,   "<br>",
      "<b>n merged:</b> ",    n_merged,    "<br>",
      "<b>Source IDs:</b> ",  merged_locationIDs, "<br>",
      "<b>Lat:</b> ", round(clust_decimalLatitude, 5),  "<br>",
      "<b>Lon:</b> ", round(clust_decimalLongitude, 5)
    )
  )

m

library(htmlwidgets)
saveWidget(m, "~/Documents/GitHub/gbr-spatial-data/index.html", selfcontained = TRUE)
