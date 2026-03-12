# Byrne, 2026
# Wrangling sampling coordinates

# Load packages
library(tidyverse)
library(tidyverse)
library(geosphere)  # install.packages("geosphere") if needed

# ---- UNIQUE COORDS ---- 

# ── 0. Load data ──────────────────────────────────────────────────────────────
df <- read_csv("~/Documents/gbr-spatial-data/GBR_coral_coords.csv")

# ── 1. Build the unique locations table ───────────────────────────────────────
locations <- df %>%
  distinct(decimalLatitude, decimalLongitude, locationID_short) %>%
  arrange(locationID_short, decimalLatitude, decimalLongitude) %>%
  group_by(locationID_short) %>%
  mutate(locationID = paste0(locationID_short, "_", row_number())) %>%
  ungroup()

# ── 2. Join locationID back to the original dataframe ─────────────────────────
df_annotated <- df %>%
  left_join(locations, by = c("decimalLatitude", "decimalLongitude", "locationID_short"))

# ── 3. Write outputs ──────────────────────────────────────────────────────────
write_csv(df_annotated, "GBR_coords_with_coordID.csv")
write_csv(locations, "GBR_coords_unique_with_coordID.csv")

# ── 4. Sanity check ───────────────────────────────────────────────────────────
cat("Original rows:   ", nrow(df), "\n")
cat("Unique locations:", nrow(locations), "\n")
cat("Annotated rows:  ", nrow(df_annotated), "\n")
cat("Any unmatched?   ", any(is.na(df_annotated$locationID)), "\n")


# ---- SPATIALLY AGGREGATED COORDS ---- 

# ── 0. Load data ──────────────────────────────────────────────────────────────
df <- read_csv("~/Documents/gbr-spatial-data/GBR_coral_coords.csv")

# ── 1. Unique locations (as before) ───────────────────────────────────────────
locations <- df %>%
  distinct(decimalLatitude, decimalLongitude, locationID_short) %>%
  arrange(locationID_short, decimalLatitude, decimalLongitude) %>%
  group_by(locationID_short) %>%
  mutate(locationID = paste0(locationID_short, "_", row_number())) %>%
  ungroup()

df_annotated <- df %>%
  left_join(locations, by = c("decimalLatitude", "decimalLongitude", "locationID_short"))

# ── 2. Cluster locations within 100m of each other ───────────────────────────
# Build pairwise Haversine distance matrix (in metres)
coords_mat <- locations %>% select(decimalLongitude, decimalLatitude) %>% as.matrix()
dist_m <- distm(coords_mat, fun = distHaversine)  # n x n matrix in metres

# Hierarchical clustering with single linkage:
# single linkage means: if A is within 100m of B and B within 100m of C,
# all three are grouped. Change to "complete" if you want every point
# in a cluster to be within 100m of *all* others in that cluster.
hc <- hclust(as.dist(dist_m), method = "single")
locations$clusterID_num <- cutree(hc, h = 12)  # cut at 100m

# ── 3. Build the condensed (clustered) locations table ───────────────────────
clustered_locations <- locations %>%
  group_by(clusterID_num, locationID_short) %>%
  summarise(
    clust_decimalLatitude  = mean(decimalLatitude),
    clust_decimalLongitude = mean(decimalLongitude),
    n_merged               = n(),
    merged_locationIDs     = paste(locationID, collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(locationID_short, clusterID_num) %>%
  group_by(locationID_short) %>%
  mutate(clusterID = paste0(locationID_short, "_", row_number())) %>%
  ungroup() %>%
  select(clusterID, clusterID_num, locationID_short,   # <-- clusterID_num kept here
         clust_decimalLatitude, clust_decimalLongitude,
         n_merged, merged_locationIDs)

# ── 4. Map clusterID back to locations and original df ───────────────────────
locations <- locations %>%
  left_join(
    clustered_locations %>% select(clusterID, merged_locationIDs),
    by = c("locationID" = "merged_locationIDs")  # won't work for multi — see below
  )

# Better approach: join via the cluster number
locations <- locations %>%
  left_join(
    clustered_locations %>% select(clusterID, clusterID_num, locationID_short),
    by = c("clusterID_num", "locationID_short")
  )

df_annotated <- df_annotated %>%
  left_join(
    locations %>% select(decimalLatitude, decimalLongitude, locationID_short, clusterID_num),
    by = c("decimalLatitude", "decimalLongitude", "locationID_short")
  )

# ── 5. Write outputs ──────────────────────────────────────────────────────────
write_csv(df_annotated,        "~/Documents/gbr-spatial-data/GBR_coral_coords_orig_12m.csv") # all original rows + locationID + clusterID
write_csv(locations,           "~/Documents/gbr-spatial-data/GBR_coral_coords_12m_clustID.csv") # one row per unique coord + clusterID
write_csv(clustered_locations, "~/Documents/gbr-spatial-data/GBR_coral_coords_12m_clussters.csv") # one row per cluster, midpoint coords

# ── 6. Sanity check ───────────────────────────────────────────────────────────
cat("Original unique locations: ", nrow(locations), "\n")
cat("After clustering (12m/50m):   ", nrow(clustered_locations), "\n")
cat("Clusters with >1 point:    ", sum(clustered_locations$n_merged > 1), "\n")

