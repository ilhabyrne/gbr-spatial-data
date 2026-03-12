# Byrne, 2026
# Subset coords

# ── Load the subset list ──────────────────────────────────────────────────────
keep_ids <- read_csv("~/Documents/GBR_coral_coords_subset_seq.csv")  # just the materialSampleID column
df <- read_csv("~/Documents/GBR_coral_coords_subset.csv")

# ── Filter the main metadata sheet ───────────────────────────────────────────
df_subset <- df %>%
  filter(species %in% c("Aken", "Aspa")) %>%          # only these two species
  filter(materialSampleID %in% keep_ids$materialSampleID)  # only matching IDs

# ── Check ─────────────────────────────────────────────────────────────────────
cat("IDs in subset list:       ", nrow(keep_ids), "\n")
cat("Aken + Aspat in metadata: ", nrow(filter(df, species %in% c("Aken", "Aspat"))), "\n")
cat("Rows kept:                ", nrow(df_subset), "\n")

# IDs in subset list not found in metadata (good to check for typos/mismatches)
missing <- keep_ids %>% filter(!materialSampleID %in% df$materialSampleID)
cat("IDs not found in metadata:", nrow(missing), "\n")
if (nrow(missing) > 0) print(missing)

# ── Write output ──────────────────────────────────────────────────────────────
write_csv(df_subset, "~/Documents/GBR_coral_coords_priority.csv")
