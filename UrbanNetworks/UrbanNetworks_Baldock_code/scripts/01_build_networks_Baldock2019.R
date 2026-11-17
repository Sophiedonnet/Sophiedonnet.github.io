# Build the plant-pollinator networks of Baldock et al. (2019), by city, by land use and
# Run from the code folder (UrbanNetworks_Baldock_code):  Rscript scripts/01_build_networks_Baldock2019.R
# by city x land use, with the status (native / non-native) of the plants.
#
# Source: Baldock K.C.R. et al. (2019) A systems approach reveals urban pollinator hotspots
# and conservation opportunities. Nature Ecology & Evolution 3: 363-373.
# doi:10.1038/s41559-018-0769-y. Files = Supplementary Information of the paper, see
# data_Baldock2019/README.md.
#
# Output: data_Baldock2019/baldock2019_networks.rds, a list with
#   records       one row per observed visit (city, land use, site, plant, insect, status...)
#   plants        one row per plant taxon: status and how it was obtained
#   insects       one row per insect taxon: order, family and how they were obtained
#   net_city      list of 4 count matrices (plants x insects), one per city
#   net_landuse   list of 9 count matrices, one per land use (4 cities pooled)
#   plants_city   one row per plant x city: visits and floral units (quadrats, NA if not counted)
#   net_city_landuse  list of count matrices, one per city x land use (35, some tiny)

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

dir_data <- "data_Baldock2019"
cities <- c("Bristol", "Edinburgh", "Leeds", "Reading")
landuse_codes <- c(ALT = "Allotment", CEM = "Cemetery", GDN = "Garden",
                   MMS = "Manmade surface", NR = "Nature reserve", OGS = "Other greenspace",
                   PK = "Park", PVT = "Pavement", VRG = "Verge")

# Names compared after removing case, spaces and punctuation
norm <- function(x) gsub("[^a-z]", "", tolower(x))
genus_of <- function(x) word(str_squish(gsub("^\\?", "", x)), 1)

#------------------------------------------------------------------
# 1. Interaction records (Supplementary Data 5): one row per visit
#------------------------------------------------------------------
records <- bind_rows(lapply(cities, function(s) {
  read_excel(file.path(dir_data, "SuppData5_interactions.xlsx"), sheet = s) |>
    mutate(city = s)
})) |>
  rename(site = Site, plant = `Plant ID`, insect = `Visitor ID`, specimen = `Specimen code`) |>
  mutate(plant = str_squish(plant), insect = str_squish(insect),
         landuse = unname(landuse_codes[str_match(site, "^[A-Z]+_([A-Z]+)_")[, 2]])) |>
  select(city, landuse, site, plant, insect, specimen)

stopifnot(!any(is.na(records$landuse)))

# 7 visits have no plant name (3 in Bristol, 4 in Edinburgh): removed
cat("Visits without plant name, removed:", sum(is.na(records$plant)), "\n")
records <- records |> filter(!is.na(plant))

#------------------------------------------------------------------
# 2. Plant status (Supplementary Table 8, after PLANTATT)
#------------------------------------------------------------------
# Table 8: one row per plant taxon of the null-model analyses, status in column 2.
# The authors define "Non-native" as archaeophyte or neophyte in PLANTATT; 10 genus groups
# are "Native & non-native taxa".
tab8 <- suppressMessages(read_excel(file.path(dir_data, "SuppTable8_plant_status.xlsx"),
                                    col_names = FALSE, col_types = "text", skip = 8))
status_tab <- tibble(taxon = str_squish(tab8[[1]]), status = tab8[[2]]) |>
  filter(!is.na(taxon), status %in% c("Native", "Non-native", "Native & non-native taxa")) |>
  mutate(status = recode(status, "Native & non-native taxa" = "Mixed"),
         key = norm(taxon), genus = genus_of(taxon),
         genus_group = str_detect(taxon, "\\bspp?\\.?$"))

# Matching, in this order:
#   exact      same name (up to case and punctuation)
#   grouped    the table entry starts with the plant name (taxa grouped by the authors,
#              e.g. "Campanula portenschlagiana C poscharskyana")
#   genus      the table has a genus group "Genus spp." / "Genus sp." for the plant's genus
#              (the authors pooled these records into the genus group for their analyses,
#              so the status is theirs, but it can be wrong for a given species, e.g.
#              Symphytum officinale is native but "Symphytum spp." is Non-native, and
#              Epilobium ciliatum is a neophyte but "Epilobium spp." is Native)
#   (none)     no status: family-level records, or species absent from the table
match_status <- function(p) {
  k <- norm(p)
  i <- which(status_tab$key == k)
  if (length(i) == 1) return(c(status_tab$status[i], "exact", status_tab$taxon[i]))
  i <- which(startsWith(status_tab$key, k))
  if (length(i) == 1) return(c(status_tab$status[i], "grouped", status_tab$taxon[i]))
  i <- which(status_tab$genus_group & status_tab$genus == genus_of(p))
  if (length(i) == 1) return(c(status_tab$status[i], "genus", status_tab$taxon[i]))
  c(NA, NA, NA)
}

plants <- records |> count(plant, name = "n_visits") |>
  rowwise() |>
  mutate(m = list(match_status(plant)),
         status = m[1], status_source = m[2], status_taxon = m[3]) |>
  ungroup() |> select(-m)

#------------------------------------------------------------------
# 3. Insect order and family
#------------------------------------------------------------------
# (a) Size_categories sheet of Supplementary Data 5 (measured taxa only)
size_tab <- read_excel(file.path(dir_data, "SuppData5_interactions.xlsx"),
                       sheet = "Size_categories") |>
  transmute(key = norm(Species), order = Order, family = Family, itd_mm = `Mean ITD (mm)`) |>
  distinct(key, .keep_all = TRUE)

# (b) Doré et al. compilation (same Baldock 2019 networks, with insect taxonomy), if present
#     Doré M., Fontaine C., Thébault E. (2021) Global Change Biology 27(6): 1266-1280.
#     doi:10.1111/gcb.15474
dore_file <- file.path(dir_data, "dore_baldock-interaction.csv")
dore_tab <- if (file.exists(dore_file)) {
  read.csv2(dore_file) |>
    transmute(key = norm(paste(insectgenus, insectspecies)), genus = insectgenus,
              order = insectorder, family = insectfamily) |>
    filter(order != "", !is.na(order)) |>
    distinct(key, .keep_all = TRUE)
} else tibble(key = character(), genus = character(), order = character(), family = character())

insects <- records |> count(insect, name = "n_visits") |>
  mutate(key = norm(insect), genus = genus_of(insect)) |>
  left_join(size_tab, by = "key") |>
  mutate(taxo_source = if_else(!is.na(order), "size_table", NA_character_))

# fill from Doré (same name)
i <- is.na(insects$order) & insects$key %in% dore_tab$key
j <- match(insects$key[i], dore_tab$key)
insects$order[i] <- dore_tab$order[j]; insects$family[i] <- dore_tab$family[j]
insects$taxo_source[i] <- "dore"

# fill from the genus (only if all known taxa of the genus share order and family)
genus_tab <- bind_rows(
  insects |> filter(!is.na(order)) |> select(genus, order, family),
  dore_tab |> select(genus, order, family)) |>
  distinct() |> group_by(genus) |> filter(n() == 1) |> ungroup()
i <- is.na(insects$order) & insects$genus %in% genus_tab$genus
j <- match(insects$genus[i], genus_tab$genus)
insects$order[i] <- genus_tab$order[j]; insects$family[i] <- genus_tab$family[j]
insects$taxo_source[i] <- "genus"

# the 4 taxa left, filled by hand ("Bacca" is a typo for Baccha)
manual_taxo <- tribble(
  ~insect,                            ~order,        ~family,
  "Bacca elongata",                   "Diptera",     "Syrphidae",
  "Limniella quadrata",               "Diptera",     "Ephydridae",
  "Passoloecus sp",                   "Hymenoptera", "Crabronidae",
  "Sarcophaga (Sarcophaga) carnaria", "Diptera",     "Sarcophagidae")
i <- is.na(insects$order) & insects$insect %in% manual_taxo$insect
j <- match(insects$insect[i], manual_taxo$insect)
insects$order[i] <- manual_taxo$order[j]; insects$family[i] <- manual_taxo$family[j]
insects$taxo_source[i] <- "manual"

# Coarse functional group, often used for pollinators
insects <- insects |>
  mutate(group = case_when(
    family %in% c("Apidae", "Andrenidae", "Halictidae", "Megachilidae", "Colletidae",
                  "Melittidae") ~ "Bee",
    family == "Syrphidae" ~ "Hoverfly",
    order == "Diptera" ~ "Other Diptera",
    order == "Coleoptera" ~ "Beetle",
    order == "Lepidoptera" ~ "Butterfly / moth",
    order == "Hymenoptera" ~ "Other Hymenoptera",
    TRUE ~ NA_character_)) |>
  select(insect, n_visits, order, family, group, itd_mm, taxo_source)

records <- records |>
  left_join(plants |> select(plant, status), by = "plant") |>
  left_join(insects |> select(insect, order, group), by = "insect")

#------------------------------------------------------------------
# 4. Count matrices (plants in rows, insects in columns)
#------------------------------------------------------------------
to_matrix <- function(df) {
  tab <- df |> count(plant, insect) |>
    pivot_wider(names_from = insect, values_from = n, values_fill = 0)
  M <- as.matrix(tab[, -1]); rownames(M) <- tab$plant
  M[order(rownames(M)), order(colnames(M)), drop = FALSE]
}

net_city    <- lapply(split(records, records$city), to_matrix)
net_landuse <- lapply(split(records, records$landuse), to_matrix)
net_city_landuse <- lapply(split(records, paste(records$city, records$landuse, sep = "_")),
                           to_matrix)

#------------------------------------------------------------------
# 4b. Floral abundance of each visited plant, per city (Supplementary Data 3)
#------------------------------------------------------------------
# Floral units counted in 1 x 1 m quadrats every 4 m along the transects, one row per
# land use x round. Summed over the rows of a city, i.e. pooled like the city networks.
# Plants were visited along the whole transect but counted in quadrats only, so many
# visited plants have no floral count (NA, not 0). Taxa are the authors' (some grouped
# by genus, "Allium.spp"); matched to the visited plants as for the status (exact,
# grouped, genus). A genus group gives its total to each species of the genus.
read_floral <- function(city) {
  b <- suppressMessages(read_excel(file.path(dir_data, "SuppData3_floral_abundance.xlsx"),
                                   sheet = city, col_names = FALSE, col_types = "text"))
  i_vis <- which(b[[1]] == "Flower visitors")
  taxa <- unlist(b[3, -1])
  fl <- b[4:(i_vis - 1), ]
  fl <- fl[!is.na(fl[[1]]), -1]
  tibble(city = city, taxon = taxa,
         floral_units = colSums(apply(fl, 2, as.numeric), na.rm = TRUE)) |>
    filter(!is.na(taxon)) |>
    mutate(key = norm(taxon), genus = genus_of(gsub("\\.", " ", taxon)),
           genus_group = str_detect(taxon, "\\.spp?$"))
}
floral_tab <- bind_rows(lapply(cities, read_floral))

match_floral <- function(p, city) {
  ft <- floral_tab[floral_tab$city == city, ]
  k <- norm(p)
  i <- which(ft$key == k)
  if (length(i) == 1) return(c(ft$floral_units[i], "exact", ft$taxon[i]))
  i <- which(startsWith(ft$key, k))
  if (length(i) == 1) return(c(ft$floral_units[i], "grouped", ft$taxon[i]))
  i <- which(ft$genus_group & ft$genus == genus_of(p))
  if (length(i) == 1) return(c(ft$floral_units[i], "genus", ft$taxon[i]))
  c(NA, NA, NA)
}

plants_city <- records |> count(city, plant, name = "n_visits") |>
  rowwise() |>
  mutate(m = list(match_floral(plant, city)),
         floral_units = as.numeric(m[1]), floral_source = m[2], floral_taxon = m[3]) |>
  ungroup() |> select(-m)

#------------------------------------------------------------------
# 5. Summaries and save
#------------------------------------------------------------------
describe <- function(L) {
  tibble(network = names(L),
         n_plants = sapply(L, nrow), n_insects = sapply(L, ncol),
         n_visits = sapply(L, sum), n_links = sapply(L, function(M) sum(M > 0)),
         connectance = round(n_links / (n_plants * n_insects), 3))
}

cat("\n== Networks per city ==\n");     print(describe(net_city), n = Inf)
cat("\n== Networks per land use ==\n"); print(describe(net_landuse), n = Inf)
cat("\n== City x land use ==\n");       print(describe(net_city_landuse), n = Inf)

cat("\n== Plant status (one count per plant taxon) ==\n")
print(table(plants$status, plants$status_source, useNA = "ifany"))
cat("Share of visits to plants with a status:",
    round(100 * mean(!is.na(records$status))), "%\n")
cat("Plants without status (most visited):\n")
print(plants |> filter(is.na(status)) |> arrange(desc(n_visits)) |> head(15))

cat("\n== Insect order (one count per insect taxon) ==\n")
print(table(insects$order, insects$taxo_source, useNA = "ifany"))
cat("Share of visits by insects with an order:",
    round(100 * mean(!is.na(records$order))), "%\n")
print(table(insects$group, useNA = "ifany"))

cat("\n== Floral abundance of the visited plants, per city ==\n")
print(plants_city |> group_by(city) |>
        summarise(plants = n(), with_count = sum(!is.na(floral_units)),
                  exact = sum(floral_source %in% "exact"), grouped = sum(floral_source %in% "grouped"),
                  genus = sum(floral_source %in% "genus"), zero = sum(floral_units %in% 0),
                  visits_covered_pct = round(100 * sum(n_visits[!is.na(floral_units)]) / sum(n_visits))))

saveRDS(list(records = records, plants = plants, insects = insects, plants_city = plants_city,
             net_city = net_city, net_landuse = net_landuse,
             net_city_landuse = net_city_landuse),
        file.path(dir_data, "baldock2019_networks.rds"))
cat("\nSaved:", file.path(dir_data, "baldock2019_networks.rds"), "\n")
