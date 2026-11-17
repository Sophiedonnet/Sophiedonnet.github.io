# Baldock et al. (2019): urban plant-pollinator networks in 4 UK cities

Baldock K.C.R., Goddard M.A., Hicks D.M. et al. (2019). A systems approach reveals urban
pollinator hotspots and conservation opportunities. *Nature Ecology & Evolution* 3: 363–373.
doi:10.1038/s41559-018-0769-y

Design: 4 cities (Bristol, Edinburgh, Leeds, Reading) × 9 land uses (allotment, cemetery,
garden, man-made surface, nature reserve, other greenspace, park, pavement, road verge),
10 sites per land use and city (360 sites), each sampled 3 times: twice between May and
September 2012, once between April and September 2013.

## Files

Downloaded on 2026-11-05 from the Supplementary Information of the paper
(`https://static-content.springer.com/esm/art%3A10.1038%2Fs41559-018-0769-y/MediaObjects/41559_2018_769_MOESM<k>_ESM.xlsx`).
They are renamed here, and their content is unchanged.

| File | Original | Content |
|---|---|---|
| `SuppData5_interactions.xlsx` | MOESM11 | **One row per visit** (site, plant, insect, specimen), one sheet per city; sheet `Size_categories`: insect order, family, body size (ITD) for the measured taxa |
| `SuppTable8_plant_status.xlsx` | MOESM6 | Status of 246 plant taxa (Native / Non-native / Native & non-native), null-model SES by city |
| `SuppTable7_null_model_by_city.xlsx` | MOESM5 | Same by city, with neophyte / archaeophyte detail |
| `SuppData1_abundance_by_site.xlsx` | MOESM7 | Pollinator and floral abundance / richness per site |
| `SuppData3_floral_abundance.xlsx` | MOESM9 | Floral abundance per land use × round (insect names hidden in the visitor part) |
| `SuppData4_landuse_areas.xlsx` | MOESM10 | Land-use areas per city, city-scale estimates |
| `dore_baldock-interaction.csv` | Compilation of Doré et al. (2021), see below | The same 4 city networks, with insect and plant taxonomy; used only to fill insect order / family |

## Built networks

`../scripts/01_build_networks_Baldock2019.R` (run from the code folder) writes `baldock2019_networks.rds` (records, plant and
insect tables, count matrices by city, by land use and by city × land use).

Choices:
- 7 visits without plant name are removed (4,921 visits remain).
- **Plant status** is the authors' status, defined as *non-native* = archaeophyte or neophyte in
  PLANTATT (Hill et al. 2004). It is matched by name, then through the taxa that the authors
  grouped, then through their genus groups (`Genus spp.`). 93 % of the visits get a status.
  Column `status_source` says how each status was obtained. Statuses coming from a genus group
  can be wrong for a single species (e.g. *Symphytum officinale* is native).
- **Insect order / family**: from `Size_categories`, then the Doré file, then the genus, plus
  4 taxa by hand. Every visit has an order. `group` = bee / hoverfly / other Diptera /
  beetle / butterfly-moth / other Hymenoptera.

## Insect taxonomy

`dore_baldock-interaction.csv` is taken from the compilation of pollination networks of

Doré M., Fontaine C., Thébault E. (2021). Relative effects of anthropogenic pressures, climate,
and sampling design on the structure of pollination networks at the global scale.
*Global Change Biology* 27(6): 1266–1280. doi:10.1111/gcb.15474
