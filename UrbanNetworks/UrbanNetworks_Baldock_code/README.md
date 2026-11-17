# Urban pollination networks with stochastic block models

R companion of the 3-hour course by Sophie Donnet (INRAE, MIA Paris-Saclay) at the school
"Biodiversité urbaine : enjeux et modélisation" (IHP, Paris, 25-27 November 2026).
Slides: https://sophiedonnet.github.io/urban_networks.html

Running example: Baldock et al. (2019), Nature Ecology & Evolution 3: 363-373
(4 UK cities x 9 land uses).

## Content

- `companion_slides_UK.qmd`: R companion of the slides.
- `data_Baldock2019/`: the supplementary files of Baldock et al. (2019) (see its `README.md`)
  and `baldock2019_networks.rds`, the networks used by the companion.
- `scripts/01_build_networks_Baldock2019.R`: builds `baldock2019_networks.rds` from the
  supplementary files (visit records, plant status, insect groups).
- `scripts/fit_colSBM_*.R`: R scripts to launch the long colSBM estimations outside of the companion.
- `results_analysis/`: cached estimation results (empty in the code zip, see below).

## Estimation results

The fitted models are cached in `results_analysis/`. Download
`UrbanNetworks_Baldock_results.zip` from the page above and unzip it in this folder:
the companion then reads the fits instead of re-estimating them (some colSBM fits take
about 30 min each). Delete a file to re-estimate it.

## Running

From this folder: `quarto render companion_slides_UK.qmd`.
To rebuild the networks: `Rscript scripts/01_build_networks_Baldock2019.R` (needs `readxl`, `stringr`).
R packages: dplyr, tidyr, ggplot2, patchwork, sbm, colSBM (>= 0.4.11, GitHub GrossSBM/colSBM),
bipartite, vegan.
