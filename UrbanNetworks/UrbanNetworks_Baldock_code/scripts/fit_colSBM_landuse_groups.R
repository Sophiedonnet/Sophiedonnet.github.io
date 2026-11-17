# iid colBiSBM on two groups of land-use networks (pooled over the 4 cities, binary):
#   greenspaces = Cemetery, Nature reserve, Park, Verge
#   gardens     = Allotment, Garden
# Run from the code folder (UrbanNetworks_Baldock_code):  Rscript scripts/fit_colSBM_landuse_groups.R
suppressMessages(library(colSBM))
baldock <- readRDS("data_Baldock2019/baldock2019_networks.rds")
groups <- list(greenspaces = c("Cemetery", "Nature reserve", "Park", "Verge"),
               gardens     = c("Allotment", "Garden"))
opts <- list(nb_cores = max(1, parallel::detectCores() - 1), verbosity = 1, plot_details = 0)
for (g in names(groups)) {
  file <- paste0("results_analysis/results_landuse_", g, "_colBiSBM_iid.rds")
  if (file.exists(file)) next
  net_bin <- lapply(baldock$net_landuse[groups[[g]]], function(M) 1 * (M > 0))
  t0 <- Sys.time(); set.seed(1)
  fit <- estimate_colBiSBM(netlist = net_bin, colsbm_model = "iid", net_id = names(net_bin),
                           distribution = "bernoulli", nb_run = 3, global_opts = opts)
  saveRDS(fit, file); message("== ", g, " done in ", format(Sys.time() - t0))
}
