# Partition of the 6 main land-use networks (pooled over the 4 cities, binary), without
# Other greenspace, Pavement and Man-made surface (small networks), with the iid and pirho models.
# Run from the code folder (UrbanNetworks_Baldock_code):  Rscript scripts/fit_colSBM_landuse6_clustering.R
suppressMessages(library(colSBM))
baldock <- readRDS("data_Baldock2019/baldock2019_networks.rds")
landuses6 <- c("Allotment", "Cemetery", "Garden", "Nature reserve", "Park", "Verge")
net_bin <- lapply(baldock$net_landuse[landuses6], function(M) 1 * (M > 0))
opts <- list(nb_cores = max(1, parallel::detectCores() - 1), verbosity = 1, plot_details = 0)
for (model in c("iid", "pirho")) {
  file <- paste0("results_analysis/results_landuse6_colBiSBM_clustering_", model, ".rds")
  if (file.exists(file)) next
  t0 <- Sys.time(); set.seed(1)
  clu <- clusterize_bipartite_networks(netlist = net_bin, colsbm_model = model,
                                       net_id = names(net_bin), nb_run = 3, global_opts = opts)
  saveRDS(clu, file); message("== clustering ", model, " done in ", format(Sys.time() - t0))
}
