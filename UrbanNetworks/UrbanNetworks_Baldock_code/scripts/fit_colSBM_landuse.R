# colSBM on the 9 land-use networks (pooled over the 4 cities), binary.
# Run from the code folder (UrbanNetworks_Baldock_code):  Rscript scripts/fit_colSBM_landuse.R
suppressMessages(library(colSBM))
baldock <- readRDS("data_Baldock2019/baldock2019_networks.rds")
net_bin <- lapply(baldock$net_landuse, function(M) 1 * (M > 0))
opts <- list(nb_cores = max(1, parallel::detectCores() - 1), verbosity = 1, plot_details = 0)
for (model in c("iid", "pirho")) {   # iid kept only as a reference
  file <- paste0("results_analysis/results_landuse_colBiSBM_", model, ".rds")
  if (file.exists(file)) next
  t0 <- Sys.time(); set.seed(1)
  fit <- estimate_colBiSBM(netlist = net_bin, colsbm_model = model, net_id = names(net_bin),
                           distribution = "bernoulli", nb_run = 3, global_opts = opts)
  saveRDS(fit, file); message("== ", model, " done in ", format(Sys.time() - t0))
}
# Partition of the land uses into groups sharing the same structure.
# pirho: the land uses differ in composition and density, so iid is not appropriate
# (colBiSBM has no density-scaled "delta" model).
file <- "results_analysis/results_landuse_colBiSBM_clustering_pirho.rds"
if (!file.exists(file)) {
  t0 <- Sys.time(); set.seed(1)
  clu <- clusterize_bipartite_networks(netlist = net_bin, colsbm_model = "pirho",
                                       net_id = names(net_bin), nb_run = 3, global_opts = opts)
  saveRDS(clu, file); message("== clustering done in ", format(Sys.time() - t0))
}
