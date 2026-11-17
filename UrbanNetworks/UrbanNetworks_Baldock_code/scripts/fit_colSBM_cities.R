# Fit the 4 colBiSBM variants on the 4 binary city networks (cached in results_analysis/).
# Run from the code folder (UrbanNetworks_Baldock_code):  Rscript scripts/fit_colSBM_cities.R
suppressMessages(library(colSBM))
baldock <- readRDS("data_Baldock2019/baldock2019_networks.rds")
net_bin <- lapply(baldock$net_city, function(M) 1 * (M > 0))
for (model in c("iid", "pi", "rho")) {  # pirho not needed: pi and rho are rejected
  file <- paste0("results_analysis/results_city_colBiSBM_", model, ".rds")
  if (file.exists(file)) next
  t0 <- Sys.time()
  set.seed(1)
  fit <- estimate_colBiSBM(netlist = net_bin, colsbm_model = model, net_id = names(net_bin),
                           distribution = "bernoulli", nb_run = 3,
                           global_opts = list(nb_cores = max(1, parallel::detectCores() - 1),
                                              verbosity = 1, plot_details = 0))
  saveRDS(fit, file)
  cat("\n==", model, "done in", format(Sys.time() - t0), "\n")
}
