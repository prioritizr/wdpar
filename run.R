# R CMD BATCH --no-save --no-restore run.R
library(devtools)
load_all()
options(error = function() {traceback();q(save ="no", status = 1)})
test_data_dir = "/mnt/GitHub/wdpar/test-data"
dir.create(test_data_dir, showWarnings = FALSE, recursive = TRUE)
tdata = land_and_eez_fetch("global", download_dir = test_data_dir, verbose = TRUE, threads = 12)
saveRDS(tdata, "global.rds", compress="xz")
