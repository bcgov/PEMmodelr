require(data.table)
require(tidyverse)


xx <- readRDS("predictions_1.RDS")
fuzz <- fread("fuzzy_matrix_basic_updated.csv")
yy <- acc_metrics(xx, fuzz)
zz <- acc_plot_metrics(yy)
