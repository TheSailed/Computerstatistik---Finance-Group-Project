
# ---- Initialsing R packages ----

packages <- c("quantmod", "ucimlrepo")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
library(quantmod)
library(ucimlrepo)

# ---- Loading the data ---- 

cc_default <- fetch_ucirepo(id = 350)
df <- cc_default$data$original


