
# Environment -------------------------------------------------------------

pacman::p_load(dplyr, tidyr, stringr, smutilities)

# setoptions()
wd <- here::here()
dd <- here::here("data")

# Load Data ---------------------------------------------------------------

res_name <- "pkg_data.rds"
res_path <- file.path(dd, res_name)
res <- readRDS(res_path)

# Process -----------------------------------------------------------------

View(dupepkgs)

View(missing_cran)
install_missing_cran(pkgs)

View(missing_github)
install_missing_github(missing_github)

# Save --------------------------------------------------------------------

res$pkgs_df <- df_master2
saveRDS(res, res_path)
