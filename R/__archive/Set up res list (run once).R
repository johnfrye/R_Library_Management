
# Environment -------------------------------------------------------------

rm(list=ls())
setoptions()
wd <- getwd()
ad <- setdirs()[1]
dd <- setdirs()[2]
gd <- setdirs()[3]
rptdir <- fp(wd, "reports")
wd

ses.info <- sessionInfo()
time <- time_stamp()

# packages ----------------------------------------------------------------

pkgs = c("pbapply", "httr", "dplyr",  "devtools")
inst = lapply(pkgs, library, character.only = TRUE)

# load data --------------------------------------------------------------------

libpaths <- .libPaths()
df <- dff(installed.packages(lib.loc=c(libpaths))[,c(1:4)])
df$Priority <- as.character(df$Priority)

df <- arrange(df, Package, Version)
dupepkgs <- dff(package=df$Package[duplicated(df$Package)])
dupepkgs_df <- df %>% 
  filter(Package %in% dupepkgs$package)
View(dupepkgs_df)

# identify source for each installed package ----------------------------------

source(fp(wd, "R", "utils.r"))
pkgs <- data.frame(installed.packages(lib.loc=c(libpaths)), stringsAsFactors = FALSE)
pkgs <- pkgs$Package
pkgs <- sort(pkgs)
desc <- lapply(pkgs, packageDescription)
version <- vapply(desc, function(x) x$Version, character(1))
date <- vapply(desc, pkg_date, character(1))
source <- vapply(desc, pkg_source, character(1))
pkgs_df <- data.frame(package = pkgs, version = version, 
                      date = date, source = source, stringsAsFactors = FALSE, 
                      check.names = FALSE)
rownames(pkgs_df) <- NULL
class(pkgs_df) <- c("dtupdate", "data.frame")

dim(pkgs_df)
# [1] 2370   4
View(pkgs_df)

res <- list(time = time,
            pkgs_df =  pkgs_df)
saveRDS(res, fp(dd, "pkg_data.rds"))