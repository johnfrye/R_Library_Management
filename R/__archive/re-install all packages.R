
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

df <- dff(installed.packages()[,c(1:4)])
df$Priority <- as.character(df$Priority)

df <- arrange(df, Package, Version)
dupepkgs <- dff(package=df$Package[duplicated(df$Package)])
dupepkgs_df <- df %>% 
  filter(Package %in% dupepkgs$package)
View(dupepkgs_df)

# identify source for each installed package ----------------------------------------

source(fp(wd, "R", "utils.r"))
libpaths <- .libPaths()
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
View(pkgs_df)
dim(pkgs_df)
# [1] 2362   4

# check vs alternate lib --------------------------------------------------------

missing <- pkgs_df
View(missing)

# drop any packages you don’t want installed on the current machine -------
# using grep here rather than 'which' because some package names change
# with the version (rare github cases, ideally the devs change this practice)

drop <- grep("adehabitat", missing$package)
drop <- c(drop, grep("RevoUtilsMath", missing$package))
if(length(drop)>0) missing <- missing[-drop, ]

# Install missing CRAN packages -------------------------------------------

# this will only install those packages hosted on CRAN -
# the others will be handled below
missing_cran <- missing[grep("CRAN", missing$source), ]
View(missing_cran)
pkgs <- paste(missing_cran$package, sep="")
length(pkgs)
install.packages(pkgs[101:200], dependencies = TRUE)

# process missing GitHub packages -----------------------------------------

missing_github <- missing[grep("Github", missing$source), ]
View(missing_github)

just_repo <- str_match(missing_github$source,
                       "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
missing_github$gh_version <- get_versions(just_repo)
k <- missing_github$package

# Install from GitHub -----------------------------------------------------

if (length(k) > 0) {
  
  invisible(apply(missing_github, 1, function(p) {
    just_repo <- str_match(p["source"],
                           "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
    # this will prevent package install errors from stopping the function
    try(devtools::install_github(just_repo, dependencies=TRUE))
  }))
}
  