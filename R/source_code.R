
# Environment -------------------------------------------------------------

pacman::p_load(dplyr, tidyr, stringr, smutilities)

# pacman::p_load(stats, devtools, purrr, magrittr, httr,
#                readxl, openxlsx, data.table, reshape2,
#                dplyr, stringr, stringi, rio, WorksheetFunctions,
#                tidyr, knitr, pbapply, smutilities)

# setoptions()
wd <- here::here()
ad <- here::here("analysis")
dd <- here::here("data")
gd <- here::here("graphics")

ses.info <- sessionInfo()
# time <- time_stamp()

# load data --------------------------------------------------------------------

res_name <- "pkg_data.rds"
res_path <- file.path(dd, res_name)
res <- readRDS(res_path)

# saveRDS(ses.info, file.path(ad, "Session Info.RDS"))

# compare -----------------------------------------------------------------

libpaths <- .libPaths()

df <- as.data.frame(installed.packages(lib.loc=c(libpaths))[,c(1:4)]) %>% 
  mutate(Priority = as.character(Priority)) %>% 
  arrange(Package, Version)

dupepkgs <- as.data.frame(package=df$Package[duplicated(df$Package)])
dupepkgs_df <- df %>% 
  filter(Package %in% dupepkgs$package)
# View(dupepkgs_df)

# identify source for each installed package ----------------------------------

package_list <- function(libpaths){
  source(file.path(wd, "R", "utils.r"))
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
}
pkgs_df <- package_list(libpaths)

# dim(pkgs_df)
# [1] 3459  4

# check vs master list --------------------------------------------------------

# identify which packages are installed in both libraries,
# and which are installed in only one.

master_check <- function(pkgs_df){
  df_master <- res$pkgs_df %>% 
    mutate(library_name="comparison")
  df_this_machine <- pkgs_df %>% 
    mutate(library_name="this_machine")
  
  df_all <- rbind(df_master, df_this_machine) %>% 
    select(package, library_name) %>% 
    reshape2::dcast(formula=package~library_name, length, value.var = "library_name") %>% 
    mutate(complete = comparison + this_machine)
  # View(df_all)
  
  missing_mas <- df_all %>% 
    dplyr::filter(comparison == 0) %>% 
    select(package) %>% 
    left_join(df_this_machine, by="package")
  
  df_master2 <- rbind(df_master, missing_mas) %>% 
    arrange(package, version, source) %>% 
    mutate(library_name = "comparison")
  # View(df_master2)
}

df_master2 <- master_check(pkgs_df)

check_dupes <- function(df_master2){
  dupepkgs <- df_master2[duplicated(df_master2$package), ]
}

dupepkgs <- check_dupes(df_master2)

# compare vs updated master -----------------------------------------------

df_master <- res$pkgs_df
df_all <- df_master %>% 
  rbind(df_this_machine) %>% 
  select(package, library_name) %>% 
  reshape2::dcast(formula=package~library_name, length, value.var = "library_name") %>% 
  mutate(complete = comparison + this_machine)

# filter to those missing from the current machine's library
missing <- df_all %>% 
  filter(complete < 2, this_machine == 0) %>% 
  arrange(complete, package)

# drop any packages you don’t want installed on the current machine -------
# using grep here rather than 'which' because some package names change
# with the version (rare github cases, ideally the devs change this practice)

drop_pkgs <- import(fp(dd, "drop_packages.xlsx"))
drop <- NULL
for(i in 1:nrow(drop_pkgs)){
  drop <- c(drop, grep(drop_pkgs$package, missing$package))
}

if(length(drop)>0) missing <- missing[-drop, ]
missing <- left_join(missing, df_master, by="package") %>% 
  select(package, version, date, source)

# Install missing CRAN packages -------------------------------------------

# this will only install those packages hosted on CRAN -
# the others will be handled below
missing_cran <- missing[grep("CRAN", missing$source), ]
missing_cran <- dff(index = 1:nrow(missing_cran), missing_cran)

# (type_sel <- c("mac.binary.mavericks", "source")[2])

pkgs <- paste(missing_cran$package, sep="")

install_missing_cran <- function(pkgs){
  for (i in 1:length(pkgs)){
    # install.packages(pkgs[i], dependencies = TRUE, type = type_sel)
    install.packages(pkgs[i], dependencies = TRUE)
    print(paste0(i, " of ", length(pkgs)))
    Sys.sleep(2)
  }
}

# process missing GitHub packages -----------------------------------------

missing_github <- missing[grep("Github", missing$source), ] %>% 
  filter(!package %in% c("gambin2", "NutrientData", "eAnalytics",
                         "plotcon", "githubinstall"))

install_missing_github <- function(missing_github){
  just_repo <- str_match(missing_github$source,
                         "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
  missing_github$gh_version <- get_versions(just_repo)
  k <- missing_github$package
  
  if (length(k) > 0) {
    invisible(apply(missing_github, 1, function(p) {
      just_repo <- str_match(p["source"],
                             "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
      # this will prevent package install errors from stopping the function
      try(devtools::install_github(just_repo, dependencies=TRUE))
      Sys.sleep(2)
    }))
  }
}
