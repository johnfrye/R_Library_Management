
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

# time <- time_stamp()
# machine <- paste(Sys.info()[1], Sys.info()[2], sep="_")
# (filename <- paste0("installed packages ", machine, "_", time, ".xlsx"))
# write.xlsx(pkgs_df, file=file.path(dd, filename), row.names = F)

# check vs alternate lib --------------------------------------------------------

# choose an alternate library package list
files <- fileTable(dd, search.pattern = "installed packages")
View(files)

# import alternate library package list
# warnings thrown by imports are inconsequential here
df_comparison_machine <- import(files$file_path[1])
df_this_machine <- pkgs_df

df_comparison_machine <- mutate(df_comparison_machine, library_name="comparison")
df_this_machine <- mutate(df_this_machine, library_name="this_machine")
df <- rbind(df_comparison_machine, df_this_machine)

# identify which packages are installed in both libraries,
# and which are installed in only one.
df <- select(df, package, library_name) %>% 
  dcast(formula=package~library_name, length, value.var = "library_name") %>% 
  mutate(complete = comparison + this_machine)

# filter to those missing from the current machine's library
missing <- df %>% 
  filter(complete < 2, this_machine == 0) %>% 
  arrange(complete, package)

# drop any packages you don’t want installed on the current machine -------
# using grep here rather than 'which' because some package names change
# with the version (rare github cases, ideally the devs change this practice)

drop <- grep("RevoUtilsMath", missing$package)
drop <- c(drop, grep("mixAK", missing$package))
if(length(drop)>0) missing <- missing[-drop, ]

missing <- left_join(missing, df_comparison_machine, by="package") %>% 
  select(package, version, date, source)
# View(missing)

# Install missing CRAN packages -------------------------------------------

# this will only install those packages hosted on CRAN -
# the others will be handled below
missing_cran <- missing[grep("CRAN", missing$source), ]
View(missing_cran)
pkgs <- paste(missing_cran$package, sep="")
length(pkgs)
install.packages(pkgs[1:50], dependencies = TRUE)

for (i in 1:length(pkgs)){
  install.packages(pkgs[i], dependencies = TRUE)
  Sys.sleep(2)
}

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
  