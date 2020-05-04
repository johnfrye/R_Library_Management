
# Environment -------------------------------------------------------------

pacman::p_load(dplyr, pbapply, tidyr, stringr, fryeR)

# pacman::p_load(stats, devtools, purrr, magrittr, httr,
#                readxl, openxlsx, data.table, reshape2,
#                dplyr, stringr, stringi, rio, WorksheetFunctions,
#                tidyr, knitr, pbapply, fryeutilities)

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

# compare -----------------------------------------------------------------

libpaths <- .libPaths()

df <- as.data.frame(installed.packages(lib.loc=c(libpaths))[,c(1:4)]) %>% 
  mutate(Priority = as.character(Priority)) %>% 
  arrange(Package, Version)
# View(df)

dupepkgs <- dff(package=df$Package[duplicated(df$Package)])
dupepkgs_df <- df %>% 
  filter(Package %in% dupepkgs$package)
# View(dupepkgs_df)

# identify source for each installed package ----------------------------------

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

dim(pkgs_df)
# [1] 3459  4

# check vs master list --------------------------------------------------------

# identify which packages are installed in both libraries,
# and which are installed in only one.

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

dupepkgs <- df_master2[duplicated(df_master2$package), ]
View(dupepkgs)

res$pkgs_df <- df_master2
saveRDS(res, res_path)

# compare vs updated master -----------------------------------------------

df_master <- res$pkgs_df
df_all <- df_master %>% 
  rbind(df_this_machine) %>% 
  select(package, library_name) %>% 
  reshape2::dcast(formula=package~library_name, length, value.var = "library_name") %>% 
  mutate(complete = comparison + this_machine)

# filter to those missing from the current machine's library
missing0 <- df_all %>% 
  filter(complete < 2, this_machine == 0) %>% 
  arrange(complete, package)

# drop any packages you don’t want installed on the current machine -------
# using grep here rather than 'which' because some package names change
# with the version (rare github cases, ideally the devs change this practice)

drop_pkgs <- c('adehabitat', 'ade4TkGUI', 'littler', 'metScanR', 'RnavGraph', 
               'narnia', 'fstrings', 'Gmisc', 'RDocumentation', 'taskscheduleR', 
               'stmgui', 'SimpleTable', 'betareg', 'rggobi', 'clusterfly', 
               'pcaL1', 'reinstallr', 'gpuR', 'strptimer', 'randomForest.ddR',
               'ddR', 'tkrplot', 'ConvergenceConcepts', 'COveR', 'datacheckr',
               'genderNaRmes', 'FGN', 'mixer', 'R2Cuba', 'rgp', 'ripums', 'RMongo',
               'rsam', 'rPython', 'genderNames', 'rite', 'backtestGraphics',
               'Kodama', 'msgtools', 'PythonInR', 'ANLP', 'anonymizer',
               'ascii', 'AutoModel', 'aws.polly', 'BayesBridge', 'bigalgebra',
               'bigRR', 'boclust', 'boxoffice', 'CALIBERrfimpute', 'CAM',
               'autopls', 'bikedata', 'camel', 'classify', 'CluMix', 'clustergas', 'comparer', 'correlate', 'covTest',
               'crandatapkgs', 'DAAG', 'DAAGxtras', 'Daim', 'darch', 'data360r', 'DatABEL', 'datadr', 'DeducerExtras',
               'dendextendRcpp', 'dicecrawler', 'docker', 'doMC', 'dprep', 'dtables', 'easyformatr', 'easyml', 'edgarWebR',
               'ElemStatLearn', 'elmNN', 'Emcdf', 'EnQuireR', 'ensembleEN', 'epitable', 'ExtDist', 'extracat', 'ezsummary',
               'FactoRizationMachines', 'fanovaGraph', 'fArma', 'FastRWeb', 'FCNN4R', 'fheatmap', 'fifer', 'formulize', 'FRB',
               'frequencies', 'Funclustering', 'funcy', 'GAR', 'gcExplorer', 'GenABEL', 'GenABEL.data', 'genderdata',
               'genderizeR', 'geo', 'geocodeHERE', 'geomnet', 'ggFacetSample', 'ggsubplot', 'ghit',
               'grpregOverlap', 'hei', 'HIBPwned', 'infuser', 'INLAutils', 'ionicons', 'IPSUR', 'IPtoCountry', 'ITEMAN',
               'its', 'itunesr', 'jug', 'kerasformula', 'Kmisc', 'KoNLP', 'latticeDensity', 'ldstatsHD', 'learningCurve',
               'letsR', 'lettercase', 'listless', 'lqa', 'LSAmitR', 'lsl', 'lsmeans', 'lspls', 'lucr', 'MeanShift',
               'mbgraphic', 'maxent', 'MAR1', 'mixOmics', 'MonetDBLite', 'mRm', 'MTurkR', 'mvtboost', 'playwith', 'pmg',
               'ncdf', 'neurovault', 'NNLM', 'oc', 'odfWeave', 'onlineCPD', 'AntMAN', 'aslib', 'BradleyTerryScalable',
               'bagRboostR', 'BrailleR', 'OutbreakTools', 'pkgcopier', 'Prototest', 'Radiant')



# (p1 <- paste(pkgs[1:59], sep = "'", collapse = "', '"))

missing2 <- missing0 %>% 
  filter(!package %in% drop_pkgs)

missing <- left_join(missing2, df_master, by="package") %>% 
  select(package, version, date, source)
# View(missing)

# Install missing CRAN packages -------------------------------------------

# this will only install those packages hosted on CRAN -
# the others will be handled below
missing_cran <- missing[grep("CRAN", missing$source), ]
if(nrow(missing_cran) > 0 ) missing_cran <- dff(index = 1:nrow(missing_cran), missing_cran)
# (type_sel <- c("mac.binary.mavericks", "source")[2])
pkgs <- paste(missing_cran$package, sep="")

# View Missing CRAN -------------------------------------------------------

# paste(missing_cran$package[1:24], collapse = "', '")

View(missing_cran)

(maxlen <- c(50, length(pkgs))[1])

for (i in 23:maxlen){
# for (i in 11:length(pkgs)){
  install.packages(pkgs[i], dependencies = TRUE)
  print(paste0(i, " of ", maxlen))
  Sys.sleep(2)
}

# Try Bioconductor for packages not on CRAN -------------------------------

mp <- missing_cran %>% 
  pull(package)
source("http://bioconductor.org/biocLite.R")
biocLite(mp)

# process missing GitHub packages -----------------------------------------

missing_github <- missing[grep("Github", missing$source), ] %>% 
  filter(!package %in% c("gambin2", "NutrientData", "eAnalytics",
                         "plotcon", "githubinstall", 'ashr',
                         'bettertrace', 'colformat', 'd3', 'datacomb',
                         'datapkg', 'easyRFM', 'edarf', 'evalg', 'GeomComb',
                         'easyRFM', 'mason.rpkg', 'marketeR', 'staticdocs')) %>% 
  filter(!package %in% pkgs)

View(missing_github)

just_repo <- str_match(missing_github$source,
                       "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
missing_github$gh_version <- get_versions(just_repo)
k <- missing_github$package

# Install from GitHub -----------------------------------------------------

# for (i in 1:length(k)){
for (i in 1:3){
  invisible(apply(missing_github, 1, function(p) {
    just_repo <- str_match(p["source"],
                           "\\(([[:alnum:]-_\\.]*/[[:alnum:]-_\\.]*)[@[:alnum:]]*")[,2]
    # this will prevent package install errors from stopping the function
    try(devtools::install_github(just_repo[i], dependencies=TRUE))
    Sys.sleep(2)
  }))
}

install_git('ellisp/forecastxgb')
# Look for CRAN versions of github packages -------------------------------

for (i in 1:length(k)){  
    install.packages(missing_github$package[i], dependencies = TRUE)
  print(paste0(i, " of ", length(pkgs)))
  Sys.sleep(2)
}

