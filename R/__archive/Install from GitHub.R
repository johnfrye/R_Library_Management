
# Install from GitHub -----------------------------------------------------

library(devtools)
# proxy is for SM firewall only
set_config(use_proxy("172.20.4.200: 8080"))


df <- read.xlsx2(file = "~/Package Management/library management/data/github packages.xlsx", sheetIndex = 1)
df <- select(df, -LibPath, -Version, -Priority)
df$order <- 1:nrow(df)
a <- dff(str_locate(df$git.dir, "/"))
df$pkg.name <- str_sub(df$git.dir, start = a$end + 1, end = nchar(df$git.dir))
df$author <- str_sub(df$git.dir, start = 1, end = a$end - 1)
a <- dff(str_locate(df$pkg.name, "@"))
chg <- which(!is.na(a$start))
if(length(chg)>0) df$pkg.name[chg] = str_sub(df$pkg.name[chg], start = 1, end = (a$start[chg]-1))
df$htmlwidget <- 0
df$pkg.name <- gsub("anomalous-acm", "anomalousACM", df$pkg.name)
df$pkg.name <- gsub("D3ManhattanPlots", "manhattanPlot", df$pkg.name)
df$pkg.name <- gsub("mapshaper_htmlwidget", "mapshaperWidget", df$pkg.name)
htmlwidget.pkg <- grep("htmlwidget", df$desc, ignore.case = T)
df$htmlwidget[htmlwidget.pkg] <- 1
htmlwidget.pkg <- grep("htmlwidget", df$pkg.name, ignore.case = T)
df$htmlwidget[htmlwidget.pkg] <- 1
df2 <- dff(installed.packages()[,c(1:4)])
rownames(df2) <- NULL
df2 <- arrange(df2, Package)
WriteXLS('df2', 
         fp("~/Package Management/library management/data/installed packages-work.xlsx"),
         row.names = F, col.names = T, AdjWidth = T, FreezeRow = 1, BoldHeaderRow = T)
# View(df2)
df2$Priority <- as.character(df2$Priority)
df2 <- rename(df2, pkg.name=Package)
df <- left_join(df, df2, by="pkg.name")
View(df)
df <- arrange(df, LibPath)

pkgs <- paste(df$git.dir, sep="")
length(pkgs)

devtools::install_github(pkgs[120], dependencies=TRUE)

df <- arrange(df, pkg.name)
df$order <- 1:nrow(df)
(dupepkg <- df$pkg.name[duplicated(df$pkg.name)])

write.xlsx2(df, "D:/R Working Directory/Package Management/library management/data/github packages.xlsx", row.names = F)

devtools::install_github(c('hadley/productplots', 'hadley/densityvis'))
devtools::install_github('cmpolis/datacomb', subdir='pkg', ref='1.1.2')
devtools::install_github("hadley/ggplot2", dependencies=TRUE, build_vignettes = TRUE)
devtools::install_github("", dependencies=TRUE, build_vignettes = TRUE)
devtools::install_github("", dependencies=TRUE, build_vignettes = TRUE)
devtools::install_github("", dependencies=TRUE, build_vignettes = TRUE)