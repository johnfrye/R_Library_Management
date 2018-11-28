# Reinstall all packages

LibLoc<-.libPaths()
Pkg<-list.files(LibLoc[1])

install.packages(Pkg,dependencies=TRUE)

# Update the start and restart R when max dynamic libraries loaded.

for(iPkg in 281:length(Pkg)){  
 library(Pkg[iPkg],character.only = TRUE)
}

