# R Script to render website from RShiny Code



# ----------- Notes -------------------#

# For now, render website individually using the "RUnApp" Button inside RShiny
# Then once completed, use the render_website scritpt and change all file paths and format

# Libraries
library(httpuv)
library(shinylive)

# 1. Delete previous 'docs' folder
unlink("docs", recursive = TRUE)

# 2. Load homepage
shinylive::export(appdir = "Applets/Home Page", destdir = "docs")

# 3. Load all subpages
subpages <- c("Tutorial", "Logistic Regression", "Decision Trees", "Cross Validation", "Bootstrapping")

for (subpage in subpages){
  shinylive::export(appdir = paste("Applets/",subpage, sep = ""), destdir = "docs", subdir = subpage)
}

# 4. Run Website
httpuv::runStaticServer("docs")

