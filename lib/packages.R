reqpackages <- c('plyr',
                 'tidyverse',
                 'stringr',
                 'reshape2',
                 'broom',
                 'ggplot2',
                 'readxl',
                 'rjags')

for(pkg in reqpackages){
  if(!(pkg %in% row.names(installed.packages()))){
    install.packages(pkg, repos='http://cran.rstudio.com')
  }
}

options(defaultPackages = c(getOption('defaultPackages'),reqpackages))
for(pkg in reqpackages){
  eval(substitute(suppressMessages(library(x)), list(x=pkg)))
}
