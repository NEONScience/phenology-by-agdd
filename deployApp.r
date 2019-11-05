#devtools::install_github('NEONScience/NEON-utilities/neonUtilities')
#install.packages("BiocManager")
#BiocManager::install("flowClust")
#options(repos = BiocManager::repositories())
#getOption("repos")

library(rsconnect)

if(file.exists('C:/Users/nrobinson')){
  setwd('C:/Users/nrobinson/Desktop/MyDocuments/NEON_Git/phenology-by-agdd')
}

rsconnect::setAccountInfo(name='neonproject',
                          token='D04FBB8D0B937E41D96A0AED0D92031C',
                          secret='gBEZOnPMvmuJC3m5gCWi3kHAIFZ1dVvjvZzuNZiG')

# shiny::runApp() #works to deploy locally
deployApp()
