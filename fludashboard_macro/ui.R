
# Packages that will be loaded for this application
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'shinycssloaders',
                      'bootstraplib')
new.packages <- list.of.packages[!(list.of.packages %in%
                                       installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)

# Importar pacotes necessário
for (package in list.of.packages) {
    library(package, character.only = TRUE)
}
options(bitmapType = "cairo")

addHeader <- function(contentDiv){
    header <- includeHTML('www/super-header.html')
    contentDiv <- tagAppendChild(contentDiv, div(class='row', header))
}

addFooter <- function(contentDiv){
    contentDiv <- tagAppendChild(contentDiv, div(class='row-fluid',
                                                 id="footer-brasil"))
}

getContent <- function(){
    contentDiv <- div(class="container-fluid") %>%
        addHeader() %>%
        addFooter()
}

shinyUI(
    bootstrapPage(
        title = "Infogripe",
        getContent()
    )
)
