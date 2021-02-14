# Packages that will be loaded for this application
shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'shinycssloaders',
                      'leaflet',
                      'sf')

new.packages <- list.of.packages[!(list.of.packages %in%
                                       installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)

for (package in list.of.packages) {
    shhh(library(package, character.only = TRUE))
}
options(bitmapType = "cairo")

getNavBar <- function() {
    navbar <- tags$div(
        tags$a(
            tags$strong("Monitoramento de casos de síndrome respiratória aguda grave (SRAG) notificados no SIVEP-Gripe"),
            class="navbar-brand-custom", href=""
        ),
        class = "navbar-header", style = "margin: 5px"
    )
}

addHeader <- function(contentDiv) {
   headerContent <- includeHTML(here("fludashboard_macro/www/super-header.html"))
   headerContent <- tags$div(
       tagList(headerContent, getNavBar()),
       class = "navbar navbar-dark bg-primary",
       role = "navigation"
   )
   contentDiv <- tagAppendChild(contentDiv, headerContent)
}

addContent <- function(contentDiv){
    mapBrazil <- leafletOutput("mapBrazil", height = 700)
    sidepanel <- tagList(
        tags$h1(textOutput("MACSAUD")),
        tags$div(plotOutput("trendPlot"))
    )
    panel <- fluidRow(column(6, mapBrazil), column(6, sidepanel))
    tabs <- tabsetPanel(
        type = "tabs",
        tabPanel(
            "Macrorregiões Saúde",
            panel
        ),
        tabPanel("Capitais", tags$div())
    )
    contentDiv <- tagAppendChild(contentDiv, tabs)
}

addFooter <- function(contentDiv) {
    contentDiv <- tagAppendChild(contentDiv, div(class = 'row-fluid',
                                                 id = "footer-brasil"))
}



getContent <- function() {
    contentDiv <- div(class = "container-fluid") %>%
        addHeader() %>%
        addContent() %>%
        addFooter()
}
headerStyle <- HTML('
    html, document, body {
        margin: 0!important;
        padding: 0!important;
        background: #dfdfdf;
        line-height: 0.8;
    }
    .tab-content {
        background: white!important;
    }
    .navbar-brand-custom {
        font-size: 2rem;
        line-height: 1;
        color: #fff;
        font-weight: bolder;
    }
    img.align-left, .figure.align-left, object.align-left {
        float: left ;
        margin-right: 1em }

    img.align-right, .figure.align-right, object.align-right {
        float: right ;
        margin-left: 1em }')

headerStyle <- tagList(tags$style(headerStyle),
                      tags$link(rel="stylesheet",
                                href="http://www.ensp.fiocruz.br/portal-ensp/_estilos/ensp_barra-fiocruz.css",
                                type="text/css",
                                media="all"))
headerStyle <- tags$head(headerStyle)

shinyUI(bootstrapPage(headerStyle,
                      getContent(),
                      tags$script(defer="defer",
                                  src="//barra.brasil.gov.br/barra_2.0.js",
                                  type="text/javascript")))
