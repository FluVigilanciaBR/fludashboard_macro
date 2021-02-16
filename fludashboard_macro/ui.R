# Packages that will be loaded for this application
shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'shinycssloaders',
                      'shinydashboard',
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

createTabPanel <- function(mapName, predName, trendName) {
    map <- shinydashboard::box(
        leafletOutput(mapName, height = 640),
        title = "Tendência a longo prazo",
        width = 12,
        solidHeader = F,
        status = "primary"
    )
    sidepanel <- tagList(
        shinydashboard::box(
            title = "Predição",
            status = "primary",
            width = 12,
            height = 360,
            solidHeader = F,
            plotOutput(predName)),
        shinydashboard::box(
            title = "Tendência",
            status = "primary",
            width = 12,
            height = 300,
            solidHeader = F,
            plotOutput(trendName))
    )
    panel <- fluidRow(column(6, map),
                      column(6, sidepanel))
    panel
}


addContent <- function(contentDiv){

    panelMacro <- createTabPanel("mapBrazilMacro",
                                 "castingPlot",
                                 "trendPlot")
    panelCapitals <- createTabPanel("mapBrazilCapitais",
                                    "castingCapitaisPlot",
                                    "trendCapitaisPlot")
    panelUF <- createTabPanel("mapBrazilUFs",
                              "castingUFsPlot",
                              "trendUFsPlot")

    capitais_radio <- radioButtons(
        "adm",
        "Selecione o tipo de informação",
        choiceNames = list("PADRÃO",
                        "ADM PÚBLICA",
                        "ENT. EMPRESARIAS",
                        "ENT. SEM FINS LUCR."),
        choiceValues = list("",
                         "ADM PÚBLICA",
                         "ENT. EMPRESARIAS",
                         "ENT. SEM FINS LUCR."),
        inline = TRUE
    )
    capitais_checkbox_box <- fluidRow(
        column(12, shinydashboard::box(width = 12,
                                       height = 70,
                                       solidHeader = T,
                                       capitais_radio)
    ))
    panelCapitals <- tagList(capitais_checkbox_box, panelCapitals)

    tabs <- tabsetPanel(
        type = "tabs",
        tabPanel("Unidades Federativas", panelUF),
        tabPanel("Capitais", panelCapitals),
        tabPanel("Macrorregiões Saúde", panelMacro)
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
    .box {
        position: relative;
        border-radius: 3px;
        background: #fff;
        border-top: 3px solid #d2d6de;
        margin-top: 10px;
        margin-bottom: 10px;
        width: 100%;
        box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
    }
    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
        background-color: #ecf0f5!important;
    }
    .box.box-solid {
        border-top: 0;
    }
    .box-header {
        color: #444;
        display: block;
        padding: 5px;
        position: relative;
    }
    .box-title {
        display: inline-block;
        line-height : 1;
    }
    .box.box-primary{
        border-top-color: #337ab7;
    }
    .tab-content {
        background: #ecf0f5!important;
    }
    .navbar-brand-custom {
        font-size: 2rem;
        line-height: 1;
        color: #fff;
        font-weight: bolder;
    }

    .shiny-options-group input[type=radio]{
        margin: 0px -20px;
    }
    .shiny-input-container-inline{
        padding: 15px;
    }
    .shiny-input-container-inline>label{
        display: table-cell;
        padding-bottom: 10px;
        line-height:1.5;
    }
    img.align-right, .figure.align-right, object.align-right {
        float: right;
        margin-left: 1em;
    }
')

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
