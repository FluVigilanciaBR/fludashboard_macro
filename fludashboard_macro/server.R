shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'shinycssloaders',
                      'leaflet')

new.packages <- list.of.packages[!(list.of.packages %in%
                                       installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)

# Importar pacotes necessário
for (package in list.of.packages) {
    shhh(library(package, character.only = TRUE))
}
options(bitmapType = "cairo")

source(here("fludashboard_macro/nowcasting_v2.R"))

# Shape info
macros_file <- "fludashboard_macro/geobrazilmacrosaude.RDS"
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    macsaud.id <- shiny::reactiveVal(0)
    output$mapBrazil <- renderLeaflet({
        macros_saude <- readRDS(here(macros_file))
        labels <- paste(macros_saude$DS_UF_SIGLA, "-", macros_saude$DS_ABREV_macsaud)
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = macros_saude$geom,
                fillColor="#ffffcc",
                weight = 2,
                color="#707070",
                fillOpacity = 0.7,
                layerId=macros_saude$CO_MACSAUD,
                highlight = highlightOptions(
                    weight = 5,
                    color="#666",
                    fillOpacity = 0.9,
                    bringToFront = TRUE),
                label = labels
            ) %>%
            setView(-50, -11, 4)
    })
    output$trendPlot <- renderPlot({
        if(length(macsaud.id()) == 0)
            return (NA);
        xbreaks <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
        xlimits <- c(1, 52)
        macro_name <- as.character(macros_saude %>%
                                   as.data.frame() %>%
                                   filter(CO_MACSAUD == macsaud.id()) %>%
                                   select(DS_ABREV_macsaud))
        uf <- as.character(macros_saude %>%
                           as.data.frame() %>%
                           filter(CO_MACSAUD == macsaud.id()) %>%
                           select(DS_UF_SIGLA))
        pred.srag.summy <- readRDS(here("fludashboard_macro/data/pred_srag_summy_1102.rds"))
        p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
          ylab("Incidência de SRAG (por 100mil hab.)") +
          xlab("Semana de primeiros sintomas") +
          scale_x_continuous(breaks = xbreaks, limits = xlimits) +
        #theme_Publication(base_size = 16, base_family = 'Roboto') +
          ggtitle(paste0(uf, ": ", macro_name)) +
          theme(plot.margin=unit(c(1,0,5,5), units='pt'),
                axis.text = element_text(size = rel(1)),
                legend.margin = margin(0,0,0,0, unit='pt'),
                legend.justification=c(0,1),
                legend.position=c(0.015, 1.05),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.key.size = unit(14, 'pt'),
                legend.text = element_text(family = 'Roboto', size = rel(1)))
        return(p.now.srag)
    }, height = "auto")
    output$MACSAUD <- renderText({
        if (length(macsaud.id()) == 0)
            text <- "Selecione uma macrorregião"
        else {
            macro_name <- as.character(macros_saude %>%
                                     as.data.frame() %>%
                                     filter(CO_MACSAUD == macsaud.id()) %>%
                                     select(DS_ABREV_macsaud))
            uf <- as.character(macros_saude %>%
                                   as.data.frame() %>%
                                   filter(CO_MACSAUD == macsaud.id()) %>%
                                   select(DS_UF_SIGLA))
            text <- paste(uf, "-", macro_name)
        }
        text
    })

    observe({
        event <- input$mapBrazil_shape_click
        macsaud.id(event$id)
    })
})
