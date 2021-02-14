shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'ggthemes',
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
source(here('fludashboard_macro/theme.publication.R'))
source(here('fludashboard_macro/episem.R'))

# Shape info
macros_file <- "fludashboard_macro/geobrazilmacrosaude.RDS"

#Data files
datafiles <- Sys.getenv(c("MACROSDATA", "CAPITALSDATA"),
                        names = T)
macros_data <- readRDS(datafiles[[1]])
macros_saude <- readRDS(here(macros_file))
today.week <- 5
coerce2double <- function(df){
    as.numeric(unlist(df))
}

# Epiweek


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    macsaud.id <- shiny::reactiveVal(0)
    output$mapBrazil <- renderLeaflet({

        latest.tendencia.6s <- macros_data %>%
            filter(epiyear == 2021) %>%
            filter(epiweek == max(epiweek)) %>%
            select(tendencia.6s) %>%
            coerce2double()

        pal <- colorNumeric("RdYlBu", domain = range(latest.tendencia.6s),
                        reverse = F,
                        na.color = "transparent")
        labels <- paste(macros_saude$DS_UF_SIGLA, "-",
                        macros_saude$DS_ABREV_macsaud)
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(
                data = macros_saude$geom,
                fillColor = pal(latest.tendencia.6s),
                weight = 1,
                color="#ccc",
                fillOpacity = 1,
                layerId=macros_saude$CO_MACSAUD,
                highlight = highlightOptions(
                    weight = 5,
                    color="#000",
                    fillOpacity = 1,
                    bringToFront = T),
                label = labels
            ) %>%
            addLegend(pal = pal,
                      values = latest.tendencia.6s,
                      opacity = 0.7,
                      title = NULL,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                      position = "bottomright"
            ) %>%
            setView(-50, -11, 4)
    })
    output$castingPlot <- renderPlot({

        if(length(macsaud.id()) == 0)
            p.now.srag <- NA
        else {
            epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
            xbreaks <- c(epilbls, epilbls + 53)
            xlbls <- c(epilbls, epilbls)
            xlimits <- c(1, macros_data %>%
                             select(Date) %>% max())
            macro_name <- as.character(macros_saude %>%
                                       as.data.frame() %>%
                                       filter(CO_MACSAUD == macsaud.id()) %>%
                                       select(DS_ABREV_macsaud))
            uf <- as.character(macros_saude %>%
                               as.data.frame() %>%
                               filter(CO_MACSAUD == macsaud.id()) %>%
                               select(DS_UF_SIGLA))
            pred.srag.summy <- macros_data %>% filter(CO_MACSAUD == macsaud.id())
            p.now.srag <- plot.nowcast(pred.srag.summy, Fim=today.week ) +
              ylab("Incidência de SRAG (por 100mil hab.)") +
              xlab("Semana de primeiros sintomas") +
              scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
              theme_Publication(base_size = 16, base_family = 'Roboto') +
              theme(plot.margin=unit(c(1,0,5,5), units='pt'),
                    axis.text = element_text(size = rel(1)),
                    legend.margin = margin(0,0,0,0, unit='pt'),
                    legend.justification=c(0,1),
                    legend.position=c(0.015, 1.05),
                    legend.background = element_blank(),
                    legend.key = element_blank(),
                    legend.key.size = unit(14, 'pt'),
                    legend.text = element_text(family = 'Roboto', size = rel(1)))


        }
        p.now.srag
    }, height = "auto")

    output$trendPlot <- renderPlot({

        if(length(macsaud.id()) == 0)
            p.nivel <- NA
        else {
            epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
            xbreaks <- c(epilbls, epilbls + 53)
            xlbls <- c(epilbls, epilbls)
            xlimits <- c(1, macros_data %>%
                             select(Date) %>% max())
            pred.srag.summy <- macros_data %>% filter(CO_MACSAUD == macsaud.id())

            p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                          today.week = today.week,
                                          xbreaks = xbreaks,
                                          xlbls = xlbls,
                                          xlimits = xlimits)

        }
        p.nivel
    }, height = 240)


    observe({
        event <- input$mapBrazil_shape_click
        macsaud.id(event$id)
    })
})
