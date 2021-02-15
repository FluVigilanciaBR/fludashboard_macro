shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'ggthemes',
                      'shinycssloaders',
                      "geobr",
                      "stringr",
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
macros_file <- here("fludashboard_macro/geobrazilmacrosaude.RDS")
states <- geobr::read_state(year=2019)

#Data files
datafiles <- Sys.getenv(c("MACROSDATA", "CAPITALSDATA"),
                        names = T)
macros_data <- readRDS(datafiles[[1]])
capitais_data <- readRDS(datafiles[[2]])
macros_saude <- readRDS(here(macros_file))
today.week <- 5
coerce2double <- function(df){
    as.numeric(unlist(df))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    macsaud.id <- shiny::reactiveVal(0)
    output$mapBrazilMacro <- renderLeaflet({
        # Consolidate data.frame
        filtered_data <- macros_data %>%
            filter(epiyear == 2021) %>%
            filter(epiweek == max(epiweek)) %>%
            select(c(DS_UF_SIGLA, DS_NOMEPAD_macsaud, CO_MACSAUD, tendencia.6s))
        latest.tendencia.6s <- filtered_data %>%
            select(tendencia.6s) %>%
            coerce2double() %>%
            as.factor()
        levels(latest.tendencia.6s) <- LEVELS.TENDENCIA
        filtered_data$latest.tendencia.6s <- latest.tendencia.6s
        geom_order <- match(filtered_data$CO_MACSAUD, macros_saude$CO_MACSAUD)
        filtered_data$geom <- macros_saude[geom_order, "geom"]

        # MAP
        pal <- colorFactor("BrBG",
                           domain = rev(unique(filtered_data$latest.tendencia.6s)),
                           reverse = T,
                           na.color = "transparent")
        labels <- paste(filtered_data$DS_UF_SIGLA, "-",
                        filtered_data$DS_NOMEPAD_macsaud,
                        ":", filtered_data$latest.tendencia.6s)
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(
                data = filtered_data$geom,
                fillColor = pal(filtered_data$latest.tendencia.6s),
                weight = 2,
                color="#444",
                fillOpacity = 1,
                layerId=filtered_data$CO_MACSAUD,
                highlight = highlightOptions(
                    weight = 5,
                    color="#000",
                    fillOpacity = 1,
                    bringToFront = T),
                label = labels
            ) %>%
            addLegend(pal = pal,
                      values = filtered_data$latest.tendencia.6s,
                      opacity = 1,
                      title = NULL,
                      position = "bottomright"
            ) %>%
            setView(-50, -11, 4)
    })

    output$mapBrazilCapitais <- renderLeaflet({

        # Consolidate data.frame
        adm_choice <- input$adm
        filtered_data <- capitais_data %>%
            filter(epiyear == 2021) %>%
            filter(epiweek == max(epiweek)) %>%
            select(c(DS_UF_SIGLA, CO_MUN_RES, CO_MUN_RES_nome, tendencia.6s))
        latest.tendencia.6s <- filtered_data %>%
            select(tendencia.6s) %>%
            coerce2double() %>%
            as.factor()
        levels(latest.tendencia.6s) <- LEVELS.TENDENCIA
        filtered_data$latest.tendencia.6s <- latest.tendencia.6s
        splited_name <- as.data.frame(str_split_fixed(filtered_data$CO_MUN_RES_nome, " - ", 2))
        filtered_data$cidade <- trimws(as.character(splited_name$V1))
        filtered_data$adm <- trimws(as.character(splited_name$V2))
        idx_states <- match(filtered_data$DS_UF_SIGLA, states$abbrev_state)
        filtered_data$geom <- states[idx_states, "geom"]
        filtered_data <- filtered_data %>% filter(adm == adm_choice)
        # MAP
        pal <- colorFactor("BrBG",
                           domain = rev(unique(filtered_data$latest.tendencia.6s)),
                           reverse = T,
                           na.color = "transparent")
        labels <- paste(filtered_data$cidade, ":", filtered_data$latest.tendencia.6s)
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(
                data = filtered_data$geom,
                fillColor = pal(filtered_data$latest.tendencia.6s),
                weight = 3,
                color="#444",
                fillOpacity = 1,
                layerId = filtered_data$CO_MUN_RES,
                label = labels,
                highlight = highlightOptions(
                    weight = 5,
                    color="#000",
                    fillOpacity = 1,
                    bringToFront = T)
            )  %>%
            addLegend(pal = pal,
                      values = filtered_data$latest.tendencia.6s,
                      opacity = 1,
                      title = NULL,
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
        event <- input$mapBrazilMacro_shape_click
        macsaud.id(event$id)
    })
})
