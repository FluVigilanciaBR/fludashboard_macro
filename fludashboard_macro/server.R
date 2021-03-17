shhh <- suppressPackageStartupMessages
list.of.packages <- c('shiny',
                      'tidyverse',
                      'here',
                      'cowplot',
                      'extrafont',
                      'magick',
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

CASTING_H = 400
TREND_H = 150


source(here("fludashboard_macro/nowcasting_v2.R"))
source(here('fludashboard_macro/theme.publication.R'))
source(here('fludashboard_macro/episem.R'))

# Shape info
macros_file <- here("fludashboard_macro/geobrazilmacrosaude.RDS")
states <- geobr::read_state(year=2019)

#Data files
datafiles <- Sys.getenv(c("MACROSDATA", "CAPITALSDATA", "UFDATA"),
                        names = T)
macros_data <- readRDS(datafiles[[1]])
capitais_data <- readRDS(datafiles[[2]])
ufs_data <- readRDS(datafiles[[3]])
macros_saude <- readRDS(here(macros_file))
today.week <- 5
coerce2double <- function(df){
    as.numeric(unlist(df))
}

info.leaflet.map <- function(filtered_data){
    pal <- colorFactor("BrBG",
                       domain = rev(unique(filtered_data$latest.tendencia.6s)),
                       reverse = T,
                       na.color = "transparent")
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
            layerId=filtered_data$layerId,
            highlight = highlightOptions(
                weight = 5,
                color="#000",
                fillOpacity = 1,
                bringToFront = T),
            label = filtered_data$labels
        ) %>%
        addLegend(pal = pal,
                  values = filtered_data$latest.tendencia.6s,
                  opacity = 1,
                  title = NULL,
                  position = "bottomright"
        ) %>%
        setView(-50, -11, 4)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    macsaud.id <- shiny::reactiveVal(NA)
    capital.name <- shiny::reactiveVal(NA)
    uf.code <- shiny::reactiveVal(NA)
    ##
    # Views

    # Map for health macro regions
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
        latest.tendencia.6s <- factor(latest.tendencia.6s,
                                      labels=LEVELS.TENDENCIA,
                                      levels=c(-1, -.5, 0, .5, 1))
        filtered_data$latest.tendencia.6s <- latest.tendencia.6s
        geom_order <- match(filtered_data$CO_MACSAUD, macros_saude$CO_MACSAUD)
        filtered_data$geom <- macros_saude[geom_order, "geom"]

        filtered_data$labels <- paste(filtered_data$DS_UF_SIGLA, "-",
                                      filtered_data$DS_NOMEPAD_macsaud,
                                      ":", filtered_data$latest.tendencia.6s)
        filtered_data$layerId <- filtered_data$CO_MACSAUD

        # MAP
        info.leaflet.map(filtered_data)
    })

    # Map highlighting Brazilian states for selecting capital data
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
        latest.tendencia.6s <- factor(latest.tendencia.6s,
                                      labels=LEVELS.TENDENCIA,
                                      levels=c(-1, -.5, 0, .5, 1))
        filtered_data$latest.tendencia.6s <- latest.tendencia.6s
        splited_name <- as.data.frame(str_split_fixed(filtered_data$CO_MUN_RES_nome, " - ", 2))
        filtered_data$cidade <- trimws(as.character(splited_name$V1))
        filtered_data$adm <- trimws(as.character(splited_name$V2))
        idx_states <- match(filtered_data$DS_UF_SIGLA, states$abbrev_state)
        filtered_data$geom <- states[idx_states, "geom"]
        filtered_data <- filtered_data %>% filter(adm == adm_choice)

        filtered_data$labels <- paste(filtered_data$cidade, ":",
                                      filtered_data$latest.tendencia.6s)
        filtered_data$layerId <- filtered_data$cidade

        # Map
        info.leaflet.map(filtered_data)
    })

    # Map highlighting Brazilian states for selecting UF data
    output$mapBrazilUFs <- renderLeaflet({
        # Consolidate data.frame
        filtered_data <- ufs_data %>%
            filter(epiyear == 2021) %>%
            filter(epiweek == max(epiweek)) %>%
            select(c(DS_UF_SIGLA, CO_UF, tendencia.6s))
        latest.tendencia.6s <- filtered_data %>%
            select(tendencia.6s) %>%
            coerce2double() %>%
            as.factor()
        latest.tendencia.6s <- factor(latest.tendencia.6s,
                                      labels=LEVELS.TENDENCIA,
                                      levels=c(-1, -.5, 0, .5, 1))
        filtered_data$latest.tendencia.6s <- latest.tendencia.6s
        idx_states <- match(filtered_data$DS_UF_SIGLA, states$abbrev_state)
        filtered_data$geom <- states[idx_states, "geom"]
        filtered_data$labels <- paste(filtered_data$DS_UF_SIGLA, ":",
                                       filtered_data$latest.tendencia.6s)
        filtered_data$layerId <- filtered_data$CO_UF

        # Map
        info.leaflet.map(filtered_data)
    })

    # Forecast plot for macro regions
    output$castingPlot <- renderPlot({
        if(is.na(macsaud.id()) || is.null(macsaud.id()))
            p.now.srag <- NA
        else {
            xlimits <- c(1, macros_data %>%
                             select(Date) %>% max())

            pred.srag.summy <- macros_data %>% filter(CO_MACSAUD == macsaud.id())
            label <- unique(paste(pred.srag.summy$DS_UF_SIGLA, "-",
                            pred.srag.summy$DS_NOMEPAD_macsaud))[[1]]
            p.now.srag <- plot.prediction(pred.srag.summy,
                                          today.week,
                                          xlimits,
                                          label)
        }
        p.now.srag
    }, height = CASTING_H)

    # Forcast plot for capitals
    output$castingCapitaisPlot <- renderPlot({
        adm <- input$adm
        capital.name.ctrl <- capital.name()
        if(is.na(capital.name.ctrl) || is.null(capital.name.ctrl))
            p.now.srag <- NA
        else {
            xlimits <- c(1, capitais_data %>%
                             select(Date) %>% max())
            mun_res_nome <- capital.name.ctrl
            print(adm)
            if (adm != "")
                mun_res_nome <- paste(mun_res_nome, "-", adm)
            print(mun_res_nome)
            pred.srag.summy <- capitais_data %>%
                filter( CO_MUN_RES_nome == mun_res_nome )
            p.now.srag <- plot.prediction(pred.srag.summy,
                                          today.week,
                                          xlimits,
                                          mun_res_nome)
        }
        p.now.srag
    }, height = CASTING_H)

    # Forecasting plots for UFs
    output$castingUFsPlot <- renderPlot({
        uf.code.current <- uf.code()
        if(is.na(uf.code.current) || is.null(uf.code.current))
            p.now.srag <- NA
        else {
            xlimits <- c(1, ufs_data %>%
                             select(Date) %>% max())
            pred.srag.summy <- ufs_data %>%
                filter( CO_UF == uf.code.current )
            label <- unique(pred.srag.summy$DS_UF_SIGLA)[[1]]
            p.now.srag <- plot.prediction(pred.srag.summy, today.week, xlimits,
                                          label)
        }
        p.now.srag
    }, height = CASTING_H)

    # Trending plots for macro regions
    output$trendPlot <- renderPlot({
        if(is.na(macsaud.id()) || is.null(macsaud.id()))
            p.nivel <- NA
        else {

            xlimits <- c(1, macros_data %>%
                             select(Date) %>% max())
            pred.srag.summy <- macros_data %>% filter(CO_MACSAUD == macsaud.id())

            p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                          today.week = today.week,
                                          xlimits = xlimits)
        }
        p.nivel
    }, height = TREND_H)

    # Trending plots for capitals
    output$trendCapitaisPlot <- renderPlot({
        adm <- input$adm
        capital.name.ctrl <- capital.name()
        if(is.na(capital.name.ctrl) || is.null(capital.name.ctrl))
            p.nivel <- NA
        else {
            xlimits <- c(1, capitais_data %>%
                             select(Date) %>% max())
            mun_res_nome <- capital.name.ctrl
            if (length(adm))
                mun_res_nome <- paste(mun_res_nome, "-", adm)
            pred.srag.summy <- capitais_data %>%
                filter( CO_MUN_RES_nome == capital.name.ctrl )
            p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                          today.week = today.week,
                                          xlimits = xlimits)
        }
        p.nivel
    }, height = TREND_H)

    # Trending plots for UFS
    output$trendUFsPlot <- renderPlot({
        uf.code.current <- uf.code()
        if(is.na(uf.code.current) || is.null(uf.code.current))
            p.nivel <- NA
        else {
            xlimits <- c(1, ufs_data %>%
                             select(Date) %>% max())
            pred.srag.summy <- ufs_data %>%
                filter( CO_UF == uf.code.current )
            p.nivel <-  plot.ts.tendencia(df = pred.srag.summy,
                                          today.week = today.week,
                                          xlimits = xlimits)
        }
        p.nivel
    }, height = TREND_H)

    # Controller
    observe({
        ##
        # UI Event Handler
        eventMac <- input$mapBrazilMacro_shape_click
        macsaud.id(eventMac$id)

        eventCap <- input$mapBrazilCapitais_shape_click
        capital.name(eventCap$id)

        eventUF <- input$mapBrazilUFs_shape_click
        uf.code(eventUF$id)
    })
})
