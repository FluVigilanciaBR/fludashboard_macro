# Auxiliar functions for nowcasting
extrafont::loadfonts()


# Auxiliar function, sampling from a negative binomial likelihood
ff <- function(x, idx){
  rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1])
}

# Auxiliar function selecionando um pedaco do dataset
gg <- function(x, dados, idx, Fim.sat, Dmax){
  data.aux <- dados
  data.aux$Casos[idx] <- x
  data.aggregated <- data.aux %>%
    # Selecionando apenas os dias faltantes a partir
    # do domingo da respectiva ultima epiweek
    # com dados faltantes
    filter(Date >= Fim.sat - Dmax  ) %>%
    group_by(Date) %>%
    dplyr::summarise(
      Casos = sum(Casos)
    )
  data.aggregated
}


# Algorithm to get samples for the predictive distribution for the number of cases

nowcasting <- function(output.day, dadosRio.ag,
                       Fim = today.week, Dm = Dmax){

  index.missing = which(is.na(dadosRio.ag$Casos))


  # Step 1: Sampling from the approximate posterior distribution using INLA
  srag.samples.list <- inla.posterior.sample(n = 1000, output.day)

  # Step 2: Sampling the missing triangle (in vector form) from the likelihood using INLA estimates
  vector.samples <- lapply(X = srag.samples.list,
                           FUN = ff,
                           idx = index.missing
  )

  # Step 3: Calculate N_t for each triangle sample {N_t : t=Tactual-Dmax+1,...Tactual}
  tibble.samples <- lapply( X = vector.samples,
                            FUN = gg,
                            dados = dadosRio.ag,
                            idx = index.missing,
                            Fim.sat = Fim,
                            Dmax = Dm
  )

  # Nowcasting
  srag.pred <- bind_rows(tibble.samples, .id = "sample")

  srag.pred
}


#######################
# hess.min <- -1
# h.value <- 0.01
# trials <- 0
# while (hess.min <= 0 & trials < 50){
#   output <- inla(model, family = "nbinomial", data = delay.inla.trian,
#                  control.predictor = list(link = 1, compute = T),
#                  control.compute = list( config = T, waic=TRUE, dic=TRUE),
#                  control.family = list(
#                    hyper = list("theta" = list(prior = "loggamma", param = c(0.1, 0.1)))
#                  ),
#                  control.inla = list(h = h.value)
#   )
#   hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
#   hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
#   h.value <- h.value + 0.01
#   trials <- trials + 1
# }
# print(paste('Hessian trials:',trials))
#
######################


# Running INLA for the nowcasting model
nowcast.INLA <- function(dados.ag, model.day,...){
  hess.min <- -1
  h.value <- 0.01
  trials <- 0
  while (hess.min <= 0 & trials < 50){
    output <- inla(formula = model.day,
                 family = "nbinomial",
                 data = dados.ag,
                 num.threads = 4,
                 control.predictor = list(link = 1, compute = T),
                 control.compute = list( config = T),
                 control.inla = list(h = h.value),
                 ...
                 # control.family = list(
                 # hyper = list("theta" = list(
                 #   prior = "loggamma", param = c(1, 0.1)))
                 #   )
    )
    hess.start <- which(output$logfile == 'Eigenvalues of the Hessian')
    hess.min <- min(as.numeric(output$logfile[(hess.start+1):(hess.start+3)]))
    h.value <- h.value + 0.01
    trials <- trials + 1
  }
  output
}


# Plot nowcasting
plot.nowcast <- function(pred.summy, Fim, nowcast = T){

  if(!nowcast){
    # Time series
    p0.day <- pred.summy %>%
      ggplot(aes(x =  Date, y = Casos,
                 color = "Casos notificados",
                 linetype = "Casos notificados")) +
      geom_line(size = 1, na.rm = T)
  } else {
    p0.day <- pred.summy %>%
      ggplot(aes(x =  Date, y = Casos.cut,
                 color = "Casos notificados",
                 linetype = "Casos notificados",
                 text=paste0('Semana: ', epiweek, ' ', epiyear,
                            '<br>Casos notificados: ', round(Casos.cut,2),
                            '<br>Casos estimados: ', round(Median,2), ' [',round(IC90I,2),'-',round(IC90S,2),']',
                            '<br>Média móvel: ', round(rolling_average,2)),
                 group=DS_UF_SIGLA)) +
      geom_line(size = 1, na.rm = T) +
      geom_ribbon( aes( ymin=IC90I, ymax=IC90S), fill = 'gray',
                   color = 'gray', alpha = 0.5,
                   show.legend = FALSE) +
      geom_line(aes(x = Date, y = Median,
                    colour = "Casos estimados",
                    linetype = "Casos estimados"),
                size = 1, na.rm = T) +
      geom_line(aes(x=Date, y=rolling_average,
                    colour='Média móvel',
                    linetype='Média móvel'), size=1) +
      scale_colour_manual(name = "",
                          values = c("black", "black", 'blue'),
                          guide = guide_legend(reverse=F)) +
      scale_linetype_manual(name = "",
                            values = c("dotted", "solid", 'solid'),
                            guide = guide_legend(reverse=F))
  }

  p0.day
}



add.logo <- function(plot, x=0.4, y=-0.3, scale=0.15){
    logo <-here('fludashboard_macro/www/static/img/info_gripe.png')
    ggdraw() +
      draw_plot(plot) +
      draw_image(logo, x=x, y=y, scale=scale)
}

plot.prediction <- function(pred.srag.summy, today.week, xlimits, label="Predição") {
    epilbls <- c(1, 8, 16, 24, 32, 40, 48)
    xbreaks <- pred.srag.summy$Date[pred.srag.summy$epiweek %in% epilbls]
    xlbls <- pred.srag.summy$epiweek[pred.srag.summy$Date %in% xbreaks]


    plt <-plot.nowcast(pred.srag.summy, Fim=today.week ) +
      ylab("Incidência de SRAG (por 100mil hab.)") +
      xlab("Semana de primeiros sintomas") +
      ggtitle(label) +
      scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits) +
      theme_Publication(base_size = 14, base_family = 'Roboto') +
      theme(plot.margin=unit(c(1,0,5,5), units='pt'),
            legend.margin = margin(0,0,0,0, unit='pt'),
            legend.justification=c(0,1),
            legend.position=c(0.015, 1.05),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.key.size = unit(14, 'pt'),
            legend.text = element_text(size = rel(1)))

    plt
}

LEVELS.TENDENCIA <- list('Prob. queda\n> 95%',
                      'Prob. queda\n> 75%',
                      'Estabilidade./\noscilação',
                      'Prob. cresc.\n> 75%',
                      'Prob. cresc.\n> 95%')

# Function for trend plot
plot.ts.tendencia <- function(df,
                              today.week=0,
                              xlimits=c(1, 53)){
  epilbls <- c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52)
  xbreaks <- df$Date[df$epiweek %in% epilbls]
  xlbls <- df$epiweek[df$Date %in% xbreaks]
  plt <- df %>%
    select(Date, tendencia.3s, tendencia.6s) %>%
    mutate(tendencia.3s = case_when(
      Date < today.week - 1 ~ NA_real_,
      TRUE ~ tendencia.3s
    )) %>%
    rename('curto prazo'=tendencia.3s, 'longo prazo'=tendencia.6s) %>%
    pivot_longer(-Date, names_to = 'janela', values_to = 'tendencia') %>%
    arrange(desc(janela)) %>%
    ggplot(aes(x=Date, y=tendencia, color=janela)) +
    geom_hline(yintercept = 0, color='grey', size=1.5, linetype=2) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(-1,1,.5),
                       labels= LEVELS.TENDENCIA,
                       limits = c(-1,1),
                       name=NULL) +
    scale_x_continuous(breaks = xbreaks, labels = xlbls, limits = xlimits, name=NULL) +
    scale_color_discrete(name=NULL) +
    theme_Publication() +
    theme(plot.margin=unit(c(1,0,5,5), units='pt'),
          axis.text.y = element_text(size = rel(.8), angle=30),
          legend.margin = margin(0,0,0,0, unit='pt'),
          legend.justification=c(0,1),
          legend.position=c(0.015, 1.05),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(14, 'pt'),
          legend.text = element_text(size = rel(.8))
    )
  plt
}
