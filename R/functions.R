makePlot <- function(d, y, group, subtitle, ylab, ymin, ymax, legendTitle, foldername, filename, type) {
  # prepare SSES
  d$SSES <- ifelse(d$SSES >= 1 & d$SSES < 4, "Low SES",
                   ifelse(d$SSES >= 4 & d$SSES < 7, "Medium SES",
                          ifelse(d$SSES >= 7 & d$SSES <= 9, "High SES", NA)))
  d$SSES <- factor(d$SSES, levels = c("High SES", "Medium SES", "Low SES"))
  # prepare IPregion
  d$IPregion <- ifelse(d$IPregion == "UK", "United Kingdom",
                       ifelse(d$IPregion == "Europe", "Europe (excl. UK)", d$IPregion))
  d$IPregion <- factor(d$IPregion, levels = unique(d$IPregion)[c(3,2,1,4)])
  # prepare children
  d$ChildrenDichot <- factor(ifelse(d$ChildrenDichot==0, "None", "At least one"))
  # prepare sex
  d$Sex <- factor(ifelse(d$Sex==1, "Male", "Female"))
  # make plots
  if (type == "line") {
    out <-
      d %>%
      drop_na((!!sym(y))) %>%
      group_by(Time, (!!sym(group))) %>%
      summarise(mean = mean((!!sym(y))),
                se = sd((!!sym(y))) / sqrt(n())) %>%
      ggplot(aes(x = Time, y = mean, colour = (!!sym(group)))) +
      geom_line(position = position_dodge(0.1)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, 
                    position = position_dodge(0.1)) +
      geom_point(position = position_dodge(0.1)) +
      scale_y_continuous(breaks = ymin:ymax, limits = c(ymin, ymax)) +
      guides(colour=guide_legend(title=legendTitle))
  }
  if (type == "bar") {
    out <-
      d %>%
      drop_na((!!sym(y))) %>%
      group_by(Time, (!!sym(group))) %>%
      summarise(mean = mean((!!sym(y))),
                se = sd((!!sym(y))) / n()) %>%
      ggplot(aes(x = Time, y = mean - 1, group = (!!sym(group)), fill = (!!sym(group)))) +
      geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.5) +
      geom_errorbar(aes(ymin = mean - se - 1, ymax = mean + se - 1), width = 0, 
                    position = position_dodge(0.6)) +
      scale_y_continuous(breaks = (ymin-1):(ymax-1), limits = c(ymin-1, ymax-1), 
                         labels = ymin:ymax) +
      guides(fill=guide_legend(title=legendTitle))
  }
  out <-
    out +
    labs(subtitle = subtitle, y = ylab) +
    scale_x_continuous(breaks = 1:4, limits = c(0.7, 4.3), labels = c("March\n6-7", "March\n14-17", 
                                                                      "March\n24-27", "April\n3-6")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10),
          plot.subtitle = element_text(size = 10, face = "italic"))
  # save to file
  if (type == "line") {
    ggsave(
      out, 
      file = paste0("figures/linePlots/", foldername, "/individual/line_", 
                    foldername, "_", filename), 
      width = 6, height = 4)
  }
  if (type == "bar") {
    ggsave(
      out,
      file = paste0("figures/barPlots/", foldername, "/individual/bar_", 
                    foldername, "_", filename), 
      width = 6,
      height = 4)
  }
  return(out)
}

makePlotList <- function(d, plotPars, type) {
  out <- list()
  for (i in 1:nrow(plotPars)) {
    out[[i]] <- makePlot(d,
                         y  = plotPars$y[i],
                         group = plotPars$group[i],
                         # title = plotPars$title[i],
                         subtitle = plotPars$subtitle[i],
                         ylab  = plotPars$ylab[i],
                         ymin  = plotPars$ymin[i],
                         ymax  = plotPars$ymax[i],
                         legendTitle = plotPars$legendTitle[i],
                         foldername = plotPars$foldername[i],
                         filename = plotPars$filename[i],
                         type = type)
  }
  return(out)
}

makeGrid <- function(plotList, indexes, file, height = 7, width = 9) {
  l <- get_legend(plotList[[indexes[1]]])
  out <- plot_grid(plotList[[indexes[1]]] + theme(legend.position = "none"),
                   plotList[[indexes[2]]] + theme(legend.position = "none"),
                   plotList[[indexes[3]]] + theme(legend.position = "none"),
                   plotList[[indexes[4]]] + theme(legend.position = "none"),
                   nrow = 2, labels = letters[1:4])
  out <- plot_grid(out, l, rel_widths = c(1, 0.2))
  ggsave(file, plot = out, height = height, width = width)
  return(out)
}

makeGridList <- function(plotList, type) {
  # filenames
  groups <- c("region", "sex", "children", "ses")
  gridTypes <- c("risk", "pfi", "help")
  files <- paste0("figures/", type, "Plots/", rep(groups, each=3), "/grids/", 
                  type, "_", rep(groups, each=3), "_", rep(gridTypes, times=4), "Grid.pdf")
  # indexes
  i <- list(c(10:13), c( 1:4 ), c( 5:8 ), # region
            c(23:26), c(14:17), c(18:21), # sex
            c(36:39), c(27:30), c(31:34), # children
            c(49:52), c(40:43), c(44:47)) # ses
  # list
  out <- list()
  for (j in 1:length(files)) {
    out[[j]] <- makeGrid(plotList, i[[j]], files[j])
  }
  return(out)
}

makeCorPlot <- function(data, timepoint) {
  # data
  data <-
    data %>%
    filter(Time == timepoint) %>%
    select(c(17:23,27:30,37:38))
  # correlation matrix
  corr <- cor(data, use = "pairwise.complete.obs") %>% round(1)
  # p-values
  p.mat <- cor_pmat(data)
  # plot
  plot <-
    ggcorrplot(
      corr, type = "lower",
      outline.col = "white",
      p.mat = p.mat,
      sig.level = (0.05 / sum(lower.tri(corr))),
      pch.col = "grey"
      )
  # save
  ggsave(plot + ggtitle(paste0("Cohort 1, Timepoint ", timepoint)), 
         file = paste0("figures/correlations/corC1T", timepoint, ".pdf"),
         width = 7,
         height = 7)
  return(plot)
}

countTableRegion <- function(data) {
  data$IPregion <- ifelse(data$IPregion == "UK", "United Kingdom",
                          ifelse(data$IPregion == "Europe", "Europe (excl. UK)", data$IPregion))
  data$IPregion <- factor(data$IPregion, levels = unique(data$IPregion)[c(3,2,1,4)])
  out <- 
    data %>%
    filter(Time == 1) %>%
    group_by(IPregion) %>%
    summarise(count = n())
  return(out)
}

worldMapCounts <- function(data) {
  # use rgeolocate package
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
  # create world
  world <- 
    ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent %in% unique(continent)[1:6]) %>%
    mutate(region = ifelse(iso_a2 %in% c("US", "CA"), "North America",
                           ifelse(iso_a2 == "GB", "United Kingdom",
                                  ifelse(continent == "Europe", 
                                         "Europe (excl. UK)", "Other"))),
           region = factor(region, levels = c("North America", "United Kingdom",
                                                     "Europe (excl. UK)", "Other"))) %>%
    drop_na(region)
  world <- 
    ggplot() +
    geom_sf(data = world, aes(fill = region, colour = region), colour = NA, alpha = 0.2) + 
    coord_sf(ylim = c(-50, 90), xlim = c(), datum = NA) +
    theme(panel.background = element_rect(fill = 'white'))
  data <-
    data %>%
    mutate(country_code = maxmind(IPAddress, file, "country_code")$country_code,
           continent_name = maxmind(IPAddress, file, "continent_name")$continent_name,
           region = factor(ifelse(country_code %in% c("CA", "US"), "North America",
                                  ifelse(country_code == "GB", "United Kingdom",
                                         ifelse(continent_name == "Europe", "Europe (excl. UK)",
                                                "Other")))),
           region = factor(region, levels = levels(region)[c(2,4,1,3)])) %>%
    filter(Time == 1) %>%
    group_by(region, country_code) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    # get longitudes and latitudes for country centroids
    left_join(CoordinateCleaner::countryref %>%
                filter(type == "country") %>%
                select(iso2, centroid.lon, centroid.lat) %>%
                distinct(iso2, .keep_all = TRUE), 
              by = c("country_code" = "iso2")) %>%
    rename(region = 1) %>%
    drop_na(region)
  map <- 
    world +
    geom_point(aes(x = centroid.lon, y = centroid.lat, size = Count, 
                   colour = region),
               alpha = 0.85, data = data) +
    guides(colour = FALSE, fill = guide_legend(title = "Region",
                                               override.aes = list(alpha = 0.85))) +
    scale_size_area(breaks = seq(25,125,25)) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  # save
  ggsave(map, 
         file = "figures/worldMapCounts/worldMapCounts.pdf",
         width = 9, 
         height = 4.5)
  return(map)
}