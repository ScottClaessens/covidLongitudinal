# functions
loadData <- function(file) {
  out <- read_sav(file)
  # get country_codes based on self-report
  # if no self-report, take IP address country
  out$country_code <-
    ifelse(out$CurrentCountrySelfReport == "Wales",             "GB",
    ifelse(out$CurrentCountrySelfReport == "US",                "US",
    ifelse(out$CurrentCountrySelfReport == "UK",                "GB",
    ifelse(out$CurrentCountrySelfReport == "Turkey",            "TR",
    ifelse(out$CurrentCountrySelfReport == "Switzerland",       "CH",
    ifelse(out$CurrentCountrySelfReport == "Sweden",            "SE",
    ifelse(out$CurrentCountrySelfReport == "Spain",             "ES",
    ifelse(out$CurrentCountrySelfReport == "Slovenia",          "SI",
    ifelse(out$CurrentCountrySelfReport == "Scotland",          "GB",
    ifelse(out$CurrentCountrySelfReport == "Republic",          "IE",
    ifelse(out$CurrentCountrySelfReport == "Portugal",          "PT",
    ifelse(out$CurrentCountrySelfReport == "Poland",            "PL",
    ifelse(out$CurrentCountrySelfReport == "pl",                "PL",
    ifelse(out$CurrentCountrySelfReport == "NS",                "CA",
    ifelse(out$CurrentCountrySelfReport == "Norway",            "NO",
    ifelse(out$CurrentCountrySelfReport == "Northern",          "GB",
    ifelse(out$CurrentCountrySelfReport == "New Zealand",       "NZ",
    ifelse(out$CurrentCountrySelfReport == "Netherlands",       "NL",
    ifelse(out$CurrentCountrySelfReport == "Mexico",            "MX",
    ifelse(out$CurrentCountrySelfReport == "Luxembourg",        "LU",
    ifelse(out$CurrentCountrySelfReport == "Latvia",            "LV",
    ifelse(out$CurrentCountrySelfReport == "Italy",             "IT",
    ifelse(out$CurrentCountrySelfReport == "Israel",            "IL",
    ifelse(out$CurrentCountrySelfReport == "Ireland",           "IE",
    ifelse(out$CurrentCountrySelfReport == "Hungary",           "HU",
    ifelse(out$CurrentCountrySelfReport == "Greece",            "GR",
    ifelse(out$CurrentCountrySelfReport == "Germany",           "DE",
    ifelse(out$CurrentCountrySelfReport == "France",            "FR",
    ifelse(out$CurrentCountrySelfReport == "Finland",           "FI",
    ifelse(out$CurrentCountrySelfReport == "Estonia",           "EE",
    ifelse(out$CurrentCountrySelfReport == "England",           "GB",
    ifelse(out$CurrentCountrySelfReport == "Ecuador",           "EC",
    ifelse(out$CurrentCountrySelfReport == "Denmark",           "DK",
    ifelse(out$CurrentCountrySelfReport == "Czech Republic",    "CZ",
    ifelse(out$CurrentCountrySelfReport == "Chile",             "CL",
    ifelse(out$CurrentCountrySelfReport == "carms",             "US",
    ifelse(out$CurrentCountrySelfReport == "Canada",            "CA",
    ifelse(out$CurrentCountrySelfReport == "Belgium",           "BE",
    ifelse(out$CurrentCountrySelfReport == "Austria",           "AT",
    ifelse(out$CurrentCountrySelfReport == "Australia",         "AU", 
           NA))))))))))))))))))))))))))))))))))))))))
  # modify a few country_codes
  if (grepl("Long", file)) {
    out$country_code[1:4] <- "PL"
    out$country_code[1985:1988] <- "GB"
  } else {
    out$country_code[265] <- "PL"
    out$country_code[458] <- "AU"
    out$country_code[497] <- "GB"
  }
  # get regions
  out$region <-
    ifelse(out$country_code %in% c("US", "CA"), "United States / Canada",
           ifelse(out$country_code %in% c("GB", "IE"), "United Kingdom / Ireland",
                  ifelse(out$country_code %in% c("PT", "PL", "ES", "GR", "FR", "IT",
                                                 "BE", "CZ", "DE", "NL", "NO", "EE",
                                                 "SI", "FI", "LV", "LU", "SE", "CH",
                                                 "AT", "DK", "HU"), "Europe (excl. UK / Ireland)",
                         "Other")))
  out$region <- factor(out$region, levels = c("United States / Canada",
                                              "United Kingdom / Ireland",
                                              "Europe (excl. UK / Ireland)",
                                              "Other"))
  return(out)
}


makePlot <- function(d, y, group, subtitle, ylab, ymin, ymax, legendTitle, foldername, filename, type) {
  # prepare SSES
  d$SSES <- ifelse(d$SSES >= 1 & d$SSES < 4, "Low SES",
                   ifelse(d$SSES >= 4 & d$SSES < 7, "Medium SES",
                          ifelse(d$SSES >= 7 & d$SSES <= 9, "High SES", NA)))
  d$SSES <- factor(d$SSES, levels = c("High SES", "Medium SES", "Low SES"))
  # prepare children
  d$ChildrenDichot <- factor(ifelse(d$ChildrenDichot==0, "None", "At least one"))
  # prepare sex
  d$Sex <- factor(ifelse(d$Sex==1, "Male", "Female"))
  # prepare age
  d$Age <- factor(ifelse(d$Age < 30, "18-29", "30+"))
  # prepare medical
  d$Preexisting <- factor(ifelse(d$Preexisting == 1, "Yes", "No"), levels = c("Yes", "No"))
  # if humanity vs. neighborhood plots
  if (group == "Grouping") {
    d <-
      d %>%
      select(Time, NBTHumanity, PFIHumanityEmpathy, PFIHumanitySharedFate, WillingnessHumanity,
             NBTNeigh, PFINeighEmpathy, PFINeighSharedFate, WillingnessNeigh) %>%
      rename(
        NBT_Humanity               = NBTHumanity,
        NBT_Neighborhood           = NBTNeigh,
        PFIEmpathy_Humanity        = PFIHumanityEmpathy,
        PFIEmpathy_Neighborhood    = PFINeighEmpathy,
        PFISharedFate_Humanity     = PFIHumanitySharedFate,
        PFISharedFate_Neighborhood = PFINeighSharedFate,
        Willingness_Humanity       = WillingnessHumanity,
        Willingness_Neighborhood   = WillingnessNeigh
        ) %>%
      pivot_longer(cols = -Time,
                   names_to = c(".value", "Grouping"), 
                   names_sep = "_")
    if (grepl("PFI", y)) {
      d$Grouping <- ifelse(d$Grouping == "Humanity", "All of humanity", "My neighborhood")
    } else {
      d$Grouping <- ifelse(d$Grouping == "Humanity", 
                           "A person who is\nnot a citizen of\nyour own country\n", 
                           "Someone from\nyour neighborhood\n")
      d$Grouping <- factor(d$Grouping, levels = unique(d$Grouping)[c(2,1)])
    }
  }
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
      geom_point(position = position_dodge(0.1), size = 0.8) +
      scale_y_continuous(breaks = ymin:ymax, limits = c(ymin, ymax)) +
      guides(colour=guide_legend(title=legendTitle))
  }
  if (type == "bar") {
    out <-
      d %>%
      drop_na((!!sym(y))) %>%
      group_by(Time, (!!sym(group))) %>%
      summarise(mean = mean((!!sym(y))),
                se = sd((!!sym(y))) / sqrt(n())) %>%
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
    ggsave(out, file = paste0("figures/linePlots/", foldername, "/individual/line_", 
                              foldername, "_", filename, ".pdf"), width = 6, height = 4)
    ggsave(out, file = paste0("figures/linePlots/", foldername, "/individual/line_", 
                              foldername, "_", filename, ".png"), width = 6, height = 4)
  }
  if (type == "bar") {
    ggsave(out, file = paste0("figures/barPlots/", foldername, "/individual/bar_", 
                              foldername, "_", filename, ".pdf"), width = 6, height = 4)
    ggsave(out, file = paste0("figures/barPlots/", foldername, "/individual/bar_", 
                              foldername, "_", filename, ".png"), width = 6, height = 4)
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

makeGrid <- function(plotList, indexes, file, height = 7, width = 9.5) {
  l <- get_legend(plotList[[indexes[1]]])
  if (grepl("grouping", file) | grepl("medical", file) | grepl("age", file)) {
    out <- plot_grid(plotList[[indexes[1]]] + theme(legend.position = "none"),
                     plotList[[indexes[2]]] + theme(legend.position = "none"),
                     nrow = 1, labels = letters[1:2])
  } else {
    out <- plot_grid(plotList[[indexes[1]]] + theme(legend.position = "none"),
                     plotList[[indexes[2]]] + theme(legend.position = "none"),
                     plotList[[indexes[3]]] + theme(legend.position = "none"),
                     plotList[[indexes[4]]] + theme(legend.position = "none"),
                     nrow = 2, labels = letters[1:4])
  }
  out <- plot_grid(out, l, rel_widths = c(1, 0.25))
  ggsave(paste0(file, ".pdf"), 
         plot = out, 
         height = ifelse(grepl("grouping", file) | grepl("medical", file) | grepl("age", file), 
                         height / 2, height), 
         width = width)
  ggsave(paste0(file, ".png"), 
         plot = out, 
         height = ifelse(grepl("grouping", file) | grepl("medical", file) | grepl("age", file), 
                         height / 2, height), 
         width = width)
  return(out)
}

makeGridList <- function(plotList, type) {
  # filenames
  groups <- c("region", "sex", "children", "ses")
  gridTypes <- c("risk", "pfi", "help")
  files <- paste0("figures/", type, "Plots/", rep(groups, each=3), "/grids/", 
                  type, "_", rep(groups, each=3), "_", rep(gridTypes, times=4), "Grid")
  files <- c(files,
             # grouping
             paste0("figures/", type, "Plots/grouping/grids/", type, "_grouping_pfiGrid"),
             paste0("figures/", type, "Plots/grouping/grids/", type, "_grouping_helpGrid"),
             # medical
             paste0("figures/", type, "Plots/medical/grids/", type, "_medical_pfiGrid"),
             # age
             paste0("figures/", type, "Plots/age/grids/", type, "_age_pfiGrid")
             )
  # indexes
  i <- list(c(10:13), c( 1:4 ), c( 5:8 ), # region
            c(23:26), c(14:17), c(18:21), # sex
            c(36:39), c(27:30), c(31:34), # children
            c(49:52), c(40:43), c(44:47), # ses
            c(53:54), c(55:56),           # grouping
            c(59:60),                     # medical
            c(61,58)                      # age
  )
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
  ggsave(plot + ggtitle(paste0("Timepoint ", timepoint)), 
         file = paste0("figures/correlations/corC1T", timepoint, ".pdf"),
         width = 7, height = 7)
  ggsave(plot + ggtitle(paste0("Timepoint ", timepoint)), 
         file = paste0("figures/correlations/corC1T", timepoint, ".png"),
         width = 7, height = 7)
  return(plot)
}

countTableRegion <- function(data) {
  out <- 
    data %>%
    filter(Time == 1) %>%
    group_by(region, country_code) %>%
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
    mutate(region = ifelse(iso_a2 %in% c("US", "CA"), "United States / Canada",
                    ifelse(iso_a2 %in% c("GB", "IE"), "United Kingdom / Ireland",
                    ifelse(iso_a2 %in% c("PT", "PL", "ES", "GR", "FR", "IT",
                                         "BE", "CZ", "DE", "NL", "NO", "EE",
                                         "SI", "FI", "LV", "LU", "SE", "CH",
                                         "AT", "DK", "HU"), "Europe (excl. UK / Ireland)", 
                    ifelse(iso_a2 %in% c("TR", "NZ", "MX", "IL", "EC", "CL", "AU"), "Other",
                                         "Not measured")))),
           region = factor(region, levels = c("United States / Canada", "United Kingdom / Ireland",
                                                     "Europe (excl. UK / Ireland)", "Other", "Not measured"))) %>%
    drop_na(region)
  world <- 
    ggplot() +
    geom_sf(data = world, aes(fill = region, colour = region), colour = NA, alpha = 0.2) + 
    coord_sf(ylim = c(-50, 90), xlim = c(), datum = NA) +
    theme(panel.background = element_rect(fill = 'white')) +
    scale_fill_manual(breaks = c("United States / Canada",
                                 "United Kingdom / Ireland",
                                 "Europe (excl. UK / Ireland)",
                                 "Other"),
                      values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "grey80")) 
  data <-
    data %>%
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
    scale_size_area(breaks = c(5, seq(25,125,25))) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  # save
  ggsave(map, file = "figures/worldMapCounts/worldMapCounts.pdf",
         width = 9, height = 4.5)
  ggsave(map, file = "figures/worldMapCounts/worldMapCounts.png",
         width = 9, height = 4.5)
  return(map)
}