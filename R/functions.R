# custom functions
addCountryFromIP <- function(d) {
  # use rgeolocate package
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
  # create data
  d <-
    d %>%
    mutate(country_name.1   = maxmind(IPAddress.1, file, "country_name")$country_name,
           country_code.1   = maxmind(IPAddress.1, file, "country_code")$country_code,
           continent_name.1 = maxmind(IPAddress.1, file, "continent_name")$continent_name,
           country_name.2   = maxmind(IPAddress.2, file, "country_name")$country_name,
           country_code.2   = maxmind(IPAddress.2, file, "country_code")$country_code,
           continent_name.2 = maxmind(IPAddress.2, file, "continent_name")$continent_name,
           country_name.3   = maxmind(IPAddress.3, file, "country_name")$country_name,
           country_code.3   = maxmind(IPAddress.3, file, "country_code")$country_code,
           continent_name.3 = maxmind(IPAddress.3, file, "continent_name")$continent_name,
           country_name.4   = maxmind(IPAddress.4, file, "country_name")$country_name,
           country_code.4   = maxmind(IPAddress.4, file, "country_code")$country_code,
           continent_name.4 = maxmind(IPAddress.4, file, "continent_name")$continent_name) %>%
    # categorise by (1) UK, (2) US, (3) Rest of Europe, (4) Other
    mutate(region.1 = ifelse(country_name.1 == "United Kingdom", "United Kingdom",
                             ifelse(country_name.1 == "United States", "United States",
                                    ifelse(country_name.1 != "United Kingdom" & continent_name.1 == "Europe", "Europe (excl. UK)",
                                           ifelse(is.na(country_name.1), NA, "Other")))),
           region.2 = ifelse(country_name.2 == "United Kingdom", "United Kingdom",
                             ifelse(country_name.2 == "United States", "United States",
                                    ifelse(country_name.2 != "United Kingdom" & continent_name.2 == "Europe", "Europe (excl. UK)",
                                           ifelse(is.na(country_name.2), NA, "Other")))),
           region.3 = ifelse(country_name.3 == "United Kingdom", "United Kingdom",
                             ifelse(country_name.3 == "United States", "United States",
                                    ifelse(country_name.3 != "United Kingdom" & continent_name.3 == "Europe", "Europe (excl. UK)",
                                           ifelse(is.na(country_name.3), NA, "Other")))),
           region.4 = ifelse(country_name.4 == "United Kingdom", "United Kingdom",
                             ifelse(country_name.4 == "United States", "United States",
                                    ifelse(country_name.4 != "United Kingdom" & continent_name.4 == "Europe", "Europe (excl. UK)",
                                           ifelse(is.na(country_name.4), NA, "Other")))))
  return(d)
}

loadData <- function(file) {
  out <-
    read_sav(file) %>%
    addCountryFromIP()
  return(out)
}

makePlot <- function(d, y1, y2, y3, y4, group1, group2, group3, group4, 
                     subtitle, ylab, ymin, ymax, legendTitle, foldername, filename, type) {
  # prepare Children.1
  d$Children.1 <- ifelse(d$Children.1 == 0, "0",
                         ifelse(d$Children.1 == 1, "1",
                                ifelse(d$Children.1 == 2, "2",
                                       ifelse(is.na(d$Children.1), NA, "3+"))))
  d$Children.1 <- factor(d$Children.1)
  # prepare SES_ladder.1
  d$SES_Ladder.1 <- ifelse(d$SES_Ladder.1 %in% c(1,2,3), "Low SES",
                           ifelse(d$SES_Ladder.1 %in% c(4,5,6), "Medium SES",
                                  ifelse(d$SES_Ladder.1 %in% c(7,8,9), "High SES", NA)))
  d$SES_Ladder.1 <- factor(d$SES_Ladder.1, levels = c("Low SES", "Medium SES", "High SES"))
  # summary function
  getSummary <- function(data, y, group) {
    out <- 
      data %>% 
      select(all_of(y), all_of(group)) %>%
      mutate_at(vars(2), as.factor) %>%
      drop_na(all_of(group)) %>%
      group_by_at(2) %>%
      summarise_at(all_of(y), list(mean = mean, se = std.error), na.rm=T) %>%
      rename(Group = all_of(group))
    return(out)
  }
  out <-
    bind_rows(
      getSummary(d, y1, group1), 
      getSummary(d, y2, group2), 
      getSummary(d, y3, group3),
      getSummary(d, y4, group4)
    ) %>%
    mutate(Timepoint = rep(1:4, each = n_distinct(Group)))
  # if group is "region"
  if (grepl("region", group1)) {
    out <- out %>% 
      mutate(Group = factor(Group, levels = c("United States", "United Kingdom",
                                              "Europe (excl. UK)", "Other")))
  }
  # if group is "Sex.1"
  if (grepl("Sex", group1)) {
    out <- out %>% 
      filter(Group != 3) %>%
      mutate(Group = factor(ifelse(Group == 1, "Male", "Female"), 
                            levels = c("Male", "Female")))
  }
  # make plots
  if (type == "line") {
    out <- 
      ggplot(out, aes(x = Timepoint, y = mean, colour = Group)) +
      geom_line(position = position_dodge(0.1)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, 
                    position = position_dodge(0.1)) +
      geom_point(position = position_dodge(0.1)) +
      scale_y_continuous(breaks = ymin:ymax, limits = c(ymin, ymax)) +
      guides(colour=guide_legend(title=legendTitle))
  }
  if (type == "bar") {
    out <-
      ggplot(out, aes(x = Timepoint, y = mean - 1, group = Group, fill = Group)) +
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
                         y1  = plotPars$y1[i],
                         y2  = plotPars$y2[i],
                         y3  = plotPars$y3[i],
                         y4  = plotPars$y4[i],
                         group1 = plotPars$group1[i],
                         group2 = plotPars$group2[i],
                         group3 = plotPars$group3[i],
                         group4 = plotPars$group4[i],
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
  if (timepoint == 1) data <- data %>% select(c(195:194, 206, 200:199, 208:207, 198:196, 205:201, 193:184))
  if (timepoint == 2) data <- data %>% select(c(356:355, 361:359, 363:362, 358:357, 354:349))
  if (timepoint == 3) data <- data %>% select(c(374:373, 409, 391, 387, 390, 386, 389, 385, 389, 388, 384))
  if (timepoint == 4) data <- data %>% select(c(420:419, 450, 437, 433, 436, 432, 435, 431, 434, 430))
  # sort colnames
  if (timepoint == 1) colnames(data) <- c("Anxiety", "Stress", "DiseaseAvoidance", "RiskManagement", "RiskPerception",
                                          "HumanityNBT", "NeighborNBT", "HumanityWillingToHelp",
                                          "CountryWillingToHelp", "NeighborWillingToHelp", "HumanityCFI",
                                          "CountryCFI", "NeighborCFI", "AcqCFI", "CloseFriendCFI",
                                          "HumanityPFISharedFate", "CountryPFISharedFate", "NeighborPFISharedFate",
                                          "AcqPFISharedFate", "CloseFriendPFISharedFate", "HumanityPFIEmpathy",
                                          "CountryPFIEmpathy", "NeighborPFIEmpathy", "AcqPFIEmpathy", "CloseFriendPFIEmpathy")
  if (timepoint == 2) colnames(data) <- c("Anxiety", "Stress", "DiseaseAvoidance", "RiskManagement", "RiskPerception",
                                          "HumanityNBT", "NeighborNBT", "HumanityWillingToHelp", "NeighborWillingToHelp",
                                          "HumanityPFISharedFate", "NeighborPFISharedFate", "CloseFriendPFISharedFate", 
                                          "HumanityPFIEmpathy", "NeighborPFIEmpathy", "CloseFriendPFIEmpathy")
  if (timepoint %in% c(3, 4)) colnames(data) <- c("Anxiety", "Stress", "RiskManagement",
                                                  "HumanityNBT", "NeighborNBT", "HumanityWillingToHelp", "NeighborWillingToHelp",
                                                  "HumanityPFISharedFate", "NeighborPFISharedFate", 
                                                  "HumanityPFIEmpathy", "NeighborPFIEmpathy")
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
  if (timepoint == 1) ggsave(plot + ggtitle("Cohort 1, Timepoint 1"), 
                             file = "figures/correlations/corC1T1.pdf",
                             width = 10, height = 10)
  if (timepoint == 2) ggsave(plot + ggtitle("Cohort 1, Timepoint 2"), 
                             file = "figures/correlations/corC1T2.pdf",
                             width = 8, height = 8)
  if (timepoint == 3) ggsave(plot + ggtitle("Cohort 1, Timepoint 3"), 
                             file = "figures/correlations/corC1T3.pdf",
                             width = 7, height = 7)
  if (timepoint == 4) ggsave(plot + ggtitle("Cohort 1, Timepoint 4"), 
                             file = "figures/correlations/corC1T4.pdf",
                             width = 7, height = 7)
  return(plot)
}

countTableRegion <- function(data, timepoint) {
  if (timepoint == 1) out <- data %>% group_by(region.1, country_code.1, country_name.1)
  if (timepoint == 2) out <- data %>% group_by(region.2, country_code.2, country_name.2)
  if (timepoint == 3) out <- data %>% group_by(region.3, country_code.3, country_name.3)
  if (timepoint == 4) out <- data %>% group_by(region.4, country_code.4, country_name.4)
  out <- out %>% summarise(count = n())
  return(out)
}

worldMapCounts <- function(data, timepoint) {
  world <- 
    ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent %in% unique(continent)[1:6]) %>%
    mutate(region = ifelse(iso_a2 == "US", "United States",
                           ifelse(iso_a2 == "GB", "United Kingdom",
                                  ifelse(continent == "Europe", 
                                         "Europe (excl. UK)", "Other"))),
           region = factor(region, levels = c("United States", "United Kingdom",
                                                     "Europe (excl. UK)", "Other"))) %>%
    drop_na(region)
  world <- 
    ggplot() +
    geom_sf(data = world, aes(fill = region, colour = region), colour = NA, alpha = 0.2) + 
    coord_sf(ylim = c(-50, 90), xlim = c(), datum = NA) +
    theme(panel.background = element_rect(fill = 'white'))
  if (timepoint == 1) data <- data %>% group_by(region.1, country_code.1, country_name.1)
  if (timepoint == 2) data <- data %>% group_by(region.2, country_code.2, country_name.2)
  if (timepoint == 3) data <- data %>% group_by(region.3, country_code.3, country_name.3)
  if (timepoint == 4) data <- data %>% group_by(region.4, country_code.4, country_name.4)
  data <- data %>% summarise(Count = n())
  if (timepoint == 1) by <- c("country_code.1" = "iso2")
  if (timepoint == 2) by <- c("country_code.2" = "iso2")
  if (timepoint == 3) by <- c("country_code.3" = "iso2")
  if (timepoint == 4) by <- c("country_code.4" = "iso2")
  data <-
    data %>%
    ungroup() %>%
    # get longitudes and latitudes for country centroids
    left_join(CoordinateCleaner::countryref %>%
                filter(type == "country") %>%
                select(iso2, centroid.lon, centroid.lat) %>%
                distinct(iso2, .keep_all = TRUE), by = by) %>%
    rename(region = 1) %>%
    drop_na(region) %>%
    mutate(region = factor(region, levels = c("United States", "United Kingdom",
                                              "Europe (excl. UK)", "Other")))
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
  if (timepoint == 1) ggsave(map, file = "figures/worldMapCounts/worldMapCountsC1T1.pdf",
                             width = 9, height = 4.5)
  if (timepoint == 2) ggsave(map, file = "figures/worldMapCounts/worldMapCountsC1T2.pdf",
                             width = 9, height = 4.5)
  if (timepoint == 3) ggsave(map, file = "figures/worldMapCounts/worldMapCountsC1T3.pdf",
                             width = 9, height = 4.5)
  if (timepoint == 4) ggsave(map, file = "figures/worldMapCounts/worldMapCountsC1T4.pdf",
                             width = 9, height = 4.5)
  return(map)
}