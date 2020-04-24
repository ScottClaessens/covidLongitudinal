# drake plan
plan <- drake_plan(
  # load data
  d = loadData("data/Prolific cohort 1 Timepoint 1 - 4 cleaned.sav"),
  # line plots
  linePlotList = makePlotList(
    d, read.csv("data/plotting/lineBarPlots.csv",
                stringsAsFactors = FALSE,
                allowEscapes = TRUE), "line"
    ),
  # bar plots
  barPlotList = makePlotList(
    d, read.csv("data/plotting/lineBarPlots.csv",
                stringsAsFactors = FALSE,
                allowEscapes = TRUE), "bar"
    ),
  # line plot grids
  linePlotGridList = makeGridList(linePlotList, "line"),
  # bar plot grids
  barPlotGridList = makeGridList(barPlotList, "bar"),
  # # cor plots
  corC1T1 = makeCorPlot(d, timepoint = 1),
  corC1T2 = makeCorPlot(d, timepoint = 2),
  corC1T3 = makeCorPlot(d, timepoint = 3),
  corC1T4 = makeCorPlot(d, timepoint = 4),
  # # table
  tableC1T1 = countTableRegion(d, timepoint = 1),
  tableC1T2 = countTableRegion(d, timepoint = 2),
  tableC1T3 = countTableRegion(d, timepoint = 3),
  tableC1T4 = countTableRegion(d, timepoint = 4),
  # # world
  worldC1T1 = worldMapCounts(d, timepoint = 1),
  worldC1T2 = worldMapCounts(d, timepoint = 2),
  worldC1T3 = worldMapCounts(d, timepoint = 3),
  worldC1T4 = worldMapCounts(d, timepoint = 4),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)