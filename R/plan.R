# drake plan
plan <- drake_plan(
  # load data
  dLong = loadData("data/Corona - cohort 1 - Times 1-4 - Long - Excluding missing region.sav"),
  dWide = loadData("data/Corona - cohort 1 - Times 1-4 - Wide - Excluding missing region.sav"),
  # line plots
  linePlotList = makePlotList(
    dLong, read.csv("data/plotting/lineBarPlots.csv",
                    stringsAsFactors = FALSE,
                    allowEscapes = TRUE), "line"
    ),
  # bar plots
  barPlotList = makePlotList(
    dLong, read.csv("data/plotting/lineBarPlots.csv",
                    stringsAsFactors = FALSE,
                    allowEscapes = TRUE), "bar"
    ),
  # line plot grids
  linePlotGridList = makeGridList(linePlotList, "line"),
  # bar plot grids
  barPlotGridList = makeGridList(barPlotList, "bar"),
  # cor plots
  corC1T1 = makeCorPlot(dLong, timepoint = 1),
  corC1T2 = makeCorPlot(dLong, timepoint = 2),
  corC1T3 = makeCorPlot(dLong, timepoint = 3),
  corC1T4 = makeCorPlot(dLong, timepoint = 4),
  # table
  tableCount = countTableRegion(dLong),
  # world
  worldMap = worldMapCounts(dLong),
  # render report
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)
