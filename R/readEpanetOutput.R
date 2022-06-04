# outputFileSize ---------------------------------------------------------------

#' Size of Binary Output File
#' 
#' Size of binary output file in Bytes, kB (rounded), MB (rounded)
#' @param configuration EPANET \code{configuration}, representing an EPANET
#'   input \code{file}, as returned by \code{\link{readEpanetInputFile}}
#' @return named vector of numeric representing output file size in bytes, kB
#'   (rounded) and MB (rounded), respectively
#' @export
outputFileSize <- function(configuration)
{
  Nnodes <- nrow(configuration$JUNCTIONS)  
  Npumps <- nrow(configuration$PUMPS)
  Ntanks <- nrow(configuration$TANKS)
  Npipes <- nrow(configuration$PIPES)
  Nvalves <- nrow(configuration$VALVES) 
  
  Nperiods <- getNumberOfPeriods(configuration)
  
  Nlinks <- Npipes + Npumps + Nvalves
  
  size.prolog <- 884L + 36L * Nnodes + 52L * Nlinks + 8L * Ntanks 
  size.energy.use <- 28L * Npumps + 4L   
  size.dynamic.results <- (16L * Nnodes + 32L * Nlinks) * Nperiods
  size.epilog <- 28L
  
  bytes <- size.prolog + size.energy.use + size.dynamic.results + size.epilog
  
  c(
    bytes = bytes, 
    kB = round(bytes / 1024), 
    MB = round(bytes / (1024 * 1024))
  )
}

# getNumberOfPeriods -----------------------------------------------------------

#' Number of Simulation Periods
#' 
#' number of simulation periods, calculated from duration and hydraulic time
#'   step both of which must be given in the \[TIMES\] section of the EPANET
#'   configuration
#' @param configuration EPANET configuration, as retrieved by \code{readEpanetInputFile} 
#' @export
#' 
getNumberOfPeriods <- function(configuration)
{
  times <- configuration$TIME
  
  stopifnot(!is.null(times))
  
  t1 <- times[[1L]]
  hydraulic.timestep <- times[t1 == "Hydraulic Timestep", 2L]
  duration <- times[t1 == "Duration", 2L]

  paste_zero_seconds <- function(x) paste(x, "00", sep = ":")
  
  kwb.utils::quotient(
    .hhmmssToSeconds(paste_zero_seconds(duration)),
    .hhmmssToSeconds(paste_zero_seconds(hydraulic.timestep))
  )
}

# reportEnergyUse --------------------------------------------------------------

#' Report Energy Use
#' 
#' @param outdat output data read from EPANET out-file
#' @export
reportEnergyUse <- function(outdat) 
{
  outdat$energyUse
  pumpIDs <- outdat$energyUse$PumpIndexInListOfLinks  
  
  data.frame(
    Pump = outdat$prolog$IDStringOfEachLink[pumpIDs],
    Percent.Utilization = round(outdat$energyUse$PumpUtilization, 2),
    Average.Efficiency = round(outdat$energyUse$AvgEfficiency, 2), 
    Kw.hr.per.m3 = round(outdat$energyUse$AvgKwPerVolume, 4),
    Average.Kwatts = round(outdat$energyUse$AvgKW, 2),
    Peak.Kwatts = round(outdat$energyUse$PeakKW, 2),
    Cost.per.day = round(outdat$energyUse$AvgCostPerDay, 2)
  )
}

# getLinkResults ---------------------------------------------------------------

#' Get Link Results
#' 
#' @param outdat output data read from EPANET output file, as returned by 
#'   \code{\link{readEpanetOutputFile}}
#' @param links names of \code{links} to be included in the returned data frame. You may use
#'   \code{\link{getNamesOfPipes}, \link{getNamesOfPumps}, 
#'   \link{getNamesOfValves}} in order to get the names of available \code{links}
#' @param vars acronyms of variables to be included in the returned data frame. 
#'   "Q" = flow, "v" = velocity, "hl" = headloss, "wq" = avg. water quality, 
#'   "sta" = status, "set" = setting, "rr" = reaction rate, 
#'   "ff" = friction factor
#' @export
getLinkResults <- function(
  outdat, links, vars = c("q", "v", "hl", "wq", "sta", "set", "rr", "ff")
)
{
  .getLinkOrNodeResults(outdat, TRUE, links, vars)
}

# getNodeResults ---------------------------------------------------------------

#' Get Node Results
#' 
#' @param outdat output data read from EPANET output file, as returned by 
#'   \code{\link{readEpanetOutputFile}}
#' @param nodes names of \code{nodes} to be included in the returned data frame. You may use
#'   \code{\link{getNamesOfJunctions}, \link{getNamesOfReservoirs}, 
#'   \link{getNamesOfTanks}} in order to get the names of available \code{nodes}
#' @param vars acronyms of variables to be included in the returned data frame. 
#'   "d" = demand, "h" = head, "p" = pressure, "wq" = water quality
#' @export
getNodeResults <- function(outdat, nodes, vars = c("d", "h", "p", "wq"))
{
  .getLinkOrNodeResults(outdat, FALSE, nodes, vars)
}

# .getLinkOrNodeResults --------------------------------------------------------
.getLinkOrNodeResults <- function(outdat, linkResults = TRUE, objects, vars)
{
  varInfo <- .variableInfo()
  
  if (linkResults) {
    
    timeSeries <- getLinkTimeseriesFromOutputData(outdat)
    varNameInfo <- varInfo$linkVariables
    
  } else {
    
    timeSeries <- getNodeTimeseriesFromOutputData(outdat)
    varNameInfo <- varInfo$nodeVariables
  }
  
  do.call(rbind, lapply(vars, function(variable) {
    
    element <- varNameInfo[[variable]][2L]
    valueMatrix <- timeSeries[[element]][, objects]
    
    cbind(
      variable = varNameInfo[[variable]][1L], 
      step = seq_len(nrow(valueMatrix)), 
      as.data.frame(valueMatrix)
    )
  }))
}

# .variableInfo ----------------------------------------------------------------
.variableInfo <- function()
{
  list(
    linkVariables = list(
      q = c("flow", "flows"), 
      v = c("velocity", "velocities"), 
      hl = c("headloss", "headlosses"), 
      wq = c("waterQuality", "avgWaterQualities"), 
      sta = c("status", "statusCodes"), 
      set = c("setting", "settings"), 
      rr = c("reactionRate", "reactionRates"), 
      ff = c("frictionFactor", "frictionFactors")
    ), 
    nodeVariables = list(
      d = c("demand", "demands"), 
      h = c("head", "heads"), 
      p = c("pressure", "pressures"), 
      wq = c("waterQuality", "waterQualities")
    )
  )
}

# getPumpPerformance -----------------------------------------------------------

#' Get Pump Performance
#' 
#' Get time series of pump performance from EPANET result using head curves
#'   and efficiency curves as contained in EPANET input file
#' 
#' @param inpdata data structure read from EPANET input file, as returned by 
#'   \code{\link{readEpanetInputFile}}.
#' @param outdata data structure read from EPANET output file, as returned by 
#'   \code{\link{readEpanetOutputFile}}.  
#' @param pumpnames vector with names of pumps for which the pump performance should be evaluated
#' 
#' @return data frame with columns \emph{Q} (discharge), \emph{H} (head), 
#' \emph{Eff} (efficiency), \emph{specEn} (specific efficiency), 
#' \emph{En} (energy)
#' 
#' @seealso \code{\link{plotPumpPerformance}}  
#' @export
getPumpPerformance <- function(inpdata, outdata, pumpnames)
{
  pumpPerformance <- data.frame()
  nodeTimeseries <- getNodeTimeseriesFromOutputData(outdata)
  linkTimeseries <- getLinkTimeseriesFromOutputData(outdata)
  
  for(pumpname in pumpnames)
  {
    efficiencyCurve <- getEfficiencyCurve(inpdata$ENERGY, inpdata$CURVES, pumpname)
    headCurve <- getHeadCurve(inpdata$PUMP, inpdata$CURVES, pumpname)
    
    Q <- linkTimeseries$flows[, pumpname]
    
    H <- stats::approx(
      x = headCurve$X_Value, 
      y = headCurve$Y_Value, 
      xout = Q
    )$y
    
    Eff <- stats::approx(
      x = efficiencyCurve$X_Value, 
      y = efficiencyCurve$Y_Value, 
      xout = Q
    )$y
    
    specEn <- H/Eff/3.67
    En <- specEn*Q
    
    tmp <- data.frame(Q = Q, H = H, Eff = Eff, specEn = specEn, En)
    
    pumpPerformance <- rbind(
      pumpPerformance, 
      data.frame(
        pumpnames = pumpname,
        step = seq_len(nrow(tmp)), 
        tmp
      )
    )
  }
  
  pumpPerformance
}

# plotPumpPerformance ----------------------------------------------------------

#' Plot Pump Performance
#' 
#' Plot time series of discharge, head and pump performance
#' 
#' @param xCols vector of columns contained in data.frame retrieved by getPumpPerformance() 
#'   to be used as for x axis plotting, e.g. c("step", "Q")
#' @param yCols vector of columns contained in data.frame retrieved by getPumpPerformance() 
#'   to be used as for y axis plotting, e.g. c("Eff", "specEn", "En")
#' @param pumpPerformanceTimeSeries pump performance time series as retrieved by getPumpPerformance()
#' @seealso \code{\link{getPumpPerformance}}
#' @export
plotPumpPerformance <- function(xCols, yCols, pumpPerformanceTimeSeries)
{
  # \xb3 = "to the power of three", keep it like this (ASCII required)!
  
  labels <- c(
    Eff = "Global pump efficiency (%)", 
    specEn = "Specific energy demand (kwh/m\xb3)",
    En = "Energy demand (kw)", 
    step = "Reporting step number", 
    Q = "Production rate (m\xb3/h)" 
  )
  
  defaultPlotType <- "l"
  numberOfPumps <- length(unique(pumpPerformanceTimeSeries$pumpnames))
  
  for (xCol in xCols)
  { 
    xLabel <- as.character(labels[names(labels)==xCol])
    
    if (xCol=="Q") defaultPlotType <- "p"
        
    for (yCol in yCols)
    {
      formula <- stats::as.formula(sprintf("%s ~ %s", yCol, xCol))
      yLabel <- as.character(labels[names(labels)==yCol])
          
      print(lattice::xyplot(
        formula, 
        groups = pumpPerformanceTimeSeries$pumpnames, 
        data = pumpPerformanceTimeSeries, 
        type = defaultPlotType, 
        auto.key = list(columns= numberOfPumps),
        xlab = xLabel,
        ylab = yLabel
      ))
    }  
  }
}

# getNodeTimeseriesFromOutputData ----------------------------------------------

#' Get Node Timeseries From Output Data
#' 
#' @param outdat data structure read from EPANET output file, as returned by 
#'   \code{\link{readEpanetOutputFile}}.    
#' @export
getNodeTimeseriesFromOutputData <- function(outdat)
{
  dynamicResults <- outdat$dynamicResults
  prolog <- outdat$prolog
 
  .stopOnEitherNull(prolog, dynamicResults)
  
  get_ts <- function(x) .getNodePropertyTimeSeries(dynamicResults, x, prolog)
  
  list(
    demands = get_ts("DemandAtEachNode"),
    heads = get_ts("HeadAtEachNode"),
    pressures = get_ts("PressureAtEachNode"),
    waterQualities = get_ts("WaterQualityAtEachNode")
  )
}

# getLinkTimeseriesFromOutputData ----------------------------------------------

#' Get Link Timeseries From Output Data
#' 
#' @param outdat data structure read from EPANET output file, as returned by 
#'   \code{\link{readEpanetOutputFile}}.    
#' 
getLinkTimeseriesFromOutputData <- function(outdat)
{
  dynamicResults <- outdat$dynamicResults
  prolog <- outdat$prolog

  .stopOnEitherNull(prolog, dynamicResults)
  
  get_ts <- function(x) .getLinkPropertyTimeSeries(dynamicResults, x, prolog)
  
  list(
    flows = get_ts("FlowInEachLink"),
    velocities = get_ts("VelocityInEachLink"),
    headlosses = get_ts("HeadlossForEachLink"),
    avgWaterQualities = get_ts("AvgWaterQualityInEachLink"),
    statusCodes = get_ts("StatusCodeForEachLink"),
    settings = get_ts("SettingForEachLink"),
    reactionRates = get_ts("ReactionRateForEachLink"),
    frictionFactors = get_ts("FrictionFactorForEachLink")
  )
}

# .stopOnEitherNull ------------------------------------------------------------
.stopOnEitherNull <- function(prolog, dynamicResults)
{
  if (is.null(prolog) || is.null(dynamicResults)) {
    stop(
      "Either or both of the elements 'prolog' and 'dynamicResults' not ", 
      "found in outdat!"
    )
  }
}

# showProperties ---------------------------------------------------------------

#' Show Properties
#' 
#' Show node and link properties available in EPANET output
#' 
#' @param outdata list structure with EPANET results as retrieved by 
#'   \code{\link{readEpanetOutputFile}}
#' 
showProperties <- function(outdata) 
{
  cat("Available node properties:\n")
  print(names(outdata$dynamicResults[[1L]]$NodeData))
  #[1] "DemandAtEachNode"       "HeadAtEachNode"         "PressureAtEachNode"    
  #[4] "WaterQualityAtEachNode"

  cat("Available link properties:\n")
  print(names(outdata$dynamicResults[[1L]]$LinkData))
  #[1] "FlowInEachLink"            "VelocityInEachLink"       
  #[3] "HeadlossForEachLink"       "AvgWaterQualityInEachLink"
  #[5] "StatusCodeForEachLink"     "SettingForEachLink"       
  #[7] "ReactionRateForEachLink"   "FrictionFactorForEachLink"  
}

# readEpanetOutputFile ---------------------------------------------------------

#' Read EPANET Output File 
#' 
#' @param outfile full path to EPANET output file
#' @param read.prolog if TRUE, the "Prolog" section is read from the output file and contained
#'   in the output list
#' @param read.energyUse if TRUE, the "Energy Use" section is read from the output file and
#'   contained in the output list
#' @param read.dynamicResults if TRUE, the "Extended Period" section is read from the output file and
#'   contained in the output list
#' @param read.epilog if TRUE, the "Epilog" section is read from the output file and contained
#'   in the output list
#' 
#' @return list with elements \emph{prolog} (if \code{read.prolog} = TRUE), \emph{energyUse}
#'   (if \code{read.energyUse} = TRUE), \emph{dynamicResults} (if \code{read.dynamicResults}
#'   = TRUE) and \emph{epilog} (if \code{read.epilog} = TRUE), containing the
#'   different parts of the output file, as described in the documentation of
#'   the EPANET Toolkit.
#' 
#' @seealso \code{\link{readEpanetInputFile}}
#' @export
readEpanetOutputFile <- function(
  outfile,
  read.prolog = TRUE,
  read.energyUse = TRUE,
  read.dynamicResults = TRUE,
  read.epilog = TRUE
) 
{
  con <- file(outfile, "rb")
  on.exit(close(con))
  
  # Always read prolog and energy use
  prolog <- .readProlog(con)  
  energyUse <- .readEnergyUse(con, prolog)  
  
  if (read.dynamicResults || read.epilog) {
    dynamicResults <- .readDynamicResults(con, prolog)  
  }
  
  #unknown <- .readStringOfLength(con, 4)
  
  if (read.epilog) {
    epilog <- .readEpilog(con)    
  }
  
  # initialise result list
  result <- list()
  
  if (read.prolog) {
    result$prolog <- prolog
  }
  
  if (read.energyUse) {
    result$energyUse <- energyUse
  }
  
  if (read.dynamicResults) {
    result$dynamicResults <- dynamicResults
  }
  
  if (read.epilog) {
    epilog <- .readEpilog(con)    
  }
  
  result
}

# .getNodePropertyTimeSeries ---------------------------------------------------
.getNodePropertyTimeSeries <- function(dynamicResults, property, prolog)
{
  .getNodeOrLinkPropertyTimeSeries(dynamicResults, property, prolog, "Node")
}

# .getLinkPropertyTimeSeries ---------------------------------------------------
.getLinkPropertyTimeSeries <- function(dynamicResults, property, prolog)
{
  .getNodeOrLinkPropertyTimeSeries(dynamicResults, property, prolog, "Link")
}

# .getNodeOrLinkPropertyTimeSeries ---------------------------------------------
.getNodeOrLinkPropertyTimeSeries <- function(
  dynamicResults, property, prolog, nodeOrLink
)
{
  name1 <- paste(nodeOrLink, "Data", sep = "")
  name2 <- paste("IDStringOfEach", nodeOrLink, sep = "")
  
  m <- t(
    sapply(
      dynamicResults, 
      FUN = function(x, property) { 
        x[[name1]][[property]] 
      }, 
      property
    )
  )
  
  colnames(m) <- prolog[[name2]]
  
  m
}

# .readEpilog ------------------------------------------------------------------
.readEpilog <- function(con)
{
  floatValues <- .readDbl(con, 4)
  intValues <- .readInt(con, 3)
  
  list(
    AvgBulkReactionRate = floatValues[1], AvgWallReactionRate = floatValues[2], 
    AvgTankReactionRate = floatValues[3], AvgSourceInflowRate = floatValues[4], 
    NumberOfReportingPeriods = intValues[1], WarningFlag = intValues[2], 
    MagicNumber = intValues[3]
  )
}

# .numberOfPeriods -------------------------------------------------------------
.numberOfPeriods <- function(prolog)
{
  prolog$SimulationDuration/prolog$ReportingTimeStep + 1
}

# .readDynamicResults ----------------------------------------------------------
.readDynamicResults <- function(con, prolog)
{
  dynamicResults <- list()
  
  for (i in seq(1, by = 1, length.out = .numberOfPeriods(prolog))) {
    dynamicResults[[i]] <- .readDynamicResultsForOnePeriod(
      con, prolog$NumberOfNodes, prolog$NumberOfLinks
    )
  }
  
  dynamicResults
}

# .readEnergyUse ---------------------------------------------------------------
.readEnergyUse <- function(con, prolog)
{
  energyUse <- NULL
  
  for (i in seq(1, by = 1, length.out = prolog$NumberOfPumps)) {
    energyUse <- rbind(energyUse, .readEnergyUseOfOnePump(con))
  }
  
  PeakEnergyUsage <- readBin(con, double(), size = 4)
  
  energyUse
}

# .readDynamicResultsForOnePeriod ----------------------------------------------
.readDynamicResultsForOnePeriod <- function(con, numberOfNodes, numberOfLinks)
{
  list(
    NodeData = data.frame(
      DemandAtEachNode = .readDbl(con, numberOfNodes), 
      HeadAtEachNode = .readDbl(con, numberOfNodes),
      PressureAtEachNode = .readDbl(con, numberOfNodes), 
      WaterQualityAtEachNode = .readDbl(con, numberOfNodes)
    ), 
    LinkData = data.frame(
      FlowInEachLink = .readDbl(con, numberOfLinks), 
      VelocityInEachLink = .readDbl(con, numberOfLinks), 
      HeadlossForEachLink = .readDbl(con, numberOfLinks), 
      AvgWaterQualityInEachLink = .readDbl(con, numberOfLinks), 
      StatusCodeForEachLink = .readDbl(con, numberOfLinks), 
      SettingForEachLink = .readDbl(con, numberOfLinks), 
      ReactionRateForEachLink = .readDbl(con, numberOfLinks), 
      FrictionFactorForEachLink = .readDbl(con, numberOfLinks)
    )
  )
}

# .readEnergyUseOfOnePump ------------------------------------------------------
.readEnergyUseOfOnePump <- function(con)
{
  data.frame(
    PumpIndexInListOfLinks = readBin(con, integer()), 
    PumpUtilization = readBin(con, double(), size = 4), 
    AvgEfficiency = readBin(con, double(), size = 4), 
    AvgKwPerVolume = readBin(con, double(), size = 4), 
    AvgKW = readBin(con, double(), size = 4), 
    PeakKW = readBin(con, double(), size = 4), 
    AvgCostPerDay = readBin(con, double(), size = 4), 
    PeakEnergyUsage = NA
  )
}

# .readNumberOfStringsOfLength -------------------------------------------------
.readNumberOfStringsOfLength <- function(con, numberOfStrings, stringLength)
{
  strings <- character()
  
  for (i in seq(1, by = 1, length.out = numberOfStrings)) {
    strings <- c(strings, .readStringOfLength(con, stringLength))
  }
  
  strings
}

# .readStringOfLength ----------------------------------------------------------
.readStringOfLength <- function(con, stringLength)
{
  rawToChar(readBin(con, raw(), stringLength))
}

# .readProlog ------------------------------------------------------------------
.readProlog <- function(con)
{
  prolog <- as.list(readBin(con, integer(), 15))
  
  names(prolog) <- c(
    "MagicNumber", "Version", "NumberOfNodes", 
    "NumberOfReservoirsAndTanks", "NumberOfLinks", "NumberOfPumps", 
    "NumberOfValves", "WaterQualityOption", "IndexOfNodeForSourceTracing", 
    "FlowUnitsOption", "PressureUnitsOption", "TimeStatisticsFlag", 
    "ReportingStartTime", "ReportingTimeStep", "SimulationDuration"
  )
  
  prolog$ProblemTitle1 <- .readStringOfLength(con, 80)
  prolog$ProblemTitle2 <- .readStringOfLength(con, 80)
  prolog$ProblemTitle3 <- .readStringOfLength(con, 80)
  prolog$NameOfInputFile <- .readStringOfLength(con, 260)
  prolog$NameOfReportFile <- .readStringOfLength(con, 260)
  prolog$NameOfChemical <- .readStringOfLength(con, 32)
  prolog$ChemicalConcentrationUnits <- .readStringOfLength(con, 32)
  prolog$IDStringOfEachNode <- .readNumberOfStringsOfLength(con, prolog$NumberOfNodes, 32)
  prolog$IDStringOfEachLink <- .readNumberOfStringsOfLength(con, prolog$NumberOfLinks, 32)
  prolog$IndexOfHeadNodeOfEachLink <- .readInt(con, prolog$NumberOfLinks)
  prolog$IndexOfTailNodeOfEachLink <- .readInt(con, prolog$NumberOfLinks)
  prolog$TypeCodeOfEachLink <- .readInt(con, prolog$NumberOfLinks)
  prolog$NodeIndexOfEachTank <- .readInt(con, prolog$NumberOfReservoirsAndTanks)
  prolog$CrossSectionalAreaOfEachTank <- .readDbl(con, prolog$NumberOfReservoirsAndTanks)
  prolog$ElevationOfEachNode <- .readDbl(con, prolog$NumberOfNodes)
  prolog$LengthOfEachLink <- .readDbl(con, prolog$NumberOfLinks)
  prolog$DiameterOfEachLink <- .readDbl(con, prolog$NumberOfLinks)
  
  prolog
}

# .readInt ---------------------------------------------------------------------
.readInt <- function(con, n)
{
  readBin(con, integer(), n)
}

# .readDbl ---------------------------------------------------------------------
.readDbl <- function(con, n)
{
  readBin(con, double(), n, size = 4)
}
