# calculateSpecificEnergyDemand ------------------------------------------------

#' Calculate Specific Energy Demand
#' 
#' @return list with elements \emph{Q}, \emph{E}, \emph{Eff}, holding the column
#'   names of \emph{totalEnergy}, corresponding to discharge, energy demand and
#'   efficiency, respectively. Default: list(Q = "Q.m3.per.hour.sum", E =
#'   "Kw.hr.per.m3.avg", Eff = "Average.Efficiency.avg")
#' 
calculateSpecificEnergyDemand <-  function(
  waterDemand,
  totalEnergy,
  COLNAMES = list(
    Q = "Q.m3.per.hour.sum", 
    E = "Kw.hr.per.m3.avg", 
    Eff = "Average.Efficiency.avg"
  )
)
{
  # minimum specific energy of configurations satisfying the water demand
  satisfactory <- (totalEnergy[, COLNAMES$Q] >= waterDemand)
  minEnergy <- min(totalEnergy[satisfactory, COLNAMES$E])
  
  # more efficient (but not satisfying the demand)
  indicesOfMoreEfficient <- which(totalEnergy[, COLNAMES$E] < minEnergy)
  
  if (length(indicesOfMoreEfficient) > 0)
  {
    # initialise result data frame
    allCombinedSchemes <- data.frame()
    
    for (index in indicesOfMoreEfficient) {
      
      combinedSchemes <- .findCombinedSchemes(
        allSchemes = totalEnergy, 
        baseScheme = totalEnergy[index, ],
        waterDemand = waterDemand, 
        COLNAMES = COLNAMES
      )
      
      # Filter for schemes with less energy demand than needed by the most
      # efficient scheme satisfying the water demand "alone"
      belowMinEnergy  <- combinedSchemes[, COLNAMES$E] < minEnergy
      combinedSchemes <- combinedSchemes[belowMinEnergy, ]
      
      allCombinedSchemes <- rbind(allCombinedSchemes, combinedSchemes)
    }
    
    cat("allCombinedSchemes:\n")
    print(allCombinedSchemes)
    
    totalEnergy <- rbind(totalEnergy, allCombinedSchemes)
    
    # Renumber the operation schemes
    totalEnergy$opSchemeID <- seq_len(nrow(totalEnergy))
    
  } else {
    
    cat(
      "No better solution for minimising specific energy demand in case of", 
      "using two different operation schemes a day"
    )
  }
  
  ### Number of different pump configuration schemes per day
  totalEnergy$configsPerDay <- 1
  totalEnergy$configsPerDay[is.na(totalEnergy$nOn)] <- 2
  totalEnergy
}

# .findCombinedSchemes ---------------------------------------------------------
.findCombinedSchemes <- function(allSchemes, baseScheme, waterDemand, COLNAMES)
{
  allSchemes$timeFraction <- kwb.utils::quotient(
    waterDemand - allSchemes[[COLNAMES$Q]], 
    baseScheme[[COLNAMES$Q]] - allSchemes[[COLNAMES$Q]]
  )
  
  combiSchemes <- allSchemes[kwb.utils::inRange(allSchemes$timeFraction, 0, 1), ]
  
  x <- combiSchemes$timeFraction
  V1 <- x * baseScheme[[COLNAMES$Q]]
  V2 <- (1 - x) * combiSchemes[[COLNAMES$Q]]
  
  combiSchemes[[COLNAMES$Q]] <- V1 + V2
  
  for (columnName in c(COLNAMES$Eff, COLNAMES$E)) {
    combiSchemes[[columnName]] <- .weightedAverage(
      weight1 = V1, 
      value1 = baseScheme[[columnName]], weight2 = V2, 
      value2 = combiSchemes[[columnName]]
    )
  }
  
  combiSchemes$opSchemeID <- NA
  combiSchemes$nOn <- NA
  
  combiSchemes$PumpConfigName <- paste(
    baseScheme$PumpConfigName, combiSchemes$PumpConfigName, sep = " & "
  )
  
  kwb.utils::removeColumns(combiSchemes, "timeFraction")
}

# .weightedAverage -------------------------------------------------------------
.weightedAverage <- function(weight1, value1, weight2, value2)
{
  (weight1 * value1 + weight2 * value2) / (weight1 + weight2)
}

# plotOptimisationResults ------------------------------------------------------

#' Plot Optimisation Results
#' 
plotOptimisationResults <- function(
  totalEnergy, 
  name="",
  pumpsToReplace="",
  userConstraints,
  currentOperation,
  ...
)
{
  pch <- .getPlotCharacters(
    badQualityWells = userConstraints$namesOfWellsWithQualityProblems,
    configurationNames = totalEnergy$PumpConfigName,
    Q = totalEnergy$Q.m3.per.hour,
    demand = userConstraints$waterDemand       
  )
  
  x <- totalEnergy$Kw.hr.per.m3.avg       
  y <- totalEnergy$Average.Efficiency.avg
  
  plot3D::scatter2D(
    x = x, 
    y = y,
    colvar = totalEnergy$Q.m3.per.hour.sum,
    col = rev(plot3D::jet.col(n = nrow(totalEnergy))),
    pch = pch, 
    xlab = "Specific energy (kWh/m\u00b3)", 
    ylab = "Average efficiency (%)",
    clab = "Total\npumping rate\n(m\xb3/h)", # \xb3 = "to the power of three"!
    main = .defaultLabel(name, pumpsToReplace), 
    las = 1,
    ...
  )
  
  graphics::legend(
    "topright", pch = c(2, 1, 17, 16), 
    legend = c(
      "< demand & quality possibly not ok",
      "< demand & quality ok",
      "> demand & quality possibly not ok",
      "> demand & quality ok"
    )
  )
  
  # draw vertical line at specific energy of current operation (if applicable)
  specificEnergy <- currentOperation$SpecificEnergy
  
  if (! is.null(specificEnergy) && !is.na(specificEnergy)) {    
    graphics::abline(v = specificEnergy, col = "grey", lty = 2)
    graphics::text(
      specificEnergy,
      kwb.plot::getPlotRegionSizeInUserCoords()$bottom + 2, 
      col = "grey", 
      labels = currentOperation$Label
    )
  }
  
  indices <- which(is.na(totalEnergy$nOn) | totalEnergy$nOn == 1)
  
  plot3D::text2D(
    x = x[indices], # * 0.95, 
    y = y[indices] + kwb.plot::cmToUserWidthAndHeight(cm = 0.3)$height, # * 0.95, , 
    labels = totalEnergy$PumpConfigName[indices], 
    add = TRUE, 
    col = "black",
    adj = 0.5
  )
}

# .getPlotCharacters -----------------------------------------------------------
.getPlotCharacters <- function(badQualityWells, configurationNames, Q, demand)
{
  PCH <- kwb.plot:::getPlotCharacterConstants()
  containsBadQualityWell <- grepl(pattern = paste(badQualityWells, 
                                                  collapse = "|"), x = configurationNames)
  pch <- rep(PCH$FILLED_CIRCLE, length(Q))
  pch[!containsBadQualityWell & Q < demand] <- PCH$CIRCLE
  pch[containsBadQualityWell & Q < demand] <- PCH$TRIANGLE
  pch[containsBadQualityWell & Q > demand] <- PCH$FILLED_TRIANGLE
  pch
}

# .defaultLabel ----------------------------------------------------------------
.defaultLabel <- function(name, pumpsToReplace)
{
  paste("Optimisation strategy:", name, "\n", .replacedPumpsInfo(pumpsToReplace))
}

# .replacedPumpsInfo -----------------------------------------------------------
.replacedPumpsInfo <- function(pumpsToReplace) 
{
  if (any(pumpsToReplace != "")) {
    
    sprintf(" ( %s )", paste(pumpsToReplace, collapse = " , "))
    
  } else {
    
    ""
  }
}

# runOptimisationStrategy ------------------------------------------------------

#' Run Optimisation Strategy
#' 
#' @param operationSchemes possible wellfield operation schemes. Default:
#'   wellFieldOperationSchemes(getNamesOfPumps(\code{configuration}))
#' 
#' @return list with elements \emph{energyTotal} and \emph{energyPerPump}
#' 
runOptimisationStrategy <- function(
  configuration, 
  newCurvesData,
  optimisationStrategy,
  operationSchemes = wellFieldOperationSchemes(getNamesOfPumps(configuration)),
  showLivePlot = FALSE
)
{
  # Replace pumps if required
  if (any(optimisationStrategy$pumpsToReplace != "")) {
    configuration <- .replacePumpsInConfiguration(
      configuration = configuration,
      newCurvesData = newCurvesData, 
      pumpsToReplace = optimisationStrategy$pumpsToReplace
    )
  }
  
  # Prepare main title and average demand for plots (out of the following loop)
  if (showLivePlot) {    
    main <- .defaultLabel(
      name = optimisationStrategy$name, 
      pumpsToReplace = optimisationStrategy$pumpsToReplace
    )    
    averageDemand <- sum(as.numeric(configuration$DEMANDS$Demand))    
  }  
  
  # Initialise result data frames
  energyPerPump <- data.frame()  
  energyTotal <- data.frame()

  for (schemeID in operationSchemes$ID) {
    
    cat("operation Scheme", schemeID, "/", nrow(operationSchemes), "...\n")
    
    # Generate a new EPANET configuration
    configuration <- setWellFieldOperation (
      config = configuration,
      operationScheme = operationSchemes[operationSchemes$ID == schemeID, ]
    )
    
    # Run EPANET
    epanetResult <- runEpanetConfiguration (inpdat = configuration)
    
    # Read and calculate energy usage
    tmpEnergyPerPump <- .getEnergyPerPump(
      epanetOutput = epanetResult$output, 
      configuration = configuration, 
      schemeID = schemeID)
    
    energyPerPump <- rbind(
      energyPerPump, 
      data.frame(tmpEnergyPerPump, stringsAsFactors = FALSE)
    )
    
    tmpEnergyTotal <- .energyPerPumpToTotalEnergy(
      tmpEnergyPerPump = tmpEnergyPerPump, 
      operationSchemes = operationSchemes,
      schemeID = schemeID)
        
    energyTotal <- rbind(
      energyTotal, 
      tmpEnergyTotal
    )

    if(showLivePlot) {
      .liveplot(energyTotal, main, averageDemand)
    }    
  }
  
  energyTotal <- merge(
    energyTotal, 
    operationSchemes[ , c("ID", "nOn")], 
    by.x = "opSchemeID", 
    by.y = "ID"
  )
  
  list(
    energyTotal = energyTotal, 
    energyPerPump = energyPerPump
  )
}

# .replacePumpsInConfiguration -------------------------------------------------
.replacePumpsInConfiguration <- function(configuration, newCurvesData, pumpsToReplace)
{
  pumpCurve <- .getCurveWithReplacedID(
    curveData = newCurvesData$Pump, 
    pumpsToReplace = pumpsToReplace, 
    prefix = newCurvesData$PumpNamePrefix$PumpCurves
  )
  
  efficiencyCurve <- .getCurveWithReplacedID(
    curveData = newCurvesData$GlobalPumpEfficiency, 
    pumpsToReplace = pumpsToReplace, 
    prefix = newCurvesData$PumpNamePrefix$GlobalPumpEfficiency
  )
  
  configuration$CURVES <- replaceCurves(
    curves = configuration$CURVES, 
    newCurves = rbind(pumpCurve, efficiencyCurve)
  )
  
  configuration
}

# .getCurveWithReplacedID ------------------------------------------------------
.getCurveWithReplacedID <- function(curveData, pumpsToReplace, prefix)
{
  curve <- curveData[curveData$ID %in% pumpsToReplace, ]
  curve$ID <- paste(prefix, curve$ID, sep = "")
  curve
}

# .getEnergyPerPump ------------------------------------------------------------
.getEnergyPerPump <- function(epanetOutput, configuration, schemeID)
{
  flowsInListForm <- .getFlowsInListForm(
    epanetOutput = epanetOutput, 
    configuration = configuration
  )
  
  .getExtendedEnergyReport(
    epanetOutput = epanetOutput, 
    schemeID = schemeID, 
    flowsInListForm = flowsInListForm
  )
}

# .getFlowsInListForm ----------------------------------------------------------
.getFlowsInListForm <- function(epanetOutput, configuration)
{
  flows <- getLinkResults(
    epanetOutput, 
    links = getNamesOfPumps(configuration), 
    vars = "q"
  )
  
  kwb.utils::hsMatrixToListForm(
    df = flows, 
    keyFields = c("step", "variable"), 
    colNamePar = "Pump", 
    colNameVal = "Q.m3.per.hour"
  )
}

# .getExtendedEnergyReport -----------------------------------------------------
.getExtendedEnergyReport <- function(epanetOutput, schemeID, flowsInListForm)
{
  energyReport <- reportEnergyUse(epanetOutput)
  energyReport <- energyReport[energyReport$Percent.Utilization > 
                                 0, ]
  merge(
    data.frame(opSchemeID = schemeID, energyReport), 
    flowsInListForm[flowsInListForm$step == 1, -(1:2)]
  )
}

# .energyPerPumpToTotalEnergy --------------------------------------------------
.energyPerPumpToTotalEnergy <- function(
  tmpEnergyPerPump, operationSchemes, schemeID, COL.Q = "Q.m3.per.hour"
)
{
  Q.sum <- sum(tmpEnergyPerPump[[COL.Q]], na.rm = TRUE)
  data.frame(
    opSchemeID = schemeID, 
    PumpConfigName = operationSchemes$Label[operationSchemes$ID == schemeID], 
    Q.m3.per.hour.sum = Q.sum, 
    Average.Efficiency.avg = kwb.utils::quotient(
      sum(tmpEnergyPerPump$Average.Efficiency * tmpEnergyPerPump[[COL.Q]]), 
      Q.sum
    ), 
    Kw.hr.per.m3.avg = sum(kwb.utils::quotient(
      tmpEnergyPerPump$Kw.hr.per.m3 * tmpEnergyPerPump[[COL.Q]], 
      Q.sum
    )), 
    stringsAsFactors = FALSE
  )
}

# .liveplot --------------------------------------------------------------------
.liveplot <- function(energyTotal, main, averageDemand)
{
  graphics::plot(
    Kw.hr.per.m3.avg ~ Q.m3.per.hour.sum, 
    data = energyTotal, 
    type = "p", 
    pch = 16, 
    main = main
  )
  
  graphics::abline(v = averageDemand, col = "grey", lty = 2)
}

# wellFieldOperationSchemes ----------------------------------------------------

#' Well Field Operation Schemes
#' 
#' @param pumpNames Vector of \code{pumpNames} (used in EPANET input file section "PUMPS")
#' @param niceLabels Optionally nice labeling. List with elements \emph{searchPattern} and 
#'   \emph{searchReplacement}. Default: list(searchPattern = "pmp",
#'   searchReplacement = "p")
#' 
wellFieldOperationSchemes <- function(
  pumpNames, 
  niceLabels = list(searchPattern = "pmp", searchReplacement = "p")
)
{
  numberOfPumps <- length(pumpNames)
  
  permutationMatrix <- gtools::permutations(
    n = 2,                 # Size of the source vector
    r = numberOfPumps,     # Size of the target vectors
    v = c(0,1),            # Source vector.
    set = TRUE,            # remove duplicates from the source vector?
    repeats.allowed = TRUE # may constructed vectors have duplicated values?
  )
      
  # Remove the first permutation with only zeroes (= all pumps off)
  permutationMatrix <- permutationMatrix[-1, ]
  
  # Create data frame with one column per pump (containing 0 for "off" 
  # or 1 for "on") 
  configs <- as.data.frame(permutationMatrix)
  colnames(configs) <- pumpNames
  
  # Add columns nOn (number of "on" pumps), ID, Label
  configs$nOn <- rowSums(configs)
  configs$ID <- seq_len(nrow(configs))

  # Create nice pump names for labels
  pumpNamesShort <- sub(
    pattern = niceLabels$searchPattern, 
    replacement = niceLabels$searchReplacement, 
    x = pumpNames
  )  
  
  # Assign label containing the names of active pumps, separated by " | "
  configs$Label <- apply(
    X = permutationMatrix, 
    MARGIN = 1, # by rows
    FUN = function(x) paste(pumpNamesShort[x == 1], collapse = " | ")
  )
  
  configs
}

# setWellFieldOperation --------------------------------------------------------

#' Set Well Field Operation
#' 
setWellFieldOperation <- function(config, operationScheme)
{
  columnNames <- colnames(operationScheme)
  pumpColumnNames <- columnNames[seq_len(ncol(operationScheme) - 3)]
  
  operating <- operationScheme[, pumpColumnNames] == 1
  namesOfOperatingPumps <- pumpColumnNames[operating]
  
  isPumpStatus <- config$STATUS$ID %in% config$PUMPS$ID
                  
  if (any(isPumpStatus)) {
    config$STATUS <- config$STATUS[! isPumpStatus, ] 
  }
  
  pumpStatus <- data.frame(
    ID = config$PUMPS$ID, 
    "Closed", 
    stringsAsFactors = FALSE
  )
  
  operating <- (pumpStatus$ID %in% namesOfOperatingPumps)
  pumpStatus[operating, 2] <- "Open"
  
  names(pumpStatus) <- names(config$STATUS)
  
  config$STATUS <- rbind(config$STATUS, pumpStatus)
  
  config
}

# fitnessAdaptedModelConfiguration ---------------------------------------------

#' Fitness Adapted Model Configuration
#' 
#' Called by calibrateModel
#' 
fitnessAdaptedModelConfiguration <- function(
  parameterValue, 
  parameterName, 
  configuration, 
  pipeIDs, 
  measured, 
  pumpsToCalibrate, 
  showLivePlot
) 
{
  # reset graphical parameters on exit
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(graphicalParameters))
  
  calibrationRunNumber <- kwb.utils::getGlobally("calibrationRunNumber", default = 0) + 1
  
  indices <- which(configuration$PIPES$ID %in% pipeIDs) 
  
  newParaValue.numeric <- as.numeric(configuration$PIPES[[parameterName]][indices])
  
  newParaValue <- as.character(parameterValue * newParaValue.numeric)

  cat(sprintf("New parameter value(s): %s", 
              paste(newParaValue, sep = " ", collapse= " : ")))
  
  newconfiguration <- configuration  
  newconfiguration$PIPES[[parameterName]][indices] <- newParaValue
  
  #newconfigurationResult <<- runEpanetConfiguration(newconfiguration)
  newconfigurationResult <- runEpanetConfiguration(newconfiguration)
  
  modelled <- getLinkResults(
    outdat = newconfigurationResult$output,
    links=getNamesOfPumps(newconfiguration),
    vars = "q"
  )
  
  if (nrow(measured) >  1) {
    
    modelled <- sapply(
      modelled[,as.character(measured$pumpNames)],
      function(x) stats::median(replace(x, x == 0, NA), na.rm = TRUE)
    )
    
    modelled <- data.frame(
      pumpNames = names(modelled), 
      modelledQ = modelled
    )
    
  } else {
    
    modelled <- stats::median(
      sapply(
        modelled[, as.character(measured$pumpNames)], 
        function(x) replace(x, x == 0, NA)
      ), 
      na.rm = TRUE
    )
    
    modelled <- data.frame(
      pumpNames = as.character(measured$pumpNames), 
      modelledQ = modelled
    )
  }
  
  res <- merge(modelled, measured)
  res$Qerror <- res$modelledQ - res$measuredQ
  res$calibrationRunNumber <- calibrationRunNumber
  
  pumpNames <- as.character(unique(res$pumpNames))
  pumpInfo <- data.frame(
    pumpNames = pumpNames,
    colors = grDevices::rainbow(n = length(pumpNames)),
    stringsAsFactors = FALSE
  )
  
  pumpInfo$pch <- 1
  pumpInfo$pch[pumpInfo$pumpNames %in% pumpsToCalibrate] <- 16
  pumpInfo$pumpsToCalibrate <- paste(pumpsToCalibrate, sep="", collapse= " & ") 
  
  res <- merge(res, pumpInfo)
  
  newRes <- rbind(kwb.utils::getGlobally("newRes", default = NULL), res)
  
  if (showLivePlot)
  {
    graphics::par(xpd = TRUE)
    
    graphics::plot(
      Qerror ~ calibrationRunNumber, 
      pch = newRes$pch, 
      col = newRes$colors, 
      data = newRes, 
      ylab ="Q error (m\u00b3/h)",
      las = 1
    )
    
    plotRegion <- kwb.plot::getPlotRegionSizeInUserCoords()
    yOffset <- kwb.plot::cmToUserWidthAndHeight(cm = 2)$height
    
    graphics::legend(
      x = plotRegion$left + plotRegion$width / 2,
      y = plotRegion$top + yOffset, 
      xjust = 0.5,          
      bty = "n",
      ncol = nrow(pumpInfo),
      legend = pumpInfo$pumpNames, 
      col = pumpInfo$colors,
      pch = pumpInfo$pch
    ) 
    
    graphics::par(xpd = FALSE)
  }
  
  resToCalibrate <- res[res$pumpNames %in% pumpsToCalibrate, ]
  
  Qerror_abs <- 
    sum(abs(resToCalibrate$modelledQ - resToCalibrate$measuredQ)) / 
    nrow(resToCalibrate)
  
  Qerror_absAllPumps <- sum(abs(res$Qerror))/nrow(res)
  
  kwb.utils::assignGlobally("calibrationRunNumber", calibrationRunNumber)
  kwb.utils::assignGlobally("newconfiguration", newconfiguration)
  kwb.utils::assignGlobally("newconfigurationResult", newconfigurationResult)
  kwb.utils::assignGlobally("newRes", newRes)
  kwb.utils::assignGlobally("Qerror_abs", Qerror_abs)
  kwb.utils::assignGlobally("Qerror_absAllPumps", Qerror_absAllPumps)
  
  Qerror_abs
}

# calibrateModel ---------------------------------------------------------------

#' Calibrate Model
#' 
#' @param configuration EPANET parameterisation, e.g. as retrieved by
#'   readEpanetInputFile()
#' @param pipeIDs regular expression or name of pipeID(s) to be used for
#'   calibration
#' @param measured measurement data for all pumps as data.frame e.g.
#'   data.frame(pumpNames=c("pmpW1", "pmpW2"), measuredQ=c(140,190))
#' @param pumpsToCalibrate regular expression or name of pumps to be used for
#'   calibration: e.g. "pmpW1"
#' @param parameterName name of ONE EPANET pipe parameters to be calibrated:
#'   e.g. `Diameter` or `Roughness`
#' @param parameterRange min/max range of possible calibration parameter values:
#'   0-1; with parameterRange*cun = newParameterValue, e.g. parameter range =
#'   0.5 -> 50% reduction of initial value of \code{parameterName} for all
#'   \code{pipeIDs} defined in calibrateModel()
#' @param showLivePlot current calibration status is plotted if
#'   showLivePlot=TRUE. Default: TRUE
#' @param \dots additional parameters to be passed to
#'   fitnessAdaptedModelConfiguration()
#' 
calibrateModel <- function(
  configuration,
  pipeIDs = NULL,
  measured,
  pumpsToCalibrate = NULL,
  parameterName = "Diameter",
  parameterRange,
  showLivePlot = TRUE, 
  ...
) 
{
  optResults <- list()
  calibrationRunNumberStart <- calibrationRunNumber + 1
  
  if (!is.null(pipeIDs) && !is.null(pumpsToCalibrate)) {
    
    optResults <- stats::optimise(
      f = fitnessAdaptedModelConfiguration, 
      interval = parameterRange,
      parameterName = parameterName, 
      configuration = configuration, 
      pipeIDs = pipeIDs,
      measured = measured,
      pumpsToCalibrate = pumpsToCalibrate,
      showLivePlot = showLivePlot,
      ...
    )
    
  } else {
    
    fitnessAdaptedModelConfiguration(
      parameterValue = 1, 
      parameterName = "Diameter", 
      configuration = configuration, 
      pipeIDs = pipeIDs, 
      measured = measured, 
      pumpsToCalibrate = pumpsToCalibrate, 
      showLivePlot = showLivePlot
    )
  }
  
  cat(sprintf("Avg. absolute Q error (calibrated pumps):%2.6f m\u00b3/h\n", Qerror_abs))
  cat(sprintf("Avg. absolute Q error (all pumps):%2.6f m\u00b3/h\n", Qerror_absAllPumps))
  
  # Provide global variables as local variables (or create if not existing)
  liveplot <- kwb.utils::getGlobally("liveplot", default = data.frame())
  calibrationRunNumber <- kwb.utils::getGlobally("calibrationRunNumber", default = 0)
  Qerror_abs <- kwb.utils::getGlobally("Qerror_abs", default = 0)
  Qerror_absAllPumps <- kwb.utils::getGlobally("Qerror_absAllPumps", default = 0)
  newconfiguration <- kwb.utils::getGlobally("newconfiguration", default = NULL)
  newconfigurationResult <- kwb.utils::getGlobally("newconfigurationResult", default = NULL)
  
  ### Save optimisation results in list
  list(
    parameterName=parameterName, 
    pipeIDs = pipeIDs,
    optimalParameterValue = optResults$minimum, 
    Qerror_calibratedPumps = optResults$objective, 
    Qerror_allPumps = Qerror_absAllPumps,
    epanetConfig = newconfiguration,
    calibrationRunNumberStart = calibrationRunNumberStart,
    calibrationRunNumberEnd = calibrationRunNumber 
  )
}

# plotCalibration --------------------------------------------------------------

#' Plot Calibration
#' 
#' @param newRes expects data.frame object "newRes" as input parameter (is automatically
#'   produced by calibrateModel(). Required columns: \emph{Qerror},
#'   \emph{calibrationRunNumber}, \emph{pch}, \emph{colors}, \emph{pumpNames}
#' 
plotCalibration <- function(newRes)
{
  PCH <- kwb.plot:::getPlotCharacterConstants()
  
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(graphicalParameters))
  
  graphics::par(xpd = TRUE)
  
  graphics::plot(
    Qerror ~ calibrationRunNumber, 
    pch = newRes$pch, 
    col = newRes$colors, 
    data = newRes, 
    ylab = "Q error (m\xb3/h)", # \xb3 = "to the power of three"!
    las = 1
  )
  
  graphics::legend(
    "topright", 
    pch = c(PCH$CIRCLE, PCH$FILLED_CIRCLE), 
    legend = c("uncalibr.", "calibr.")
  )
  
  pumpInfo <- unique(newRes[, c("pumpNames", "colors")])
  plotRegion <- kwb.plot::getPlotRegionSizeInUserCoords()
  yOffset <- kwb.plot::cmToUserWidthAndHeight(cm = 2)$height
  
  graphics::legend(
    x = plotRegion$left+plotRegion$width / 2,
    y = plotRegion$top+yOffset, 
    xjust = 0.5,          
    bty = "n",
    ncol = nrow(pumpInfo),
    legend =  pumpInfo$pumpNames, 
    col = pumpInfo$colors,
    pch = PCH$FILLED_CIRCLE
  )
}

# createOptimisationResultsTable -----------------------------------------------

#' Create Optimisation Results Table
#' 
#' @param optimisationStrategies average daily water demand in m3/h to be satisfied
#' @param averageWaterDemand current specific energy demand
#' @param currentEnergyDemand should only the best solutions be written to data.frame? Default: FALSE
#' 
createOptimisationResultsTable <- function(
  optimisationStrategies, 
  averageWaterDemand, 
  currentEnergyDemand, 
  onlyBestSolutions = FALSE
)
{  
  allBestResults <- data.frame()
  
  for (index in seq_len(length(optimisationStrategies))) {
    
    strategy <- optimisationStrategies[[index]]
    
    configTotal <- strategy$results$energyTotal

    # configTotal is a data frame with columns: opSchemeID, PumpConfigName,
    # Q.m3.per.hour.sum, Average.Efficiency.avg, Kw.hr.per.m3.avg, nOn,
    # configsPerDay)
    
    if (onlyBestSolutions) {

      condition <- configTotal$configsPerDay == 1 & 
        configTotal$Q.m3.per.hour.sum >= averageWaterDemand
      
      minSpecEnergy <- min(configTotal$Kw.hr.per.m3.avg[condition]) + 0.001
      
    } else {
      
      minSpecEnergy <- currentEnergyDemand
    }

    # filter for configurations that satisfy the water demand and that require
    # less specific energy than given in minSpecEnergy
    condition <- configTotal$Q.m3.per.hour.sum >= averageWaterDemand & 
      configTotal$Kw.hr.per.m3.avg <= minSpecEnergy
    
    bestConfigs <- configTotal[condition, ]
    
    # order decreasingly by specific energy
    decreasingOrder <- order(bestConfigs$Kw.hr.per.m3.avg, decreasing = TRUE)
    bestConfigs <- bestConfigs[decreasingOrder, ] 
    
    # select and rearrange columns
    columns <- c(
      "configsPerDay", "PumpConfigName", "Q.m3.per.hour.sum", 
      "Average.Efficiency.avg", "Kw.hr.per.m3.avg"
    )
    
    bestConfigs <- bestConfigs[, columns]
    
    if (nrow(bestConfigs) > 0) {
      
      savingsInPercent <- 100 * kwb.utils::quotient(
        bestConfigs$Kw.hr.per.m3.avg - currentEnergyDemand,
        currentEnergyDemand
      )
      
      bestConfigs$EnergySavingInPercent <- round(savingsInPercent, 2)

      bestResults <- data.frame(
        ID = index, 
        Strategy = strategy$name, 
        ReplacedPumps = kwb.utils::commaCollapsed(strategy$pumpsToReplace), 
        bestConfigs
      )
            
      allBestResults <- rbind(allBestResults, bestResults)
    }
    
  } # end of for loop
  
  allBestResults
}
