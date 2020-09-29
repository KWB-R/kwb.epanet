# writeCurves ------------------------------------------------------------------

#' Write Curves
#' 
#' @param pumpCurves list(data = data.frame(ID, X_Value, Y_Value), label = "")
#' @param efficiencyCurves list(data = data.frame(ID, X_Value, Y_Value), label =
#'   "")
#' @param drawdownCurves list(data = data.frame(ID, X_Value, Y_Value), label =
#'   "")
#' @param deleteOldCurves should all curves in 'epanetConfig' be deleted before
#'   writing?
#' @export
writeCurves <- function(
  epanetConfig,
  pumpCurves = NULL, 
  efficiencyCurves = NULL , 
  drawdownCurves = NULL, 
  deleteOldCurves = TRUE, 
  dbg = TRUE
)                       
{
  epanetConfig <- .deleteCurvesIf(deleteOldCurves, epanetConfig, dbg = dbg)
  
  if (! is.null(pumpCurves)) {
    epanetConfig <- writePumpCurves(
      epanetConfig, pumpCurves$data, dbg = dbg
    )
  }
  
  if (! is.null(drawdownCurves)) {
    epanetConfig <- writeDrawdownCurves(
      epanetConfig, drawdownCurves$data, dbg = dbg
    )
  }

  if (!is.null(efficiencyCurves)) {
    epanetConfig <- writeEfficiencyCurves(
      epanetConfig, efficiencyCurves$data, dbg = dbg
    )
  }
  
  epanetConfig  
}

# writePumpCurves --------------------------------------------------------------

#' Write Pump Curves
#' 
#' @param TDH data.frame(ID, X_Value, Y_Value)
#' 
#' @return Modified EPANET configuration
#' @export
writePumpCurves <- function(
  epanetConfig,
  TDH, 
  curveNamePrefix = "TDH",
  deleteOldCurves = FALSE, 
  dbg = TRUE
)
{  
  kwb.utils::catIf(dbg, "Writing pump curves...\n")
  
  .writeCurves(
    epanetConfig = epanetConfig, 
    curvesTable = TDH, 
    curveNamePrefix = curveNamePrefix, 
    curveType = "pump", 
    dataType = "TDH", 
    deleteOldCurves = deleteOldCurves, 
    dbg = dbg
  )
}

# writeDrawdownCurves ----------------------------------------------------------

#' Write Drawdown Curves
#' 
#' @param DD data.frame(ID, X_Value, Y_Value)
#' @param curveNamePrefix prefix to be used in the curve name
#' @param deleteOldCurves if TRUE all curves in epantetConfig$CURVES will be
#'   deleted before adding new ones
#' 
#' @return Modified EPANET configuration  
#' @export
writeDrawdownCurves <- function(
  epanetConfig,
  DD, 
  curveNamePrefix = "dd", 
  deleteOldCurves = FALSE, 
  dbg = TRUE
)
{
  kwb.utils::catIf(dbg, "Writing drawdown curves...\n")
  
  .writeCurves(
    epanetConfig = epanetConfig, 
    curvesTable = DD, 
    curveNamePrefix = curveNamePrefix, 
    curveType = "drawdown", 
    dataType = "DD", 
    pattern = "W",
    deleteOldCurves = deleteOldCurves, 
    dbg = dbg
  )
}

# writeEfficiencyCurves --------------------------------------------------------

#' Write Efficiency Curves
#' 
#' @param Eff data.frame(ID, X_Value, Y_Value)
#' 
#' @return Modified EPANET configuration  
#' @export
writeEfficiencyCurves <- function(
  epanetConfig,
  Eff, 
  curveNamePrefix = "Eff",
  deleteOldCurves = FALSE, 
  dbg = TRUE
)
{ 
  kwb.utils::catIf(dbg, "Writing efficiency curves...\n")
  
  .writeCurves(
    epanetConfig = epanetConfig, 
    curvesTable = Eff, 
    curveNamePrefix = curveNamePrefix, 
    curveType = "efficiency", 
    dataType = "Eff", 
    deleteOldCurves = deleteOldCurves, 
    dbg = dbg
  )
}

# .writeCurves -----------------------------------------------------------------
.writeCurves <- function(
  epanetConfig, curvesTable, curveNamePrefix, curveType, dataType, pattern = "", 
  deleteOldCurves = FALSE, dbg = TRUE
)
{
  sectionName <- c(pump = "PUMPS", drawdown = "VALVES", efficiency = "PUMPS")[curveType]
  .stopOnMissingColumns(names(curvesTable), curveType)
  
  provided <- unique(curvesTable$ID)
  required <- grep(pattern, epanetConfig[[sectionName]]$ID, value = TRUE)
  
  .stopOnNoMatchingIDAtAll(
    provided = provided, required = required, sectionName = sectionName, 
    curveType = curveType, dataType = dataType, dbg = dbg
  )
  
  epanetConfig <- .deleteCurvesIf(deleteOldCurves, epanetConfig)
  
  if (curveType == "efficiency") {
    epanetConfig$ENERGY <- .modifyEnergyConfiguration(
      energyConfig = epanetConfig$ENERGY, 
      provided = provided, 
      curveNamePrefix = curveNamePrefix
    )
  } else {
    epanetConfig[[sectionName]] <- .modifyPumpOrValveSection(
      sectionConfig = epanetConfig[[sectionName]], 
      provided = provided, curveNamePrefix = curveNamePrefix, 
      curveType = curveType
    )
  }
  
  epanetConfig$CURVES <- .modifyCurvesConfiguration(
    curvesConfig = epanetConfig$CURVES, 
    curvesTable = curvesTable, 
    curveNamePrefix = curveNamePrefix
  )
  
  epanetConfig
}

# .modifyPumpOrValveSection ----------------------------------------------------
.modifyPumpOrValveSection <- function(
  sectionConfig, provided, curveNamePrefix, curveType
)
{
  curvePrefix <- c(pump = "HEAD ", drawdown = "", efficiency = "")[curveType]
  columnName <- c(pump = "Parameters", drawdown = "Setting", efficiency = "")[curveType]
  
  rows <- which(sectionConfig$ID %in% provided)
  
  sectionConfig[rows, columnName] <- paste(
    curvePrefix, curveNamePrefix, sectionConfig$ID[rows], sep = ""
  )
  
  sectionConfig
}

# .modifyEnergyConfiguration ---------------------------------------------------
.modifyEnergyConfiguration <- function(energyConfig, provided, curveNamePrefix)
{
  columnNames <- names(energyConfig)
  columns.1.2 <- c("X1", "X2")
  columns.3.4 <- c("X3", "X4")
  columns.1.4 <- c(columns.1.2, columns.3.4)
  
  if (all(columns.1.2 %in% columnNames)) {
    
    if (all(columns.1.4 %in% columnNames)) {
      
      is.efficiency <- grepl("EFF", energyConfig$X3, ignore.case = TRUE)
      is.provided <- energyConfig$X2 %in% provided
      
      restConfig <- energyConfig[which(!(is.efficiency & is.provided)), ]
      
    } else {
      
      restConfig <- kwb.utils::hsAddMissingCols(energyConfig, columns.3.4, fill.value = "")
    }
    
  } else {
    
    restConfig <- NULL
  }
  
  kwb.utils::safeRowBind(restConfig, .configurePumpEfficiency(
    pumpIDs = provided, 
    curveNamePrefix = curveNamePrefix
  ))
}

# .configurePumpEfficiency -----------------------------------------------------
.configurePumpEfficiency <- function(pumpIDs, curveNamePrefix)
{
  times <- length(pumpIDs)
  
  data.frame(
    X1 = rep("Pump", times = times), 
    X2 = pumpIDs, 
    X3 = rep("Efficiency", times = times), 
    X4 = sprintf("%s%s", curveNamePrefix, pumpIDs), 
    stringsAsFactors = FALSE
  )
}

# .modifyCurvesConfiguration ---------------------------------------------------
.modifyCurvesConfiguration <- function(curvesConfig, curvesTable, curveNamePrefix)
{
  curvesTable$ID <- paste(curveNamePrefix, curvesTable$ID, sep = "")
  affected <- (curvesConfig$ID %in% unique(curvesTable$ID))
  rbind(curvesConfig[!affected, ], curvesTable)
}

# .stopOnMissingColumns --------------------------------------------------------
.stopOnMissingColumns <- function(availableColumns, curveType)
{
  if (!all(c("ID", "X_Value", "Y_Value") %in% availableColumns)) {
    stop(
      .firstLetterUpperCase(curveType), 
      " curve data not supplied in expected format!\n", 
      "data.frame(ID, X_VALUE, Y_VALUE)"
    )
  }
}

# .stopOnNoMatchingIDAtAll -----------------------------------------------------
.stopOnNoMatchingIDAtAll <- function(
  provided, required, sectionName, curveType, dataType, dbg = TRUE
)
{
  objectName <- c(pump = "pump", drawdown = "well", efficiency = "pump")[curveType]
  
  kwb.utils::catIf(dbg, "Checking if", curveType, "curves are supplied for all ")
  kwb.utils::catIf(dbg, .plural(objectName), "in section", kwb.utils::hsQuoteChr(sectionName))
  kwb.utils::catIf(dbg, "... ")
  
  undefined <- setdiff(required, provided)
  numberOfUndefined <- length(undefined)
  noneDefined <- numberOfUndefined == length(required)
  
  if (numberOfUndefined == 0) {
    kwb.utils::catIf(dbg, "Ok!\n")
    return()
  }
  
  if (noneDefined) {
    
    messageText <- paste(
      paste(
        "No", objectName, "ID in section", 
        kwb.utils::hsQuoteChr(sectionName), "defined that matches the names of the given", 
        curveType, "curves:"
      ), 
      paste(
        .firstLetterUpperCase(objectName), "IDs in", kwb.utils::hsQuoteChr(sectionName), 
        "section:"
      ), 
      .spaceSeparatedAndCollapsed(required), 
      paste("Given names of", curveType, "curves:"), .spaceSeparatedAndCollapsed(provided), 
      sep = "\n"
    )
    
    stop(messageText)
  }
  
  kwb.utils::catIf(dbg, "Warning!!\n")
  kwb.utils::catIf(dbg, "No", curveType, "curves available for ")
  kwb.utils::catIf(dbg, ifelse(noneDefined, "ANY of the defined ", "the following "))
  kwb.utils::catIf(dbg, .plural(objectName))
  kwb.utils::catIf(dbg, "! Check name ids:\n")
  kwb.utils::catIf(dbg, .firstLetterUpperCase(.plural(objectName)))
  kwb.utils::catIf(dbg, " defined in epanetConfig section", kwb.utils::hsQuoteChr(sectionName))
  kwb.utils::catIf(dbg, ifelse(noneDefined, "", paste("but without a", curveType, "curve")))
  kwb.utils::catIf(dbg, ":\n")
  kwb.utils::catIf(dbg, .spaceSeparatedAndCollapsed(undefined))
  kwb.utils::catIf(dbg, "\n")
  kwb.utils::catIf(dbg, "Available", objectName, "names in input data ")
  kwb.utils::catIf(dbg, kwb.utils::hsQuoteChr(dataType))
  kwb.utils::catIf(dbg, ":\n")
  kwb.utils::catIf(dbg, .spaceSeparatedAndCollapsed(provided))
}

# .firstLetterUpperCase --------------------------------------------------------
.firstLetterUpperCase <- function(x) {
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
}

# .plural ----------------------------------------------------------------------
.plural <- function(x)
{
  paste(x, "s", sep = "")
}

# .deleteCurvesIf --------------------------------------------------------------
.deleteCurvesIf <- function(deleteOldCurves, epanetConfig, dbg = TRUE)
{
  if (deleteOldCurves & !is.null(epanetConfig$CURVES)) {
    kwb.utils::catIf(dbg, "Deleting CURVES section...\n")
    epanetConfig$CURVES <- epanetConfig$CURVES[FALSE, ]
  }
  
  epanetConfig
}

# .spaceSeparatedAndCollapsed --------------------------------------------------
.spaceSeparatedAndCollapsed <- function(x)
{
  paste(x, sep = " ", collapse = " ")
}

# .test ------------------------------------------------------------------------
.test <- function()
{
  epanetConfig <- list(PUMPS = NULL)
  TDH1 <- data.frame(ID = c("p1", "p2"), X_Value = 1:2, Y_Value = 1:2)
  TDH2 <- data.frame(ID = c("p2"), X_Value = 77, Y_Value = 88)
  epanetConfig <- writePumpCurves(epanetConfig, TDH1)
  writePumpCurves(epanetConfig, TDH2, dbg = TRUE)
}
