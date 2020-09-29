# getPipeCoordinates -----------------------------------------------------------

#' Get Pipe Coordinates
#' 
#' @export
getPipeCoordinates <- function(inpdat)
{
  x <- inpdat$PIPES[, c("ID", "Node1", "Node2")]
  x <- merge(x, inpdat$COORDINATES, by.x = "Node1", by.y = "Node")
  x <- kwb.utils::renameColumns(x, list(X_Coord = "Node1.x", Y_Coord = "Node1.y"))
  x <- merge(x, inpdat$COORDINATES, by.x = "Node2", by.y = "Node")
  x <- kwb.utils::renameColumns(x, list(X_Coord = "Node2.x", Y_Coord = "Node2.y"))
  
  rownames(x) <- x$ID
  
  numericColumns <- c("Node1.x", "Node1.y", "Node2.x", "Node2.y")

  for (col in numericColumns) {
    x[[col]] <- as.numeric(x[[col]])
  }
    
  x[, c("Node1", "Node2", numericColumns)]
}

# getPumpInfo ------------------------------------------------------------------

#' Get Pump Info
#' 
#' @export
getPumpInfo <- function(inpdat)
{
  pumpinfo <- merge(inpdat$PUMPS, inpdat$COORDINATES, by.x = "Node1", by.y = "Node")
  pumpinfo <- kwb.utils::renameColumns(pumpinfo, list(X_Coord = "Node1.x", Y_Coord = "Node1.y"))
  rownames(pumpinfo) <- pumpinfo$ID

  pumpinfo$Node1.x <- as.numeric(pumpinfo$Node1.x)
  pumpinfo$Node1.y <- as.numeric(pumpinfo$Node1.y)
  
  pumpinfo[, c("Node1", "Node2", "Node1.x", "Node1.y", "Parameters")]
}

# getNamesOfObjects ------------------------------------------------------------

#' Get Names Of Objects
#' 
#' @export
getNamesOfObjects <- function(inpdat, section, pattern = ".*")
{
  grep(pattern, unique(inpdat[[section]]$ID), value=TRUE)
}

# getNamesOfCurves -------------------------------------------------------------

#' Get Names Of Curves
#' 
#' @export
getNamesOfCurves <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "CURVES", pattern)  
}

# getNamesOfJunctions ----------------------------------------------------------

#' Get Names Of Junctions
#' 
#' @export
getNamesOfJunctions <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "JUNCTIONS", pattern)  
}

# getNamesOfReservoirs ---------------------------------------------------------

#' Get Names Of Reservoirs
#' 
#' @export
getNamesOfReservoirs <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "RESERVOIRS", pattern)  
}

# getNamesOfTanks --------------------------------------------------------------

#' Get Names Of Tanks
#'
#' @export 
getNamesOfTanks <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "TANKS", pattern)  
}

# getNamesOfPipes --------------------------------------------------------------

#' Get Names Of Pipes
#' 
#' @export
getNamesOfPipes <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "PIPES", pattern)  
}

# getNamesOfPumps --------------------------------------------------------------

#' Get Names Of Pumps
#' 
#' @export

getNamesOfPumps <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "PUMPS", pattern)  
}

# getNamesOfValves -------------------------------------------------------------

#' Get Names Of Valves
#' 
#' @export
getNamesOfValves <- function(inpdat, pattern = ".*")
{
  getNamesOfObjects(inpdat, "VALVES", pattern)  
}
