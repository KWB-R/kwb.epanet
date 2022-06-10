# removeZeroesAtTheEnd ---------------------------------------------------------
removeZeroesAtTheEnd <- function(x)
{
  gsub("\\.?0+\\s*$", "", x)
}

# trim -------------------------------------------------------------------------
trim <- function(x)
{
  gsub("^\\s+", "", gsub("\\s+$", "", x))
}

# plotModel --------------------------------------------------------------------

#' Plot Model
#' 
#' plot EPANET model network. Allows plotting of changed pipe ids, which are
#'   defined as vector in parameter "changedPipeIDs"
#' @param inpdat EPANET input file
#' @param pch Either an integer specifying a symbol or a single character to be 
#' used as the default in plotting points. See \link[graphics]{points} for possible 
#' values and their interpretation (default: 16)
#' @param cex A numerical value giving the amount by which plotting text and symbols 
#' should be magnified relative to the default. This starts as 1 when a device is 
#' opened, and is reset when the layout is changed, e.g. by setting mfrow. 
#' (default: 0.2)
#' @param xlab a title for the x axis (default: "") 
#' @param ylab a title for the y axis (default: "") 
#' @param main an overall title for the plot (default: kwb.epanet:::defaultMain(inpdat, 1))
#' @param zoomToPumps should plot zoomed to pumps (default: FALSE)
#' @param changedPipeIDs optional, vector with changed pipe IDs (default: NULL)
#' @param ... additional arguments passed to \link[graphics]{plot}
#' @export
plotModel <- function(
  inpdat, 
  pch = 16, 
  cex = 0.2, 
  xlab = "", 
  ylab = "", 
  main = defaultMain(inpdat, 1), 
  zoomToPumps = FALSE, 
  changedPipeIDs = NULL,
  ...
)
{
  pumpinfo <- getPumpInfo(inpdat)
  
  if (zoomToPumps) {
    
    xlim <- range(pumpinfo$Node1.x)
    ylim <- range(pumpinfo$Node1.y)
    main <- paste(main, "(zoomed to pumps)")
    
  } else {
    
    xlim <- NULL
    ylim <- NULL
  }
  
  graphics::plot(
    inpdat$COORDINATES$X_Coord, inpdat$COORDINATES$Y_Coord, 
    xlim = xlim, ylim = ylim, asp = 1, pch = pch, cex = cex, xlab = xlab, 
    ylab = ylab, main = main, ...
  )
  
  graphics::points(pumpinfo$Node1.x, pumpinfo$Node1.y, col = "red")
  graphics::text(pumpinfo$Node1.x, pumpinfo$Node1.y, labels = rownames(pumpinfo))
  
  pipeXY <- getPipeCoordinates(inpdat)
  graphics::segments(pipeXY$Node1.x, pipeXY$Node1.y, pipeXY$Node2.x, pipeXY$Node2.y)
  
  if (!is.null(changedPipeIDs))
  {
    changedPipes <- inpdat
    changedPipes$PIPES <- inpdat$PIPES[which(inpdat$PIPES$ID %in%  changedPipeIDs),]
    changedNodes <- which(inpdat$COORDINATES$Node %in% c(changedPipes$PIPES$Node1, changedPipes$PIPES$Node2))
    changedPipes$COORDINATES <- changedPipes$COORDINATES[changedNodes,]
    
    graphics::points(
      changedPipes$COORDINATES$X_Coord, 
      changedPipes$COORDINATES$Y_Coord, 
      col = "blue", 
      pch = 16
    )
    
    pipeXY <- getPipeCoordinates(changedPipes)
    
    graphics::segments(
      pipeXY$Node1.x, 
      pipeXY$Node1.y, 
      pipeXY$Node2.x, 
      pipeXY$Node2.y, 
      col = "blue"
    )
  }  
}

# defaultMain ------------------------------------------------------------------
defaultMain <- function(inpdat, titleLines = 1:3)
{
  paste(inpdat$TITLE[titleLines, ], collapse = "\n")
}
