# .removeZeroesAtTheEnd --------------------------------------------------------
.removeZeroesAtTheEnd <- function(x)
{
  gsub("\\.?0+\\s*$", "", x)
}

# .trim ------------------------------------------------------------------------
.trim <- function(x)
{
  gsub("^\\s+", "", gsub("\\s+$", "", x))
}

# plotModel --------------------------------------------------------------------

#' Plot Model
#' 
#' plot EPANET model network. Allows plotting of changed pipe ids, which are
#'   defined as vector in parameter "changedPipeIDs"
#' 
plotModel <- function(
  inpdat, 
  pch = 16, 
  cex = 0.2, 
  xlab = "", 
  ylab = "", 
  main = .defaultMain(inpdat, 1), 
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

# .defaultMain -----------------------------------------------------------------
.defaultMain <- function(inpdat, titleLines = 1:3)
{
  paste(inpdat$TITLE[titleLines, ], collapse = "\n")
}
