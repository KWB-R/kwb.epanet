# writeEpanetInputFile ---------------------------------------------------------
writeEpanetInputFile <- function # writeEpanetInputFile
### writeEpanetInputFile
(
  inpdat,
  ### input data to be saved in EPANET's input file format
  inpfile,
  ### full path to input file to be created
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE  
)
{
  catIf(dbg, sprintf("Writing EPANET input file to \"%s\"...\n", inpfile))
  writeLines(epanetInputFileLines(inpdat), inpfile)  
  catIf(dbg, "End of writing EPANET input file.\n")
}

# epanetInputFileLines ---------------------------------------------------------
epanetInputFileLines <- function # epanetInputFileLines
### epanetInputFileLines
(
  inpdat,
  ### input data to be saved in EPANET's input file format
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE
)
{
  txtlines <- character()
  for (section in names(inpdat)) {
    
    catIf(dbg, sprintf("Writing section [%s]... ", section))
    
    txtline <- sprintf("[%s]", section)
    txtlines <- c(txtlines, txtline)
    
    if (!all(is.na(inpdat[[section]]))) {
      txtline <- sprintf(";%s", paste(names(inpdat[[section]]), collapse="\t"))
      txtlines <- c(txtlines, txtline)
      
      if (section %in% c("JUNCTIONS", "RESERVOIRS", "TANKS", "PIPES",
                         "PUMPS", "VALVES", "DEMANDS")) {
        inpdat[[section]] <- cbind(inpdat[[section]], ";")
      }
      
      txtlines <- c(txtlines, apply(inpdat[[section]], 1, paste, collapse="\t"))      
    }
    txtlines <- c(txtlines, "")
    
    catIf(dbg, "ok.\n")
  }
  
  txtlines <- c(txtlines, "[END]")
}

# getEfficiencyCurve -----------------------------------------------------------
getEfficiencyCurve <- function # getEfficiencyCurve
### getEfficiencyCurve
(
  energy, 
  curves, 
  pumpname
)
{
  effCurveName <- energy[grepl("Pump", energy$X1, ignore.case=TRUE) 
                         & grepl("Efficiency", energy$X3, ignore.case=TRUE)
                         & energy$X2 == pumpname, 4]
  curves[curves$ID == effCurveName, -1]
}

# getHeadCurve -----------------------------------------------------------
getHeadCurve <- function # getHeadCurve
### getHeadCurve
(
  pump, 
  curves, 
  pumpname
)
{
  headCurveName <- pump[pump[, 1] == pumpname, 4]
  
  pattern <- "^HEAD\\s+"
  headCurveName <- headCurveName[grep(pattern, headCurveName, ignore.case=TRUE)]
  headCurveName <- sub(pattern, "", headCurveName, ignore.case=TRUE)
  
  curves[curves[, 1] == headCurveName, -1]
}

# plotCurves -------------------------------------------------------------------
plotCurves <- function # plotCurves
### plotCurves
(
  curves,
  ### data frame containing pump curves, as returned by 
  ### \code{\link{readEpanetInputFile}} in list element \emph{CURVES}
  curveNames = unique(curves$ID),
  ### names of curves to be plotted. Default: all names in column \emph{ID}
  ### of \emph{curves}
  ...
  ### additional arguments passed to xyplot
)
{
  curves$X_Value <- as.numeric(curves$X_Value)
  curves$Y_Value <- as.numeric(curves$Y_Value)
  trellis.object <- xyplot(Y_Value ~ X_Value | ID, 
                           data = curves, 
                           subset = curves$ID %in% curveNames,
                           type=c("l", "g"), 
                           scales="free", ...)  
  print(trellis.object)
}

# readEpanetInputFile ----------------------------------------------------------
readEpanetInputFile <- function # readEpanetInputFile
### read EPANET input file
(
  inpfile,
  ### full path to EPANET input file
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE    
)
{
  ##seealso<< \code{\link{availableSections}, \link{readEpanetOutputFile}}  
  
  textlines <- readLines(inpfile)
  skip <- c("REACTIONS", "END")  
  
  result <- list()

  for (section in setdiff(availableSections(inpfile), skip)) {
    catIf(dbg, "Section:", section, "\n")
    result[[section]] <- .getSectionFromTextLines(textlines, section)      
  }
  
  result
  ### list with elements representing the different sections of the EPANET 
  ### input file. The names of the list elements correspond to the names of
  ### the sections that were found in the input file. Each list element is a 
  ### data frame containing the content of the corresponding section. 
}

# availableSections ------------------------------------------------------------
availableSections <- function # availableSections
### names of sections available in EPANET input file
(
  inpfile
  ### full path to EPANET input file
)
{
  ##seealso<< \code{\link{readEpanetInputFile}}
  
  txtlines <- readLines(inpfile)  
  names(.getSectionStarts(txtlines))
  
  ### character vector containing the names of the sections contained in the
  ### EPANET input file (without brackets)
}

# .getSectionStarts -------------------------------------------------------------
.getSectionStarts <- function # .getSectionStarts
### .getSectionStarts
(
  txtlines
) 
{
  pattern <- "\\[\\s*(\\S*)\\s*\\]"
  sectionStarts <- grep(pattern, txtlines)
  sectionStartLines <- txtlines[sectionStarts]
  sections <- sub(pattern, "\\1", sectionStartLines)
  names(sectionStarts) <- sections
  sectionStarts
}

# getSection -------------------------------------------------------------------
getSection <- function # getSection
### get section from EPANET input file
(
  inpfile, 
  ### full path to EPANET input file
  sectionName
  ### name of section to be read, for possible section names see the 
  ### documentation of the EPANET Toolkit
) 
{  
  txtlines <- readLines(inpfile)
  .getSectionFromTextLines(txtlines, sectionName)
  
  ### data frame representing the content of the section in the input file. 
  ### If possible, column names are read from the section's header line
}

# .getSectionFromTextLines ------------------------------------------------------
.getSectionFromTextLines <- function # .getSectionFromTextLines
### .getSectionFromTextLines
(
  txtlines, 
  sectionName
) 
{  
  txtblock <- txtlines[.getRowIndicesOfSection(txtlines, sectionName)]
  
  if (all(txtblock == "")) {
    return(NA)
  }
  
  headerPattern <- "^\\s*;"
  headerRow <- grep(headerPattern, txtblock)
  splitPattern <- "\\s*\t\\s*"
  
  if (length(headerRow) > 0) {
    header <- .trimSpaceAndOptionalSemicolon(txtblock[headerRow])    
    captions <- .substSpecialCharacters(strsplit(header, split=splitPattern)[[1]])
    body <- txtblock[-headerRow]
  } else {
    captions <- NULL
    body <- txtblock
  }
  
  body <- .trimSpaceAndOptionalSemicolon(body)
  body <- body[body != ""]  
  
  if (length(body) > 0) {
    
    fields <- strsplit(body, split=splitPattern)
    numberOfColumns <- max(c(length(captions), sapply(fields, length)))
    
    fields <- lapply(fields, FUN=.fillupToLength, numberOfColumns)
    characterMatrix <- matrix(unlist(fields), ncol=numberOfColumns, byrow=TRUE)
    result <- data.frame(characterMatrix, stringsAsFactors=FALSE)    
  } else {
    numberOfColumns <- length(captions)
    result <- read.table(textConnection(""), 
                         col.names = .defaultColumnNames(numberOfColumns),
                         colClasses = "character")
  }
  
  if (!is.null(captions)) {
    names(result)[1:length(captions)] <- captions
  } else {
    names(result) <- .defaultColumnNames(numberOfColumns)
  }
  
  result
}

# .sectionEnd ------------------------------------------------------------------
.sectionEnd <- function(index, sectionStarts, maxRow)
{
  if (index < length(sectionStarts)) {
    lastRow <- sectionStarts[index+1] - 1
  } else {
    lastRow <- maxRow
  }
  lastRow
}

# .defaultColumnNames -----------------------------------------------------------
.defaultColumnNames <- function # .defaultColumnNames
### .defaultColumnNames
(
  numberOfColumns 
) 
{
  paste("X", 1:numberOfColumns, sep="")
}

# .substSpecialCharacters -------------------------------------------------------
.substSpecialCharacters <- function # .substSpecialCharacters
### susbstitute minus "-" with underscore "_"
(
  x
)
{
  gsub("\\-", "_", x)
}

# .fillupToLength ---------------------------------------------------------------
.fillupToLength <- function # .fillupToLength
### .fillupToLength
(
  x, 
  targetLength, 
  fillupWith = ""
)
{
  len <- length(x)
  if (len < targetLength) {
    c(x, rep(fillupWith, times=targetLength-length(x)))    
  }
  else {
    x
  }
}

# .trimSpaceAndOptionalSemicolon ------------------------------------------------
.trimSpaceAndOptionalSemicolon <- function # .trimSpaceAndOptionalSemicolon
### .trimSpaceAndOptionalSemicolon
(
  x
)
{
  x <- sub("^\\s*;?\\s*", "", x)
  sub("\\s*;?\\s*$", "", x)
}

# exampleInputFiles ------------------------------------------------------------
exampleInputFiles <- function # example input files
###  example input files
() 
{
  dir(system.file("extdata", package = "kwb.epanet"), "\\.inp$", full.names=TRUE)
  ### full path(s) to EPANET example input file(s)
}


# writeInputFileWithNewCurveSection --------------------------------------------
writeInputFileWithNewCurveSection <- function # writeInputFileWithNewCurveSection
### writeInputFileWithNewCurveSection
(
  inpfile, 
  ### full path to EPANET input file
  newCurves, 
  ### modified CURVES section. Data frame as returned by 
  ### \code{\link{readEpanetInputFile}} in list element \emph{curves}
  inpfile.new = sub("(\\.[^\\.]+)$", "_new\\1", inpfile)
  ### full path to modified EPANET input file. 
  ### Default: \emph{<inpfile-without-extension>_new.inp}
)
{
  cat(sprintf("Writing modified EPANET input file to %s... ", inpfile.new))
  writeLines(replaceCurveSectionInInputFile(inpfile, newCurves), inpfile.new)
  cat("ok.\n")
  
  inpfile.new
  ### full path to created input file
}

# replaceCurveSectionInInputFile -----------------------------------------------
replaceCurveSectionInInputFile <- function # replaceCurveSectionInInputFile
### replaceCurveSectionInInputFile
(
  inpfile, 
  ### full path to EPANET input file
  newCurves
  ### modified CURVES section. Data frame as returned by 
  ### \code{\link{readEpanetInputFile}} in list element \emph{curves}
)
{
  txtlines <- readLines(inpfile)
  
  curveRows <- .getRowIndicesOfSection(txtlines, "CURVES")
  
  before <- txtlines[1:min(curveRows)] 
  after <- txtlines[(max(curveRows)+1):length(txtlines)]
  
  c(before, curvesToText(newCurves), after)
  ### vector of character(s) representing the rows of the input file
}

# .getRowIndicesOfSection -------------------------------------------------------
.getRowIndicesOfSection <- function # .getRowIndicesOfSection
### .getRowIndicesOfSection
(
  txtlines, sectionName)
{
  sectionStarts <- .getSectionStarts(txtlines)
  
  index <- which(names(sectionStarts) == sectionName)
  
  if (length(index) < 1) {
    stop("No such section: ", sectionName)  
  }
  
  if(length(index) > 1) {
    stop("Multiple section: ", sectionName)      
  }
  
  firstRow <- sectionStarts[index] + 1
  lastRow <- .sectionEnd(index, sectionStarts, length(txtlines))  
  
  seq(firstRow, length.out=(lastRow-firstRow+1))
}

# curvesToText -----------------------------------------------------------------
curvesToText <- function # curvesToText
### curvesToText
(
  curves
  ### data frame representing curves, with columns \emph{ID}, \emph{X_Value}, 
  ### \emph{Y_Value}, as returned by \code{\link{readEpanetInputFile}} in 
  ### list element \emph{CURVES}
)
{
  curves$ID <- sprintf(" %16-s", curves$ID)
  curves$X_Value <- sprintf("%12.12-f", as.numeric(curves$X_Value))
  curves$Y_Value <- sprintf("%12.12-f", as.numeric(curves$Y_Value))
  
  curves$X_Value <- sprintf("%12-s", .removeZeroesAtTheEnd(curves$X_Value))
  curves$Y_Value <- sprintf("%12-s", .removeZeroesAtTheEnd(curves$Y_Value))
  
  paste(curves$ID, curves$X_Value, curves$Y_Value, sep="\t")  
}

# replaceCurves ----------------------------------------------------------------
replaceCurves <- function # replace curves in CURVES data
### replace curves in CURVES data 
(
  curves, 
  ### data frame representing curves, with columns \emph{ID}, \emph{X_Value}, 
  ### \emph{Y_Value}, as returned by \code{\link{readEpanetInputFile}} in 
  ### list element \emph{CURVES}
  newCurves
)
{
  for (curveName in .trim(unique(newCurves$ID))) {
    newCurve <- newCurves[.trim(newCurves$ID) == curveName, ]
    curves <- replaceOneCurve(curves, curveName, newCurve$X_Value, 
                              newCurve$Y_Value)
  }
  
  curves
}

# replaceOneCurve -----------------------------------------------------------------
replaceOneCurve <- function # replace one curve in CURVES data
### replace one curve in CURVES data 
(
  curves, 
  ### data frame representing curves, with columns \emph{ID}, \emph{X_Value}, 
  ### \emph{Y_Value}, as returned by \code{\link{readEpanetInputFile}} in 
  ### list element \emph{CURVES}
  curveName, 
  ### name of the curve to be replaced
  x, 
  ### vector of x values of the curve
  y
  ### vector of y values of the curve
)
{
  selection <- which(.trim(curves$ID) == curveName)
  
  before <- seq(1, length.out=min(selection)-1)
  last <- max(selection)
  after <- seq(last+1, length.out=nrow(curves)-last)
  
  rbind(curves[before, ],
        data.frame(ID=curveName, X_Value=x, Y_Value=y),
        curves[after, ])
  
  ### data frame in which the lines corresponding to the curve named 
  ### \emph{curveName} are replaced with x and y values given in \emph{x} and
  ### \emph{y}, respectively
}
