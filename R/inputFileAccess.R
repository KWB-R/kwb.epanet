# writeEpanetInputFile ---------------------------------------------------------

#' Write Epanet Input File
#' 
#' @param inpdat input data to be saved in EPANET's input file format
#' @param inpfile full path to input file to be created
#' @param dbg if TRUE, debug messages are shown. Default: FALSE  
#' @export
writeEpanetInputFile <- function(inpdat, inpfile, dbg = FALSE)
{
  kwb.utils::catIf(dbg, sprintf("Writing EPANET input file to \"%s\"...\n", inpfile))
  writeLines(epanetInputFileLines(inpdat), inpfile)  
  kwb.utils::catIf(dbg, "End of writing EPANET input file.\n")
}

# epanetInputFileLines ---------------------------------------------------------

#' Epanet Input File Lines
#' 
#' @param inpdat input data to be saved in EPANET's input file format
#' @param dbg if TRUE, debug messages are shown. Default: FALSE
#' @export
epanetInputFileLines <- function(inpdat, dbg = FALSE)
{
  txtlines <- character()
  
  for (section in names(inpdat)) {
    
    kwb.utils::catIf(dbg, sprintf("Writing section [%s]... ", section))
    
    txtline <- sprintf("[%s]", section)
    txtlines <- c(txtlines, txtline)
    
    if (! all(is.na(inpdat[[section]]))) {
      
      txtline <- sprintf(";%s", paste(names(inpdat[[section]]), collapse = "\t"))
      txtlines <- c(txtlines, txtline)
      
      if (section %in% c(
        "JUNCTIONS", "RESERVOIRS", "TANKS", "PIPES", "PUMPS", "VALVES", "DEMANDS"
      )) {
        inpdat[[section]] <- cbind(inpdat[[section]], ";")
      }
      
      txtlines <- c(txtlines, apply(inpdat[[section]], 1, paste, collapse = "\t"))      
    }
    
    txtlines <- c(txtlines, "")
    
    kwb.utils::catIf(dbg, "ok.\n")
  }
  
  txtlines <- c(txtlines, "[END]")
}

# getEfficiencyCurve -----------------------------------------------------------

#' Get Efficiency Curve
#' @keywords internal
#' @noRd
#' @noMd
getEfficiencyCurve <- function(energy, curves, pumpname)
{
  grepl_ignore <- function(p, x) grepl(p, x, ignore.case = TRUE)
  
  selected <- 
    grepl_ignore("Pump", energy$X1) & 
    grepl_ignore("Efficiency", energy$X3) & 
    energy$X2 == pumpname
  
  effCurveName <- energy[selected, 4L]
  
  curves[curves$ID == effCurveName, -1L]
}

# getHeadCurve -----------------------------------------------------------------

#' Get Head Curve
#' @keywords internal
#' @noRd
#' @noMd
getHeadCurve <- function(pump, curves, pumpname)
{
  headCurveName <- pump[pump[, 1L] == pumpname, 4L]
  
  pattern <- "^HEAD\\s+"
  headCurveName <- headCurveName[grep(pattern, headCurveName, ignore.case = TRUE)]
  headCurveName <- sub(pattern, "", headCurveName, ignore.case = TRUE)
  
  curves[curves[, 1L] == headCurveName, -1L]
}

# plotCurves -------------------------------------------------------------------

#' Plot Curves
#' 
#' @param curves data frame containing pump \code{curves}, as returned by
#'   \code{\link{readEpanetInputFile}} in list element \emph{CURVES}
#' @param curveNames names of \code{curves} to be plotted. Default: all names in
#'   column \emph{ID} of \emph{curves}
#' @param \dots additional arguments passed to xyplot
#' @export
plotCurves <- function(curves, curveNames = unique(curves$ID), ...)
{
  curves$X_Value <- as.numeric(curves$X_Value)
  curves$Y_Value <- as.numeric(curves$Y_Value)
  
  trellis.object <- lattice::xyplot(
    Y_Value ~ X_Value | ID, 
    data = curves, 
    subset = curves$ID %in% curveNames,
    type = c("l", "g"), 
    scales = "free", ...
  )
  
  print(trellis.object)
}

# readEpanetInputFile ----------------------------------------------------------

#' Read EPANET Input File
#' 
#' @param inpfile full path to EPANET input file
#' @param dbg if TRUE, debug messages are shown. Default: FALSE    
#' 
#' @return list with elements representing the different sections of the EPANET 
#'   input file. The names of the list elements correspond to the names of
#'   the sections that were found in the input file. Each list element is a 
#'   data frame containing the content of the corresponding section. 
#' 
#' @seealso \code{\link{availableSections}, \link{readEpanetOutputFile}}  
#' @export
readEpanetInputFile <- function(inpfile, dbg = FALSE)
{
  textlines <- readLines(inpfile)
  skip <- c("REACTIONS", "END")  
  
  lapply(setdiff(availableSections(inpfile), skip), function(section) {
    kwb.utils::catIf(dbg, "Section:", section, "\n")
    getSectionFromTextLines(textlines, section)
  })
}

# availableSections ------------------------------------------------------------

#' Available Sections
#' 
#' Names of sections available in EPANET input file
#' 
#' @param inpfile full path to EPANET input file
#' 
#' @return character vector containing the names of the sections contained in
#'   the EPANET input file (without brackets)
#' 
#' @seealso \code{\link{readEpanetInputFile}}
#' @export
availableSections <- function(inpfile)
{
  names(getSectionStarts(readLines(inpfile)))
}

# getSectionStarts -------------------------------------------------------------
getSectionStarts <- function(txtlines)
{
  pattern <- "\\[\\s*(\\S*)\\s*\\]"
  
  sectionStarts <- grep(pattern, txtlines)
  sectionStartLines <- txtlines[sectionStarts]
  sections <- sub(pattern, "\\1", sectionStartLines)
  
  stats::setNames(sectionStarts, sections)
}

# getSection -------------------------------------------------------------------

#' Get Section
#' 
#' Get section from EPANET input file
#' 
#' @param inpfile full path to EPANET input file
#' @param sectionName name of section to be read, for possible section names see
#'   the documentation of the EPANET Toolkit
#' 
#' @return data frame representing the content of the section in the input file. 
#'   If possible, column names are read from the section's header line
#' @export 
getSection <- function(inpfile, sectionName) 
{  
  getSectionFromTextLines(readLines(inpfile), sectionName)
}

# getSectionFromTextLines ------------------------------------------------------
getSectionFromTextLines <- function(txtlines, sectionName)
{
  txtblock <- txtlines[getRowIndicesOfSection(txtlines, sectionName)]
  
  if (all(txtblock == "")) {
    return(NA)
  }
  
  headerPattern <- "^\\s*;"
  headerRow <- grep(headerPattern, txtblock)
  splitPattern <- "\\s*\t\\s*"
  
  if (length(headerRow)) {
    
    header <- trimSpaceAndOptionalSemicolon(txtblock[headerRow])
    captions <- substSpecialCharacters(strsplit(header, split = splitPattern)[[1]])
    body <- txtblock[-headerRow]
    
  } else {
    
    captions <- NULL
    body <- txtblock
  }
  
  body <- trimSpaceAndOptionalSemicolon(body)
  body <- body[body != ""]
  
  if (length(body)) {
    
    fields <- strsplit(body, split = splitPattern)
    numberOfColumns <- max(c(length(captions), sapply(fields, length)))
    fields <- lapply(fields, FUN = fillupToLength, numberOfColumns)
    characterMatrix <- matrix(unlist(fields), ncol = numberOfColumns, byrow = TRUE)
    
    result <- data.frame(characterMatrix, stringsAsFactors = FALSE)
    
  } else {
    
    numberOfColumns <- length(captions)
    
    result <- utils::read.table(
      textConnection(""), 
      col.names = defaultColumnNames(numberOfColumns), 
      colClasses = "character"
    )
  }
  
  if (!is.null(captions)) {
    
    names(result)[seq_along(captions)] <- captions
    
  } else {
    
    names(result) <- defaultColumnNames(numberOfColumns)
  }
  
  result
}

# sectionEnd -------------------------------------------------------------------
sectionEnd <- function(index, sectionStarts, maxRow)
{
  if (index < length(sectionStarts)) {
    
    sectionStarts[index + 1L] - 1L
    
  } else {
    
    maxRow
  }
}

# defaultColumnNames -----------------------------------------------------------
defaultColumnNames <- function(numberOfColumns)
{
  paste0("X", seq_len(numberOfColumns))
}

# substSpecialCharacters -------------------------------------------------------
substSpecialCharacters <- function(x) 
{
  gsub("\\-", "_", x)
}

# fillupToLength ---------------------------------------------------------------
fillupToLength <- function(x, targetLength, fillupWith = "") 
{
  len <- length(x)
  
  if (len < targetLength) {
    
    c(x, rep(fillupWith, times = targetLength - len))
    
  } else {
    
    x
  }
}

# trimSpaceAndOptionalSemicolon ------------------------------------------------
trimSpaceAndOptionalSemicolon <- function(x)
{
  x <- sub("^\\s*;?\\s*", "", x)
  x <- sub("\\s*;?\\s*$", "", x)
  x
}

# exampleInputFiles ------------------------------------------------------------

#' Example Input Files
#' 
#' @return full path(s) to EPANET example input file(s)
#' @export
exampleInputFiles <- function() 
{
  dir(extdata_file(), "\\.inp$", full.names = TRUE)
}

#' Get Path to File in This Package
#' 
#' @inheritParams kwb.utils::extdataFile
extdata_file <- kwb.utils::createFunctionExtdataFile("kwb.epanet")

# writeInputFileWithNewCurveSection --------------------------------------------

#' Write Input File With New Curve Section
#' 
#' @param inpfile full path to EPANET input file
#' @param newCurves modified CURVES section. Data frame as returned by 
#'   \code{\link{readEpanetInputFile}} in list element \emph{curves}
#' @param inpfile.new full path to modified EPANET input file. (default: \emph{\<inpfile-without-extension_new\>.inp})
#' @return full path to created input file
#' @export
writeInputFileWithNewCurveSection <- function(
  inpfile, 
  newCurves, 
  inpfile.new = sub("(\\.[^\\.]+)$", "_new\\1", inpfile)
)
{
  kwb.utils::catAndRun(
    sprintf("Writing modified EPANET input file to %s... ", inpfile.new),
    writeLines(replaceCurveSectionInInputFile(inpfile, newCurves), inpfile.new)
  )

  inpfile.new
}

# replaceCurveSectionInInputFile -----------------------------------------------

#' Replace Curve Section In Input File
#' 
#' @param inpfile full path to EPANET input file
#' @param newCurves modified CURVES section. Data frame as returned by 
#'   \code{\link{readEpanetInputFile}} in list element \emph{curves}
#' 
#' @return vector of character(s) representing the rows of the input file
#' @export
replaceCurveSectionInInputFile <- function(inpfile, newCurves)
{
  txtlines <- readLines(inpfile)
  
  curveRows <- getRowIndicesOfSection(txtlines, "CURVES")
  
  before <- txtlines[seq_len(min(curveRows))] 
  after <- txtlines[seq(max(curveRows) + 1L, length(txtlines))]
  
  c(before, curvesToText(newCurves), after)
}

# getRowIndicesOfSection -------------------------------------------------------
getRowIndicesOfSection <- function(txtlines, sectionName)
{
  sectionStarts <- getSectionStarts(txtlines)
  index <- which(names(sectionStarts) == sectionName)
  
  if (length(index) < 1L) {
    stop("No such section: ", sectionName)
  }
  
  if (length(index) > 1L) {
    stop("Multiple section: ", sectionName)
  }
  
  firstRow <- sectionStarts[index] + 1L
  lastRow <- sectionEnd(index, sectionStarts, length(txtlines))
  
  seq(firstRow, length.out = (lastRow - firstRow + 1L))
}

# curvesToText -----------------------------------------------------------------

#' Curves To Text
#' 
#' @param curves data frame representing \code{curves}, with columns \emph{ID},
#'   \emph{X_Value}, \emph{Y_Value}, as returned by
#'   \code{\link{readEpanetInputFile}} in list element \emph{CURVES}
#' @export
curvesToText <- function(curves)
{
  curves$ID <- sprintf(" %16-s", curves$ID)
  
  fmt <- "%12.12-f"
  curves$X_Value <- sprintf(fmt, as.numeric(curves$X_Value))
  curves$Y_Value <- sprintf(fmt, as.numeric(curves$Y_Value))
  
  fmt <- "%12-s"
  curves$X_Value <- sprintf(fmt, removeZeroesAtTheEnd(curves$X_Value))
  curves$Y_Value <- sprintf(fmt, removeZeroesAtTheEnd(curves$Y_Value))
  
  paste(curves$ID, curves$X_Value, curves$Y_Value, sep = "\t")  
}

# replaceCurves ----------------------------------------------------------------

#' Replace Curves In CURVES Data
#' 
#' Replace \code{curves} in CURVES data 
#' 
#' @param curves data frame representing \code{curves}, with columns \emph{ID},
#'   \emph{X_Value}, \emph{Y_Value}, as returned by
#'   \code{\link{readEpanetInputFile}} in list element \emph{CURVES}
#' @param newCurves new curves data frame representing \code{curves}, with columns \emph{ID},
#'   \emph{X_Value}, \emph{Y_Value},
#' @export
replaceCurves <- function(curves, newCurves)
{
  for (curveName in trim(unique(newCurves$ID))) {
    newCurve <- newCurves[trim(newCurves$ID) == curveName, ]
    curves <- replaceOneCurve(
      curves, curveName, newCurve$X_Value, newCurve$Y_Value
    )
  }
  
  curves
}

# replaceOneCurve --------------------------------------------------------------

#' Replace One Curve In CURVES Data
#' 
#' @param curves data frame representing \code{curves}, with columns \emph{ID},
#'   \emph{X_Value}, \emph{Y_Value}, as returned by
#'   \code{\link{readEpanetInputFile}} in list element \emph{CURVES}
#' @param curveName name of the curve to be replaced
#' @param x vector of \code{x} values of the curve
#' @param y vector of \code{y} values of the curve
#' 
#' @return data frame in which the lines corresponding to the curve named
#'   \emph{curveName} are replaced with \code{x} and \code{y} values given in
#'   \emph{x} and \emph{y}, respectively
#' @export
replaceOneCurve <- function(curves, curveName, x, y)
{
  selection <- which(trim(curves$ID) == curveName)
  
  before <- seq(1L, length.out = min(selection) - 1L)
  last <- max(selection)
  after <- seq(last + 1L, length.out = nrow(curves) - last)
  
  rbind(
    curves[before, ],
    data.frame(ID = curveName, X_Value = x, Y_Value = y),
    curves[after, ]
  )
}
