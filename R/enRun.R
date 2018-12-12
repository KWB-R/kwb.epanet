# setEpanetInstallationPath ----------------------------------------------------
setEpanetInstallationPath <- function # setEpanetInstallationPath
### setEpanetInstallationPath
(
  epanet.dir
  ### full path to MS Access database or ODBC database name
)  
{
  if (! is.null(epanet.dir) && ! file.exists (epanet.dir)) {
    stop("The folder ", hsQuoteChr(epanet.dir), "does not exist.")
  }
  
  options(kwb.db.epanet.dir = epanet.dir)    
}

# getEpanetInstallationPath ----------------------------------------------------
getEpanetInstallationPath <- function # getEpanetInstallationPath
### getEpanetInstallationPath
() 
{
  epanet.dir <- getOption("kwb.db.epanet.dir")
  
  if (! is.null(epanet.dir) && ! file.exists (epanet.dir)) {
    stop("The folder ", hsQuoteChr(epanet.dir), "does not exist.")
    options(kwb.db.epanet.dir = NULL)
  }
  
  if (is.null(epanet.dir)) {
    
    defaultDirectories <- file.path(defaultWindowsProgramFolders(), "EPANET2")
    
    existing <- which(file.exists(defaultDirectories))
    
    if (length(existing) > 0) {
      epanet.dir <- defaultDirectories[existing[1]]
    }
    else {
      stop(
        "I did not find the EPANET installation path. I was looking for:\n  ",
        paste(hsQuoteChr(defaultDirectories), collapse="\n  "), "\n",
        "Please use 'setEpanetInstallationPath' to specify the path to an ",
        "existing EPANET installation folder.")
    }
  }  
  
  setEpanetInstallationPath(epanet.dir)
  
  return (epanet.dir)
}
# runEpanetGUI -----------------------------------------------------------------
runEpanetGUI <- function # runEpanetGUI
### runEpanetGUI
(
  inpfile = "",
  epanet.dir = getEpanetInstallationPath()
)
{
  epanet.exe <- .stopIfEpanetExeDoesNotExist(epanet.dir, "epanet2w.exe")  
  hsSystem(sprintf("%s %s", shQuote(epanet.exe), shQuote(windowsPath(inpfile))),
           wait=FALSE)
}

# .stopIfEpanetExeDoesNotExist -------------------------------------------------
.stopIfEpanetExeDoesNotExist <- function(epanet.dir, epanet.exe)
{
  epanet.exe <- file.path(epanet.dir, epanet.exe)  
  if (!file.exists(epanet.exe)) 
    stop("No such file: ", epanet.exe, 
         "\n  Please use the argument 'epanet.dir' to set the EPANET installation path")
  
  epanet.exe
}

# runEpanetConfiguration -------------------------------------------------------
runEpanetConfiguration <- function # Run EPANET INP configuration
### Run EPANET INP configuration
(
  inpdat,
  ### input data as retrieved by \code{\link{readEpanetInputFile}}
  name = "tmpEpanet",
  ### name of input file to be generated in tempdir()
  returnOutput = write.output,
  ### if TRUE, the output is read from the output file (if generated, see
  ### \emph{write.output}) and returned. Default: value of \emph{write.output}
  write.output = TRUE,
  ### if TRUE, EPANET will write a binary output file, else not  
  ...,
  ### further arguments passed to \code{\link{runEpanet}} and finally to 
  ### \code{\link{readEpanetOutputFile}}, such as: \emph{read.prolog}, 
  ### \emph{read.energyUse}, \emph{read.dynamicResults}, \emph{read.epilog}, see
  ### there.
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE  
)
{
  inpfile <- file.path(tempdir(), paste(name, "inp", sep = "."))
  
  writeEpanetInputFile(inpdat, inpfile)

  runEpanet(
    inpfile, 
    returnOutput = returnOutput, 
    write.output = write.output, 
    ..., 
    dbg = dbg
  )
}

# runEpanet --------------------------------------------------------------------
runEpanet <- function # run EPANET with given input file
### run EPANET with given input file
(
  inpfile, 
  ### full path to EPANET input file
  returnOutput = FALSE,
  ### if TRUE, the output is read from the generated output file and returned
  epanet.dir = getEpanetInstallationPath(),
  ### path to EPANET installation directory. 
  ### Default: getEpanetInstallationPath()
  intern = FALSE,
  ### a logical, indicates whether to make the output of the command an R object.
  write.output = TRUE,
  ### if TRUE, EPANET will write a binary output file, else not
  ...,
  ### further arguments passed to \code{\link{readEpanetOutputFile}}, such as:
  ### \emph{read.prolog}, \emph{read.energyUse}, \emph{read.dynamicResults},
  ### \emph{read.epilog}, see there.
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE    
)
{
  epanet.exe <- .stopIfEpanetExeDoesNotExist(epanet.dir, "epanet2d.exe")
  
  tdir <- createDirAndReturnPath(file.path(tempdir(), "epanet"), dbg = dbg)
  
  target.file <- file.path(tdir, basename(inpfile))
  
  # give warning (but overwrite anyway!) if file already exists
  if (file.exists(target.file)) {
    catIf(
      dbg, 
      sprintf("*** There is already a file named \"%s\"", target.file),
      "in the target folder. It will be overwritten.\n")
  }
  
  if (!file.copy(inpfile, tdir, overwrite=TRUE)) {
    stop(sprintf("Could not copy \"%s\" to \"%s\"!\n", inpfile, tdir))
  }
  
  filenames <- kwb.utils::runInDirectory(
    target.dir = tdir, 
    FUN = runEpanetOnCommandLine, 
    inpfile = basename(inpfile), # argument of runEpanetOnCommandLine
    epanet.exe = epanet.exe,     # argument of runEpanetOnCommandLine
    intern = intern,             # argument of runEpanetOnCommandLine
    write.output = write.output,
    dbg = dbg,
    .dbg = dbg
  )
  
  result <- list(files=file.path(tdir, filenames))
  
  checkReportFileForErrors(file.path(tdir, filenames$report))
  
  if (returnOutput) {
    cat("\n*** Reading output file... ")
    
    outputFile <- file.path(tdir, filenames$output)
    
    if (filenames$output != "" && file.exists(outputFile)) {
      result$output <- readEpanetOutputFile(outputFile, ...)  
    }
    else {
      if (outputFile == "") {
        warning("No output file was created.")  
      }
      else {
        warning("No such output file: ", outputFile)
      }
      result$output <- NULL
    }
    cat("ok.\n")
  }
  
  result
}

# checkReportFileForErrors -----------------------------------------------------
checkReportFileForErrors <- function # checkReportFileForErrors
### checkReportFileForErrors
(
  reportFile
  ### full path to report file
)
{
  fileLines <- readLines(reportFile, warn = FALSE)
  errorLines <- grep("Input Error \\d+\\:",  fileLines, value = TRUE)
  
  if (length(errorLines) > 0) {
    file.show(reportFile)
    stop("There have been errors:\n", paste(errorLines, collapse = "\n"))
  }    
}

# runEpanetOnCommandLine -------------------------------------------------------
runEpanetOnCommandLine <- function # runEpanetOnCommandLine
### runEpanetOnCommandLine
(
  inpfile,
  epanet.exe,
  intern = FALSE,
  write.output = TRUE,
  ### if TRUE, EPANET will write a binary output file, else not
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE  
)
{  
  rptfile <- replaceFileExtension(inpfile, "rpt")
  
  if (write.output) {
    outfile <- replaceFileExtension(inpfile, "out")    
  }
  else {
    outfile <- ""
  }
  
  commandLine <- .epanetCommandLine(epanet.exe, inpfile, rptfile, outfile)
  
  if (dbg) {
    # hsShell echoes the command line onto the screen
    output <- hsShell(commandLine = commandLine, intern = intern)
  }
  else {
    output <- shell(commandLine, intern = intern)
  }
  
  list(input = inpfile, 
       output = outfile, 
       report = rptfile)
}  

# .epanetCommandLine -----------------------------------------------------------
.epanetCommandLine <- function
(
  epanet.exe, inpfile, rptfile, outfile
)
{
  if (outfile != "") {
    outfile <- shQuote(outfile)    
  }
  
  sprintf("\"%s %s %s %s\"", shQuote(epanet.exe), 
          shQuote(inpfile), shQuote(rptfile), outfile)
}

# replaceFileExtension ---------------------------------------------------------
replaceFileExtension <- function # replaceFileExtension
### replaceFileExtension
(
  filename, 
  ### full path to file of which the extension is to be changed
  newExtension
  ### new extension to be given to the file, without dot "."
)
{
  gsub("\\.[^\\.]+$", paste(".", newExtension, sep=""), filename)
}
