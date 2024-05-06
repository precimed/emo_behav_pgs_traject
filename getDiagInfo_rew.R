getDiagInfo = function(listID2445, listDiagCodes, wildCard = FALSE, 
                       codeType = c("all", "major"), 
                       pathNPR, yearOfBirth)
{
  
  # Preliminaries -----------------------------------------------------------
  # Get the data.table library for faster file reading with additional functions
  require(data.table)
  
  # Defining the directory where parsed NPR csv files are saved
  if(missing(pathNPR))
  {
    pathNPR <- "Y:/parekh/2023-08-14_parseNPR"
  }
  
  # Fixing the delimiter here
  delimiter  <- ","
  
  # All possible years from NPR: 2008 to 2022
  allTimes   <- 2008:2022
  
  # Fixed column names that are always to be read
  fixedNames <- c("ID_2445", "Role","PREG_ID_2445", "ChildNumber", "WithdrawnConsent18years")
  
  
  # Check inputs ------------------------------------------------------------
  # Check if all subjects are to be processed
  if(missing(listID2445))
  {
    doAllSubjs <- TRUE
  } else
  {
    doAllSubjs <- FALSE
  }
  
  # Check if diagnostic codes are passed
  if(missing(listDiagCodes))
  {
    doAllCodes <- TRUE
  } else
  {
    doAllCodes <- FALSE
    
    # Make sure that all codes are usable
    listDiagCodes <- sub("\\.", "", listDiagCodes)
    listDiagCodes <- sub(" ",   "", listDiagCodes)
  }
  
  
  # Determine file to read --------------------------------------------------
  if(missing(codeType))
  {
    codeType <- "all"
  } else
  {
    codeType <- match.arg(codeType)
  }
  
  
  listFiles <- file.path(pathNPR, list.files(path = pathNPR, 
                                             pattern = glob2rx(paste("*-*-*-MoBa-ParsedNPR_", 
                                                                     codeType, "Codes.csv", sep = ""))))
  if(length(listFiles) == 0)
  {
    stop("No NPR file(s) found; have the path(s) or file name(s) changed?")
  } else
  {
    if(length(listFiles) > 1)
    {
      warning(paste("Multiple NPR files found; selecting: ", listFiles[length(listFiles)], sep = ""))
      listFiles <- listFiles[length(listFiles)]
    }
  }
  
  
  # Get header --------------------------------------------------------------
  tmpNamesNPR <- colnames(fread(file = listFiles, sep = delimiter, nrows = 0))
  
  
  # List of codes to read ---------------------------------------------------
  if(doAllCodes)
  {
    listDiagCodes <- unlist(lapply(unique(sapply(strsplit(tmpNamesNPR[6:length(tmpNamesNPR)], "_"), 
                                                 "[", 1)), paste, allTimes, sep = "_x"))
    namesToLoad   <- c(fixedNames, listDiagCodes)
  } else
  {
    if(!wildCard)
    {
      # Make a list of all cross-sectional column names
      namesToLoad   <- c(fixedNames, unlist(
        lapply(
          listDiagCodes, paste, allTimes, sep = "_x")))
    } else
    {
      namesToLoad   <- c(fixedNames, tmpNamesNPR[unlist(
        sapply(
          glob2rx(
            unlist(
              lapply(
                listDiagCodes, paste, allTimes, sep = "_x"))), 
          grep, tmpNamesNPR))])
    }
  }
  
  
  # Read the file -----------------------------------------------------------
  if(sum(namesToLoad %in% tmpNamesNPR) != length(namesToLoad))
  {
    stop("One or more diagnostic codes were not found in the NPR")
  }
  dataNPR <- fread(file = listFiles, sep = delimiter, header = TRUE, 
                   select = namesToLoad, data.table = FALSE)
  
  
  # Subset subjects, if required --------------------------------------------
  if(!doAllSubjs)
  {
    dataNPR <- merge(x = as.data.frame(listID2445), y = dataNPR, by.x = "listID2445", 
                     by.y = "ID_2445", all.x = TRUE)
    dataNPR <- dataNPR[rank(listID2445), ]
    names(dataNPR)[names(dataNPR) == "listID2445"] <- "ID_2445"
  }
  
  
  # List of diagnostic codes ------------------------------------------------
  onlyCodes <- unique(sapply(strsplit(namesToLoad[6:length(namesToLoad)], "_x"), "[", 1))
  
  
  # All years of diagnoses --------------------------------------------------
  # Find every occurrence of diagnoses of interest
  xx  <- stack(apply(as.matrix(dataNPR[,6:ncol(dataNPR)] == 1), 2, which))
  
  # Merge all diagnostic codes by subject index
  xx2 <- aggregate(xx["ind"], by=xx["values"], paste)
  
  # Initialize a table of results
  diagInfo            <- as.data.frame(matrix(data = NA, nrow = nrow(dataNPR), 
                                              ncol = length(onlyCodes)+1))
  firstDiag           <- as.data.frame(matrix(data = NA, nrow = nrow(dataNPR), 
                                              ncol = length(onlyCodes)+1))
  colnames(diagInfo)  <- c("ID2445", onlyCodes)
  colnames(firstDiag) <- c("ID2445", onlyCodes)
  diagInfo$ID2445     <- dataNPR$ID_2445
  firstDiag$ID2445    <- dataNPR$ID_2445
  
  # Go over every subject who has any of the diagnoses and then compile info
  for(idx in 1:nrow(xx2))
  {
    # What is this index?
    whichIdx <- xx2$values[idx]
    
    # Who is this subject?
    whichSubject <- dataNPR$ID_2445[whichIdx]
    
    # Which is the corresponding row in results table? - should be same as whichIdx
    whichRow     <- which(diagInfo$ID2445 %in% whichSubject)
    
    # Sanity check
    if(whichRow != whichIdx)
    {
      stop("Something went wrong in subject alignment")
    }
    
    # What was the retrieved information?
    info <- strsplit(unlist(xx2$ind[idx]), "_x")
    
    # Which codes and years did we find?
    whichDiag  <- sapply(info, "[", 1)
    whichYears <- sapply(info, "[", 2)
    
    # Go over every code and compile years
    uqDiag <- unique(whichDiag)
    for(codes in uqDiag)
    {
      # Every year this diagnosis occurs for this subject
      diagInfo[whichRow, codes]  <- paste(whichYears[whichDiag == codes], collapse = ",")
      
      # Year of first diagnosis
      firstDiag[whichRow, codes] <- min(as.numeric(whichYears[whichDiag == codes]))
    }
  }
  
  # For firstDiag, also calculate the "min" year across all diagnostic codes
  firstDiag$minYear <- suppressWarnings(apply(X = firstDiag[, 2:ncol(firstDiag)], 
                                              MARGIN = 1, FUN = min, na.rm = TRUE))
  firstDiag$minYear[firstDiag$minYear == Inf] <- NA
  firstDiag <- firstDiag[, c(1,ncol(firstDiag), 2:(ncol(firstDiag)-1))]
  
  
  # If year of birth is included, calculate age ------------------
  if(!missing(yearOfBirth))
  {
    # Age of first diagnoses
    ageFirstDiag <- firstDiag[,2:ncol(firstDiag)] - yearOfBirth
    
    # Include the ID column
    ageFirstDiag$ID2445 <- firstDiag$ID2445
    
    # Rearrange columns
    ageFirstDiag <- ageFirstDiag[, c(ncol(ageFirstDiag), 1:(ncol(ageFirstDiag)-1))]
    
    # Rename minYear to minAge
    names(ageFirstDiag)[names(ageFirstDiag) == "minYear"] <- "minAge"
    
    # Age of all diagnoses
    ageAllDiag <- as.data.frame(matrix(data = NA, nrow = nrow(diagInfo), 
                                       ncol = length(onlyCodes)+1))
    colnames(ageAllDiag) <- c("ID2445", onlyCodes)
    ageAllDiag$ID2445    <- diagInfo$ID2445
    
    # Go over every diagnostic category and calculate
    for(diag in colnames(diagInfo)[2:ncol(diagInfo)])
    {
      tmp  <- diagInfo[, diag]
      locs <- which(!(is.na(tmp)))
      for(ll in locs)
      {
        ageAllDiag[ll, diag] <- paste(as.numeric(unlist(strsplit(
          diagInfo[ll, diag], split = ",")))
          - yearOfBirth[ll], collapse = ",")
      }
    }
  }
  
  
  # Put together as a list to return ----------------------------------------
  if(missing(yearOfBirth))
  {
    results <- list("DiagInfo" = diagInfo, "FirstDiagnoses" = firstDiag)
  } else
  {
    results <- list("DiagInfo" = diagInfo, "FirstDiagnoses" = firstDiag, 
                    "ageFirstDiag" = ageFirstDiag, "ageAllDiag" = ageAllDiag)
  }
  return(results)
}
