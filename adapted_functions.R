#directory: FLA2_LPA_c
MyplotGrowthMixtures <-
  function(modelList,
           bw = FALSE,
           rawdata = FALSE,
           estimated = TRUE,
           poly = FALSE,
           alpha_range = c(0, .1),
           growth_variables = NULL,
           time_scale = NULL,
           jitter_lines = NULL,
           coefficients = "unstandardized") {
    # Check if mplusModel is of class mplus.model
    if (!(inherits(modelList, "mplus.model") |
          all(sapply(modelList, function(x) {
            inherits(x, "mplus.model")
          })))) {
      stop(
        "plotGrowthMixtures requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
      )
    }
    if (inherits(modelList, "mplus.model")) {
      modelList <- list(Model_1 = modelList)
    }
    mixtures <- sapply(modelList, function(x) {
      !is.null(x$input$analysis[["type"]])
    })
    mixtures[which(mixtures)] <-
      sapply(modelList[which(mixtures)], function(x) {
        grepl("mixture", tolower(x$input$analysis$type))
      })
    
    if (any(!mixtures)){
      if (!any(mixtures))
        stop(
          "plotMixtures requires a list of mixture models, or one mixture model, as its first argument."
        )
      warning(
        "Some output files were excluded because they are not mixture models; specifically: ",
        paste(names(modelList)[which(!mixtures)], collapse = ", "),
        call. = FALSE
      )
      modelList <- modelList[which(mixtures)]
    }
    # Remove models which are not type "mixture"
    
    # Check if all models were run on the same dataset
    if (length(unique(sapply(modelList, function(x) {
      x$input$data$file
    }))) > 1) {
      stop("Models were not all run on the same data file.")
    }
    # Check if any models have missing columns (result of nonconvergence)
    missing_cols <- sapply(modelList, function(x) {
      length(names(x))
    })
    missing_cols <- which(missing_cols != max(missing_cols))
    if (length(missing_cols) > 0) {
      warning(
        "Some models had missing columns in the coefficients section. This likely indicates a convergence problem. These models were dropped: ",
        paste(names(modelList)[missing_cols], collapse = ", "),
        call. = FALSE
      )
      modelList <- modelList[-missing_cols]
    }
    
    # Check if all models are growth models
    is_growth_model <- sapply(modelList, function(x){
      any(grepl("\\|$", x$parameters[[coefficients]]$paramHeader))|
        any(grepl("\\w+\\|(\\s?\\b\\w+\\b\\s?){2,}", x$input$model))
    })
    if (any(!is_growth_model)){
      if (!any(is_growth_model))
        stop(
          "plotMixtures requires a list of growth models, or one growth model, as its first argument."
        )
      warning(
        "Some output files were excluded because they are not growth models; specifically: ",
        paste(names(modelList)[which(!is_growth_model)], collapse = ", "),
        call. = FALSE
      )
      # Remove models which are not growth models
      modelList <- modelList[which(is_growth_model)]
    }
    
    
    # Prepare plot data
    # Get coefficients
    missing_coefficients <- sapply(modelList, function(x) {
      is.null(x$parameters[[coefficients]])
    })
    if (any(missing_coefficients)) {
      warning(
        "Some models were missing the ",
        coefficients,
        " coefficients. Please request these coefficients from Mplus for models: ",
        paste(names(modelList)[which(missing_coefficients)], collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
      modelList <- modelList[-which(missing_coefficients)]
    }
    
    if (length(modelList) < 1)
      stop("No data left to plot.", call. = FALSE)
    
    plotdat <-
      lapply(modelList, function(x) {
        x$parameters[[coefficients]]
      })
    
    # Get matrix of loadings
    loadings <-
      lapply(plotdat, function(x) {
        tmp <- subset(
          x,
          subset = if (is.null(growth_variables)) {
            grepl("\\|$", x$paramHeader)
          } else {
            x$paramHeader %in% paste0(toupper(growth_variables), ".|")
          },
          select = c("paramHeader", "est", "param", "est_se", "LatentClass")
        )
        tmp$paramHeader <- gsub("\\.\\|$", "", tmp$paramHeader)
        tmp
      })
    
    if(is.null(growth_variables)) growth_variables <- unique(unlist(lapply(loadings, function(x){x$paramHeader})))
    
    # Set time scale
    if (is.null(time_scale) &
        !all(unlist(lapply(loadings, function(x) {
          x$est_se == 999
        })))) {
      stop(
        "Factor loadings freely estimated. Please specify the correct time scale in the argument time_scale."
      )
    }
    
    observed_variables <-
      table(unlist(lapply(loadings, function(x) {
        x$param
      })))
    if (any(observed_variables != max(observed_variables))) {
      stop("Different variables used for latent growth analyses across models.")
    }
    
    loadings <- lapply(loadings, function(x) {
      array(x$est, dim=c(length(unique(x$param)), length(unique(x$paramHeader)), length(unique(x$LatentClass))))
    })
    
    
    
    num_loadings <- sapply(loadings, nrow)
    if(!all(num_loadings == max(num_loadings))){
      stop("Different models appear to be on different time scales (not the same number of loadings for latent growth variables.")
    } else {
      if(is.null(time_scale)) time_scale <- 0:(max(num_loadings)-1)
    }
    
    # Extract estimates
    estimates <- lapply(plotdat, function(x) {
      subset(
        x,
        select = c("param", "est","se", "LatentClass"),
        subset = (x$param %in% toupper(growth_variables)) &
          (x$paramHeader %in% c("Means", "Intercepts")))
    })
    # Extract estimates
    estimates_ses <- lapply(plotdat, function(x) {
      subset(
        x,
        select = c("param", "est","se", "LatentClass"),
        subset = (x$param %in% toupper(growth_variables)) &
          (x$paramHeader %in% c("Means", "Intercepts")))
    })
    
    # Extract estimates
    estimates_ <- lapply(estimates, function(x) {
      cbind(x, lci= x$est - (1.96*x$se),
            uci= x$est + (1.96*x$se))
    })
    
    estimates <- lapply(estimates_, function(x) {
      array(sapply(x$est, rep, length(time_scale)), dim=c(length(time_scale), length(unique(x$param)), length(unique(x$LatentClass))))
    })
    
    estimates_lci <- lapply(estimates_, function(x) {
      array(sapply(x$lci, rep, length(time_scale)), dim=c(length(time_scale), length(unique(x$param)), length(unique(x$LatentClass))))
    })
    
    estimates_uci <- lapply(estimates_, function(x) {
      array(sapply(x$uci, rep, length(time_scale)), dim=c(length(time_scale), length(unique(x$param)), length(unique(x$LatentClass))))
    })
    
    
    predicted_trajectories <- lapply(1:length(plotdat), function(x){
      loadings[[x]] * estimates[[x]]
    })
    
    predicted_trajectories <- lapply(predicted_trajectories, apply, 3, rowSums)
    
    predicted_trajectories <- unlist(lapply(predicted_trajectories, matrix))
    
    
    predicted_trajectory_lci <- lapply(1:length(plotdat), function(x){
      loadings[[x]] * estimates_lci[[x]]
    })
    
    predicted_trajectory_lci <- lapply(predicted_trajectory_lci, apply, 3, rowSums)
    
    predicted_trajectory_lci <- unlist(lapply(predicted_trajectory_lci, matrix))
    
    predicted_trajectory_uci <- lapply(1:length(plotdat), function(x){
      loadings[[x]] * estimates_uci[[x]]
    })
    
    predicted_trajectory_uci <- lapply(predicted_trajectory_uci, apply, 3, rowSums)
    
    predicted_trajectory_uci <- unlist(lapply(predicted_trajectory_uci, matrix))
    #Time <- rep(time_scale, length(predicted_trajectories)/length(time_scale))
    Time <- time_scale
    classes <- sapply(loadings, function(x){ dim(x)[3]})
    Class <- unlist(sapply(classes, function(x){
      sort(rep(1:x, length(time_scale)))
    }), use.names = FALSE)
    Title <- unlist(mapply(FUN = function(Title, Times){
      rep(Title, Times*length(time_scale))
    }, Title = sapply(names(loadings), function(x){
      trimws(modelList[[x]]$input$title)
    }), Times = classes))
    
    predicted_trajectories <- data.frame(Time = Time,
                                         Value = predicted_trajectories,
                                         Lci = predicted_trajectory_lci,
                                         Uci = predicted_trajectory_uci,
                                         Class = ordered(Class),
                                         Title = Title, row.names = NULL)
    
    
    
    # mylist<-list(trajs=predicted_trajectories, ses=predicted_trajectory_ses)
    return(predicted_trajectories)
  }

MyplotGrowthModels <-
  function(modelList,
           bw = FALSE,
           rawdata = FALSE,
           estimated = TRUE,
           poly = FALSE,
           alpha_range = c(0, .1),
           growth_variables = NULL,
           time_scale = NULL,
           jitter_lines = NULL,
           coefficients = "unstandardized") {
    # Check if mplusModel is of class mplus.model
    if (!(inherits(modelList, "mplus.model") |
          all(sapply(modelList, function(x) {
            inherits(x, "mplus.model")
          })))) {
      stop(
        "MyplotGrowthModels requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
      )
    }
    
    
    # Check if all models were run on the same dataset
    if (length(unique(sapply(modelList, function(x) {
      x$input$data$file
    }))) > 1) {
      stop("Models were not all run on the same data file.")
    }
    # Check if any models have missing columns (result of nonconvergence)
    missing_cols <- sapply(modelList, function(x) {
      length(names(x))
    })
    missing_cols <- which(missing_cols != max(missing_cols))
    if (length(missing_cols) > 0) {
      warning(
        "Some models had missing columns in the coefficients section. This likely indicates a convergence problem. These models were dropped: ",
        paste(names(modelList)[missing_cols], collapse = ", "),
        call. = FALSE
      )
      modelList <- modelList[-missing_cols]
    }
    
    # Check if all models are growth models
    is_growth_model <- sapply(modelList, function(x){
      any(grepl("\\|$", x$parameters[[coefficients]]$paramHeader))|
        any(grepl("\\w+\\|(\\s?\\b\\w+\\b\\s?){2,}", x$input$model))
    })
    if (any(!is_growth_model)){
      if (!any(is_growth_model))
        stop(
          "MyplotGrowthModels requires a list of growth models, or one growth model, as its first argument."
        )
      warning(
        "Some output files were excluded because they are not growth models; specifically: ",
        paste(names(modelList)[which(!is_growth_model)], collapse = ", "),
        call. = FALSE
      )
      # Remove models which are not growth models
      modelList <- modelList[which(is_growth_model)]
    }
    
    
    # Prepare plot data
    # Get coefficients
    missing_coefficients <- sapply(modelList, function(x) {
      is.null(x$parameters[[coefficients]])
    })
    if (any(missing_coefficients)) {
      warning(
        "Some models were missing the ",
        coefficients,
        " coefficients. Please request these coefficients from Mplus for models: ",
        paste(names(modelList)[which(missing_coefficients)], collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
      modelList <- modelList[-which(missing_coefficients)]
    }
    
    if (length(modelList) < 1)
      stop("No data left to plot.", call. = FALSE)
    
    plotdat <-
      lapply(modelList, function(x) {
        x$parameters[[coefficients]]
      })
    
    # Get matrix of loadings
    loadings <-
      lapply(plotdat, function(x) {
        tmp <- subset(
          x,
          subset = if (is.null(growth_variables)) {
            grepl("\\|$", x$paramHeader)
          } else {
            x$paramHeader %in% paste0(toupper(growth_variables), ".|")
          },
          select = c("paramHeader", "est", "param", "est_se")
        )
        tmp$paramHeader <- gsub("\\.\\|$", "", tmp$paramHeader)
        tmp
      })
    
    if(is.null(growth_variables)) growth_variables <- unique(unlist(lapply(loadings, function(x){x$paramHeader})))
    
    # Set time scale
    if (is.null(time_scale) &
        !all(unlist(lapply(loadings, function(x) {
          x$est_se == 999
        })))) {
      stop(
        "Factor loadings freely estimated. Please specify the correct time scale in the argument time_scale."
      )
    }
    
    observed_variables <-
      table(unlist(lapply(loadings, function(x) {
        x$param
      })))
    if (any(observed_variables != max(observed_variables))) {
      stop("Different variables used for latent growth analyses across models.")
    }
    
    loadings <- lapply(loadings, function(x) {
      array(x$est, dim=c(length(unique(x$param)), length(unique(x$paramHeader)), 1))
    })
    
    num_loadings <- sapply(loadings, nrow)
    if(!all(num_loadings == max(num_loadings))){
      stop("Different models appear to be on different time scales (not the same number of loadings for latent growth variables.")
    } else {
      if(is.null(time_scale)) time_scale <- 0:(max(num_loadings)-1)
    }
    
    # Extract estimates
    estimates <- lapply(plotdat, function(x) {
      subset(
        x,
        select = c("param", "est"),
        subset = (x$param %in% toupper(growth_variables)) &
          (x$paramHeader %in% c("Means", "Intercepts")))
    })
    
    estimates <- lapply(estimates, function(x) {
      array(sapply(x$est, rep, length(time_scale)), dim=c(length(time_scale), length(unique(x$param)), 1))
    })
    
    predicted_trajectories <- lapply(1:length(plotdat), function(x){
      loadings[[x]] * estimates[[x]]
    })
    
    predicted_trajectories <- lapply(predicted_trajectories, apply, 3, rowSums)
    
    predicted_trajectories <- unlist(lapply(predicted_trajectories, matrix))
    #Time <- rep(time_scale, length(predicted_trajectories)/length(time_scale))                                     
    Time <- time_scale
    classes <- sapply(loadings, function(x){ dim(x)[3]})
    Class <- unlist(sapply(classes, function(x){
      sort(rep(1:x, length(time_scale)))
    }), use.names = FALSE)
    Title <- unlist(mapply(FUN = function(Title, Times){
      rep(Title, Times*length(time_scale))
    }, Title = sapply(names(loadings), function(x){
      trimws(modelList[[x]]$input$title)
    }), Times = 1))
    
    predicted_trajectories <- data.frame(Time = Time, 
                                         Value = predicted_trajectories, 
                                         Class = ordered(Class),
                                         Title = Title, row.names = NULL)
    
    
    
    my_list = list(ests=estimates,loadings=loadings,trajs=predicted_trajectories)
    return(predicted_trajectories)
  }

#master function to parse text into sections
parse_into_sections <- function(outfiletext) {
  headers <- c("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS", "SUMMARY OF DATA",
               "SUMMARY OF DATA FOR THE FIRST DATA SET", "SUMMARY OF DATA FOR THE FIRST REPLICATION",
               "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION",
               "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST DATA SET",
               "SUMMARY OF MISSING DATA PATTERNS",
               "SUMMARY OF CATEGORICAL DATA PROPORTIONS",
               "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION",
               "COVARIANCE COVERAGE OF DATA", "UNIVARIATE SAMPLE STATISTICS",
               "THE MODEL ESTIMATION TERMINATED NORMALLY",
               "SAMPLE STATISTICS", "SAMPLE STATISTICS FOR THE FIRST REPLICATION",
               "RESULTS FOR BASIC ANALYSIS",
               "CROSSTABS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES",
               "SUMMARY OF CENSORED LIMITS", "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES",
               "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
               "TESTS OF MODEL FIT", "MODEL FIT INFORMATION", "MODEL FIT INFORMATION FOR .*", "CLASSIFICATION QUALITY",
               "SUMMARY OF MODEL FIT INFORMATION", "RESULTS FOR EXPLORATORY FACTOR ANALYSIS",
               "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES",
               "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS",
               "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN",#
               "Average Latent Class Probabilities for Most Likely Latent Class Pattern \\(Row\\)",#
               "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL",
               "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE",
               "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
               "Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)",
               "Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)",
               "Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)",
               "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)",
               "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)",
               "MODEL RESULTS", "MODEL RESULTS FOR .*", "LOGISTIC REGRESSION ODDS RATIO RESULTS", "RESULTS IN PROBABILITY SCALE",
               "IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC METRIC",
               "IRT PARAMETERIZATION IN TWO-PARAMETER PROBIT METRIC",
               "IRT PARAMETERIZATION",
               "BRANT WALD TEST FOR PROPORTIONAL ODDS",
               "BETWEEN-LEVEL FACTOR SCORE COMPARISONS",
               "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
               "LATENT CLASS ODDS RATIO RESULTS", "LOGRANK OUTPUT", "STANDARDIZED MODEL RESULTS",
               "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER \\d+",
               "R-SQUARE", "QUALITY OF NUMERICAL RESULTS", "QUALITY OF NUMERICAL RESULTS FOR .*", "TECHNICAL OUTPUT", "TECHNICAL \\d+ OUTPUT",
               "TECHNICAL \\d+ OUTPUT FOR .*", "TECHNICAL 5/6 OUTPUT",
               "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
               "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES",
               "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS \\(CAUSALLY-DEFINED EFFECTS\\)",
               "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "CONFIDENCE INTERVALS OF MODEL RESULTS",
               "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",
               "CREDIBILITY INTERVALS OF MODEL RESULTS",
               "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS",
               "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS",
               "CONFIDENCE INTERVALS IN PROBABILITY SCALE",
               "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
               "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,", #omitted "AND DIRECT EFFECTS" in v7
               "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", #fit onto 1 line in v8!
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING POSTERIOR PROBABILITY-BASED",
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE",
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE 3-STEP PROCEDURE",
               "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES",
               "THE FOLLOWING DATA SET\\(S\\) DID NOT RESULT IN A COMPLETED REPLICATION:",
               "RESIDUAL OUTPUT", "MODEL MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",
               "SUMMARIES OF PLAUSIBLE VALUES \\(N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS\\)",
               "SUMMARY OF PLAUSIBLE STANDARD DEVIATION \\(N = NUMBER OF OBSERVATIONS\\)",
               "Available post-processing tools:",
               "FACTOR SCORE INFORMATION \\(COMPLETE DATA\\)", "SUMMARY OF FACTOR SCORES", "PLOT INFORMATION", "SAVEDATA INFORMATION",
               "RESULTS SAVING INFORMATION", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", "DIAGRAM INFORMATION",
               "Beginning Time:\\s*\\d+:\\d+:\\d+", "MUTHEN & MUTHEN"
  )
  
  #form alternation pattern for regular expression (currently adds leading and trailing spaces permission to each header)
  headerRegexpr <- paste("(", paste(gsub("(.*)", "^\\\\s*\\1\\\\s*$", headers, perl=TRUE), sep="", collapse="|"), ")", sep="")
  headerLines <- grep(headerRegexpr, outfiletext, perl=TRUE)
  
  attr(outfiletext, "headerlines") <- headerLines
  return(outfiletext)
}