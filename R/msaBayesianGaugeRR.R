#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @export
msaBayesianGaugeRR <- function(jaspResults, dataset, options) {
  vars  <- .msabVariables(options)
  ready <- .msabIsReady(vars)

  data    <- NULL
  fits    <- NULL
  model   <- NULL
  samples <- NULL
  draws   <- NULL

  if (ready) {
    data    <- .msabReadDataset(dataset, options, vars)
    vars    <- data[["vars"]]
    fits    <- .msabFitModels(jaspResults, data[["long"]], options, vars)
    model   <- .msabSelectModel(fits, options)
    samples <- .msabRunMCMC(jaspResults, fits[[model]], options, vars)
    draws   <- .msabComponentDraws(samples, options)
  }

  if (options[["report"]]) {
    .msabReport(jaspResults, data, draws, options, vars, ready)
    return()
  }

  if (options[["RRTable"]]) {
    .msabBfTable(jaspResults, fits, options, vars, ready)
    .msabVarCompTable(jaspResults, draws, model, options, ready)
    .msabContribTable(jaspResults, draws, options, ready)
    .msabGaugeEvalTables(jaspResults, draws, options, ready)
  }

  if (options[["priorPlot"]])
    .msabPriorPlot(jaspResults, options)

  if (options[["diagnosticsTable"]] || options[["diagnosticsPlots"]])
    .msabDiagnostics(jaspResults, samples, options, ready)

  if (options[["varianceComponentsGraph"]])
    .msabVarCompPlot(jaspResults, draws, options, ready)

  if (options[["posteriorPlot"]])
    .msabPosteriors(jaspResults, samples, draws, options, ready)

  if (options[["rChart"]] && !options[["type3"]])
    .msabRChart(jaspResults, data, vars, ready)

  if (options[["xBarChart"]] && !options[["type3"]])
    .msabXbarChart(jaspResults, data, vars, ready)

  if (options[["scatterPlot"]] && !options[["type3"]])
    .msabScatterPlot(jaspResults, data, options, vars, ready)

  if (options[["partMeasurementPlot"]])
    .msabMeasureByPartPlot(jaspResults, data, options, vars, ready)

  if (options[["operatorMeasurementPlot"]] && !options[["type3"]])
    .msabMeasureByOperatorPlot(jaspResults, data, options, vars, ready)

  if (options[["partByOperatorMeasurementPlot"]] && !options[["type3"]])
    .msabPartByOperatorPlot(jaspResults, data, options, vars, ready)

  if (options[["trafficLightChart"]])
    .msabTrafficLightPlot(jaspResults, draws, options, ready)

  if (options[["contourPlot"]])
    .msabContourPlot(jaspResults, data, draws, options, vars, ready)
}

#### Dependencies ----
.msabDataDeps <- c("dataFormat", "type3",
                   "measurementLongFormat", "operatorLongFormat", "partLongFormat",
                   "measurementsWideFormat", "operatorWideFormat", "partWideFormat")

# Bayes factor model fits
.msabBfDeps <- c(.msabDataDeps, "rscalePrior", "setSeed", "seed")

# MCMC samples
.msabModelDeps <- c(.msabBfDeps, "estimationType", "modelType", "bfFavorFull",
                    "mcmcChains", "mcmcIterations", "mcmcBurnin")

# variance components derived from the MCMC samples
.msabComponentDeps <- c(.msabModelDeps, "processVariationReference", "historicalSdValue")

.msabStudyVarDeps  <- c("studyVarianceMultiplierType", "studyVarianceMultiplierValue")
.msabToleranceDeps <- c("tolerance", "toleranceValue")

.msabPosteriorDeps <- c("posteriorPlotType", "distType", "posteriorHistogram",
                        "posteriorPointEstimate", "posteriorPointEstimateType",
                        "posteriorCi", "posteriorCiType", "posteriorCiMass", "customCiType",
                        "posteriorCiLower", "posteriorCiUpper", "posteriorCiCutOff")

.msabReportDeps <- c("report", "reportMetaData", "reportTitle", "reportTitleText",
                     "reportPartName", "reportPartNameText", "reportGaugeName", "reportGaugeNameText",
                     "reportCharacteristic", "reportCharacteristicText", "reportGaugeNumber", "reportGaugeNumberText",
                     "reportTolerance", "reportToleranceText", "reportLocation", "reportLocationText",
                     "reportPerformedBy", "reportPerformedByText", "reportDate", "reportDateText",
                     "reportGaugeTable", "reportVariationComponents", "reportMeasurementsByPartPlot",
                     "reportRChartByOperator", "reportMeasurementsByOperatorPlot", "reportAverageChartByOperator",
                     "reportPartByOperatorPlot", "reportTrafficLightChart")

#### Data reading & error checks ----
.msabVariables <- function(options) {
  wideFormat <- options[["dataFormat"]] == "wideFormat"

  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    parts        <- options[["partWideFormat"]]
    operators    <- options[["operatorWideFormat"]]
  } else {
    measurements <- unlist(options[["measurementLongFormat"]])
    parts        <- options[["partLongFormat"]]
    operators    <- options[["operatorLongFormat"]]
  }

  # a type 3 study has no operators; a constant dummy factor is added to the data
  if (options[["type3"]])
    operators <- "operators"

  list(wideFormat   = wideFormat,
       measurements = measurements[measurements != ""],
       parts        = parts,
       operators    = operators)
}

.msabIsReady <- function(vars) {
  minMeasurements <- if (vars[["wideFormat"]]) 2 else 1
  length(vars[["measurements"]]) >= minMeasurements && vars[["parts"]] != "" && vars[["operators"]] != ""
}

.msabReadDataset <- function(dataset, options, vars) {
  measurements <- vars[["measurements"]]
  parts        <- vars[["parts"]]
  operators    <- vars[["operators"]]
  factorVars   <- if (options[["type3"]]) parts else c(parts, operators)

  if (is.null(dataset))
    dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = factorVars)

  dataset <- as.data.frame(dataset)[c(measurements, factorVars)]
  for (v in factorVars)
    dataset[[v]] <- as.factor(dataset[[v]])
  if (options[["type3"]])
    dataset[[operators]] <- factor(rep(1, nrow(dataset)))

  .hasErrors(dataset, type = c("missingValues", "infinity"),
             missingValues.target = c(measurements, factorVars),
             infinity.target      = measurements,
             exitAnalysisIfErrors = TRUE)

  if (vars[["wideFormat"]]) {
    dataWide        <- dataset
    dataLong        <- as.data.frame(.convertToLong(dataset, measurements))
    measurementLong <- "Measurements" # name assigned to the column inside the conversion function
  } else {
    dataLong        <- dataset
    dataWide        <- .convertToWide(dataset, measurements, parts, operators)
    measurementLong <- measurements
  }

  vars[["measurementLong"]]  <- measurementLong
  vars[["measurementsWide"]] <- setdiff(colnames(dataWide), c(parts, operators))

  .msabCheckDesign(dataLong, dataWide, options, vars)

  list(long = dataLong, wide = dataWide, vars = vars)
}

.msabCheckDesign <- function(dataLong, dataWide, options, vars) {
  parts     <- vars[["parts"]]
  operators <- vars[["operators"]]

  if (!options[["type3"]] && nlevels(droplevels(dataLong[[operators]])) == 1)
    .quitAnalysis(gettext("This dataset seems to have only a single unique operator. Please use the Type 3 study by checking the box below."))

  if (nlevels(droplevels(dataLong[[parts]])) < 2)
    .quitAnalysis(gettext("At least two different parts are required."))

  if (!options[["type3"]] && !.checkIfCrossed(dataLong, operators, parts, vars[["measurementLong"]]))
    .quitAnalysis(gettext("Design is not balanced: not every operator measured every part. Use non-replicable gauge r&R."))

  # in wide format every part/operator combination should occupy a single row
  if (vars[["wideFormat"]] && !options[["type3"]]) {
    nRows <- max(table(dataWide[[parts]], dataWide[[operators]]))
    if (nRows > 1)
      .quitAnalysis(gettextf("The measurements selected seem to be in a 'Single Column' format as every operator's part is measured %s times.", nRows))
  }

  # long data with unequal numbers of replicates leads to NAs after conversion to wide format
  if (anyNA(dataWide))
    .quitAnalysis(gettext("Number of replicates differ per operator/part. Make sure that each operator measures each part equally often."))

  if (length(vars[["measurementsWide"]]) < 2)
    .quitAnalysis(gettext("At least two measurements per part and operator are required."))
}

.convertToWide <- function(dataset, measurements, parts, operators) {
  dataset <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(dataset, dplyr::across(dplyr::all_of(c(parts, operators)))),
      trial = dplyr::row_number()
    )
  )

  dataWide <- tidyr::pivot_wider(dataset, id_cols = dplyr::all_of(c(parts, operators)), values_from = dplyr::all_of(measurements),
                                 names_from = "trial", names_prefix = "V")
  dataWide <- as.data.frame(dataWide)
  dataWide <- dataWide[order(dataWide[, parts]), ]

  return(dataWide)
}

.convertToLong <- function(dataset, measurements) {
  tidyr::pivot_longer(dataset, cols = tidyr::all_of(measurements),
                      values_to = "Measurements", names_to = NULL)
}

#### Models ----
.msabFitModels <- function(jaspResults, dataLong, options, vars) {
  # models needed: the one used for estimation and, for the model comparison, the other one
  needed <- if (options[["type3"]]) {
    "full"
  } else if (options[["estimationType"]] == "automatic" || (options[["RRTable"]] && !options[["report"]])) {
    c("main", "full")
  } else if (options[["modelType"]] == "mainEffectsOnly") {
    "main"
  } else {
    "full"
  }

  state <- jaspResults[["modelFits"]]
  if (is.null(state)) {
    state <- createJaspState()
    state$dependOn(.msabBfDeps)
    jaspResults[["modelFits"]] <- state
  }
  fits <- state[["object"]]
  if (is.null(fits))
    fits <- list()

  missing <- setdiff(needed, names(fits))
  if (length(missing) == 0)
    return(fits)

  measurement <- vars[["measurementLong"]]
  parts       <- vars[["parts"]]
  operators   <- vars[["operators"]]

  for (m in missing) {
    if (options[["type3"]]) {
      formula     <- as.formula(paste(measurement, "~", parts))
      whichRandom <- parts
    } else {
      rhs         <- if (m == "full") paste0(parts, " + ", operators, " + ", parts, ":", operators) else paste(parts, "+", operators)
      formula     <- as.formula(paste(measurement, "~", rhs))
      whichRandom <- c(parts, operators)
    }

    # seed per model so the result does not depend on the order in which the models are fit
    if (options[["setSeed"]])
      set.seed(options[["seed"]])

    fits[[m]] <- try(BayesFactor::lmBF(formula, data = dataLong, whichRandom = whichRandom,
                                       rscaleRandom = options[["rscalePrior"]], progress = FALSE))
  }

  state[["object"]] <- fits
  return(fits)
}

# Bayes factor (and its proportional error) of the full model over the main effects model
.msabBfFullVsMain <- function(fits) {
  full <- BayesFactor::extractBF(fits[["full"]], logbf = TRUE)
  main <- BayesFactor::extractBF(fits[["main"]], logbf = TRUE)
  list(bf = exp(full[["bf"]] - main[["bf"]]), error = sqrt(full[["error"]]^2 + main[["error"]]^2))
}

.msabSelectModel <- function(fits, options) {
  if (options[["type3"]] || options[["estimationType"]] == "manual") {
    model <- if (!options[["type3"]] && options[["modelType"]] == "mainEffectsOnly") "main" else "full"
    if (isTryError(fits[[model]]))
      .quitAnalysis(gettextf("The BayesFactor model could not be fit: %s", .rmNewLine(fits[[model]])))
    return(model)
  }

  failed <- Filter(isTryError, fits)
  if (length(failed) > 0)
    .quitAnalysis(gettextf("Model comparison Bayes factors could not be computed: %s<br>Select manual estimation to try running the rest of the analysis.",
                           .rmNewLine(failed[[1]])))

  if (.msabBfFullVsMain(fits)[["bf"]] <= options[["bfFavorFull"]]) "main" else "full"
}

.msabRunMCMC <- function(jaspResults, fit, options, vars) {
  if (!is.null(jaspResults[["MCMCsamples"]]))
    return(jaspResults[["MCMCsamples"]][["object"]])

  # BayesFactor parameter names and the internal ids used in the rest of the analysis
  gNames <- c(part        = paste0("g_", vars[["parts"]]),
              operator    = paste0("g_", vars[["operators"]]),
              interaction = paste0("g_", vars[["parts"]], ":", vars[["operators"]]))

  nChains <- options[["mcmcChains"]]
  burnin  <- options[["mcmcBurnin"]]
  iter    <- options[["mcmcIterations"]]

  if (options[["setSeed"]])
    set.seed(options[["seed"]])

  chains <- vector("list", nChains)
  for (i in seq_len(nChains)) {
    chain <- try(as.matrix(BayesFactor::posterior(fit, iterations = iter, progress = FALSE)))
    if (isTryError(chain))
      .quitAnalysis(gettextf("The MCMC sampler failed: %s", .rmNewLine(chain)))

    gNamesModel <- gNames[gNames %in% colnames(chain)]
    chain       <- chain[-seq_len(burnin), c(gNamesModel, "sig2"), drop = FALSE]

    # revert standardization: sigma^2_effect = g_effect * sigma^2_error
    chain[, gNamesModel] <- chain[, gNamesModel] * chain[, "sig2"]
    colnames(chain)      <- c(names(gNamesModel), "error")
    chains[[i]]          <- chain
  }

  mcmcArray <- array(unlist(chains), dim = c(iter - burnin, ncol(chains[[1]]), nChains),
                     dimnames = list(NULL, colnames(chains[[1]]), NULL))
  mcmcArray <- aperm(mcmcArray, c(1, 3, 2)) # iterations x chains x parameters

  state <- createJaspState()
  state$dependOn(.msabModelDeps)
  state[["object"]] <- mcmcArray
  jaspResults[["MCMCsamples"]] <- state

  return(mcmcArray)
}

.arrayToMat <- function(array) {
  mat <- matrix(array, nrow = prod(dim(array)[1:2]), ncol = dim(array)[3])
  colnames(mat) <- dimnames(array)[[3]]
  return(mat)
}

# variance components per posterior draw
.msabComponentDraws <- function(samples, options) {
  mat <- .arrayToMat(samples)

  repeatability <- mat[, "error"]
  part          <- mat[, "part"]

  if (options[["type3"]]) {
    gauge <- repeatability
    draws <- data.frame(gauge, repeatability)
  } else {
    operator        <- mat[, "operator"]
    reproducibility <- if ("interaction" %in% colnames(mat)) operator + mat[, "interaction"] else operator
    gauge           <- repeatability + reproducibility
    draws           <- data.frame(gauge, repeatability, reproducibility, operator)
  }

  truncated <- 0
  if (options[["processVariationReference"]] == "historicalSd") {
    # the total variation is fixed at the historical variance; the part variation is what remains per draw
    historicalVar <- options[["historicalSdValue"]]^2
    if (historicalVar < mean(gauge))
      .quitAnalysis(gettextf("The historical standard deviation (%1$s) is smaller than the estimated gauge r&R standard deviation (%2$s), so the part-to-part variation cannot be derived. Enter a larger historical standard deviation or base the process variation on the study variation.",
                             options[["historicalSdValue"]], signif(sqrt(mean(gauge)), 3)))

    part      <- historicalVar - gauge
    truncated <- mean(part < 0)
    part      <- pmax(part, 0)
    total     <- rep(historicalVar, length(gauge))
  } else {
    total <- gauge + part
  }

  draws[["part"]]  <- part
  draws[["total"]] <- total

  attr(draws, "truncated") <- truncated
  return(draws)
}

.msabStudyVarMultiplier <- function(options) {
  if (options[["studyVarianceMultiplierType"]] == "sd")
    return(options[["studyVarianceMultiplierValue"]])

  abs(2 * qnorm((1 - options[["studyVarianceMultiplierValue"]] / 100) / 2))
}

# transform the variance draws into the quantity of interest
.msabTransformDraws <- function(draws, options, type = c("var", "sd", "studyVar", "percContrib", "percStudyVar", "percTol")) {
  type  <- match.arg(type)
  draws <- as.matrix(draws)
  switch(type,
         "var"          = draws,
         "sd"           = sqrt(draws),
         "studyVar"     = sqrt(draws) * .msabStudyVarMultiplier(options),
         "percContrib"  = draws / draws[, "total"] * 100,
         "percStudyVar" = sqrt(draws) / sqrt(draws[, "total"]) * 100,
         "percTol"      = sqrt(draws) * .msabStudyVarMultiplier(options) / options[["toleranceValue"]] * 100)
}

# posterior mean, sd and 95% CrI per column; constant columns (e.g. fixed total variation) get no spread
.msabSummariseDraws <- function(draws) {
  draws    <- as.matrix(draws)
  constant <- apply(draws, 2, function(x) all(x == x[1]))

  res <- data.frame(source     = colnames(draws),
                    sourceName = .msabSourceLabels(colnames(draws)),
                    mean       = colMeans(draws),
                    sd         = apply(draws, 2, sd),
                    lower      = apply(draws, 2, quantile, probs = 0.025, names = FALSE),
                    upper      = apply(draws, 2, quantile, probs = 0.975, names = FALSE),
                    row.names  = NULL)
  res[constant, c("sd", "lower", "upper")] <- NA
  return(res)
}

.msabSourceLabels <- function(ids) {
  labels <- c(gauge           = gettext("Total gauge r&R"),
              repeatability   = gettext("Repeatability"),
              reproducibility = gettext("Reproducibility"),
              operator        = gettext("Operator"),
              part            = gettext("Part-to-part"),
              total           = gettext("Total variation"))
  unname(labels[ids])
}

.msabParameterLabels <- function(ids, html = FALSE) {
  labels <- c(part        = gettext("Part"),
              operator    = gettext("Operator"),
              interaction = paste(gettext("Part"), "\u273B", gettext("Operator")),
              error       = gettext("Error"))
  labels <- unname(labels[ids])
  if (html)
    labels <- paste0("\u03C3<sup>2</sup><sub>", labels, "</sub>")
  return(labels)
}

.msabDistinctCategories <- function(draws) {
  nCat <- sqrt(draws[["part"]]) / sqrt(draws[["gauge"]]) * 1.41
  c(mean  = mean(nCat),
    lower = quantile(nCat, 0.025, names = FALSE),
    upper = quantile(nCat, 0.975, names = FALSE))
}

.msabAddCommonFootnotes <- function(table, fillDat, draws) {
  extreme <- .hasExtremeValues(fillDat)
  if (extreme[["large"]])
    table$addFootnote(gettext("Estimates are very large potentially making results unreliable. Consider transforming the data."),
                      symbol = gettext("<b>Warning:</b>"))
  if (extreme[["small"]])
    table$addFootnote(gettext("Estimates are very small potentially making results unreliable. Consider transforming the data."),
                      symbol = gettext("<b>Warning:</b>"))

  truncated <- attr(draws, "truncated")
  if (!is.null(truncated) && truncated > 0)
    table$addFootnote(gettextf("In %s%% of the posterior samples the gauge r&R variance exceeded the historical variance; the part-to-part variance was set to zero in these samples.",
                               signif(100 * truncated, 2)),
                      symbol = gettext("<b>Warning:</b>"))
}

#### Tables ----
.msabBfTable <- function(jaspResults, fits, options, vars, ready) {
  if (!is.null(jaspResults[["BFtable"]]))
    return()

  table <- createJaspTable(title = gettext("Model Comparison"))
  table$position <- 1
  table$dependOn(c(.msabBfDeps, "estimationType", "bfFavorFull", "RRTable", "report"))
  jaspResults[["BFtable"]] <- table

  table$addColumnInfo(name = "modelName",    title = gettext("Models"),          type = "string")
  table$addColumnInfo(name = "comparisonBF", title = gettext("BF<sub>10</sub>"), type = "number")
  table$addColumnInfo(name = "error",        title = gettextf("error %%"),       type = "number")

  if (!ready)
    return()

  failed <- Filter(isTryError, fits)
  if (length(failed) > 0) {
    table$setError(gettextf("Model comparison Bayes factors could not be computed: %s", .rmNewLine(failed[[1]])))
    return()
  }

  parts     <- vars[["parts"]]
  operators <- vars[["operators"]]
  full      <- BayesFactor::extractBF(fits[["full"]])

  if (options[["type3"]]) {
    fillDat <- data.frame(modelName    = c(gettext("Null model"), parts),
                          comparisonBF = c(full[["bf"]], 1),
                          error        = c(full[["error"]], NA) * 100)
  } else {
    fullVsMain <- .msabBfFullVsMain(fits)
    mainName   <- paste(parts, "+", operators)
    fillDat    <- data.frame(modelName    = c(gettext("Null model"), mainName, paste(mainName, "+", jaspBase::gsubInteractionSymbol(paste0(parts, ":", operators)))),
                             comparisonBF = c(full[["bf"]], fullVsMain[["bf"]], 1),
                             error        = c(full[["error"]], fullVsMain[["error"]], NA) * 100)

    if (options[["estimationType"]] == "automatic") {
      bfText <- format(fullVsMain[["bf"]], digits = 3)
      if (fullVsMain[["bf"]] <= options[["bfFavorFull"]]) {
        table$addFootnote(gettextf("The model including only the main effects is used for estimation, because the BF<sub>10</sub> in favor of the full model (%1$s, error %2$s%%) does not exceed the cut-off of %3$s.",
                                   bfText, signif(100 * fullVsMain[["error"]], 2), options[["bfFavorFull"]]))
      } else {
        table$addFootnote(gettextf("The full model is used for estimation, because the BF<sub>10</sub> in favor of the full model (%1$s, error %2$s%%) exceeds the cut-off of %3$s.",
                                   bfText, signif(100 * fullVsMain[["error"]], 2), options[["bfFavorFull"]]))
      }
    }
  }

  table$setData(fillDat[order(-fillDat[["comparisonBF"]]), ])
  table$addFootnote(gettext("BF<sub>10</sub> compares the full model to the indicated model in each row."))
}

.msabVarCompTable <- function(jaspResults, draws, model, options, ready) {
  if (!is.null(jaspResults[["varCompTable"]]))
    return()

  table <- createJaspTable(title = gettext("Variance Components"))
  table$position <- 2
  table$dependOn(c(.msabComponentDeps, "RRTable", "report"))
  jaspResults[["varCompTable"]] <- table

  overtitle <- gettextf("95%% Credible Interval")
  table$addColumnInfo(name = "sourceName", title = gettext("Source"),         type = "string")
  table$addColumnInfo(name = "mean",       title = gettext("Mean"),           type = "number")
  table$addColumnInfo(name = "sd",         title = gettext("Std. Deviation"), type = "number")
  table$addColumnInfo(name = "lower",      title = gettext("Lower"),          type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper",      title = gettext("Upper"),          type = "number", overtitle = overtitle)

  if (!ready)
    return()

  fillDat <- .msabSummariseDraws(draws)[, c("sourceName", "mean", "sd", "lower", "upper")]
  .msabAddCommonFootnotes(table, fillDat[, -1], draws)
  table$setData(fillDat)

  table$addFootnote(gettext("Credible intervals are estimated based on the MCMC samples."))
  if (model == "main") {
    table$addFootnote(gettext("The components are based on the model only including the main effects."))
  } else {
    table$addFootnote(gettext("The components are based on the full model."))
  }
}

.msabContribTable <- function(jaspResults, draws, options, ready) {
  if (!is.null(jaspResults[["contribTable"]]))
    return()

  table <- createJaspTable(title = gettextf("%% Contribution to Total Variation"))
  table$position <- 3
  table$dependOn(c(.msabComponentDeps, "RRTable", "report"))
  jaspResults[["contribTable"]] <- table

  overtitle <- gettextf("95%% Credible Interval")
  table$addColumnInfo(name = "sourceName", title = gettext("Source"), type = "string")
  table$addColumnInfo(name = "mean",       title = gettext("Mean"),   type = "number")
  table$addColumnInfo(name = "lower",      title = gettext("Lower"),  type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper",      title = gettext("Upper"),  type = "number", overtitle = overtitle)

  if (!ready)
    return()

  fillDat <- .msabSummariseDraws(.msabTransformDraws(draws, options, "percContrib"))
  table$setData(fillDat[, c("sourceName", "mean", "lower", "upper")])
  table$addFootnote(gettext("Credible intervals are estimated based on the MCMC samples."))
}

.msabGaugeEvalTables <- function(jaspResults, draws, options, ready) {
  if (!is.null(jaspResults[["gaugeEvaluation"]]))
    return()

  container <- createJaspContainer(title = gettext("Gauge Evaluation"))
  container$position <- 4
  container$dependOn(c(.msabComponentDeps, .msabStudyVarDeps, .msabToleranceDeps, "RRTable", "report"))
  jaspResults[["gaugeEvaluation"]] <- container

  ### Standard deviation & study variation table
  stdTable <- createJaspTable(title = gettext("Standard Deviation & Study Variation"))
  stdTable$position <- 1
  container[["stdTable"]] <- stdTable

  stdTable$addColumnInfo(name = "sourceName",    title = gettext("Source"),                  type = "string")
  stdTable$addColumnInfo(name = "meansStd",      title = gettext("Mean<br>Std"),             type = "number")
  stdTable$addColumnInfo(name = "lowerStd",      title = gettext("Lower"),                   type = "number", overtitle = gettextf("95%% Credible Interval<br>Std"))
  stdTable$addColumnInfo(name = "upperStd",      title = gettext("Upper"),                   type = "number", overtitle = gettextf("95%% Credible Interval<br>Std"))
  stdTable$addColumnInfo(name = "meansStudyVar", title = gettext("Mean<br>Study Variation"), type = "number")
  stdTable$addColumnInfo(name = "lowerStudyVar", title = gettext("Lower"),                   type = "number", overtitle = gettextf("95%% Credible Interval<br>Study Variation"))
  stdTable$addColumnInfo(name = "upperStudyVar", title = gettext("Upper"),                   type = "number", overtitle = gettextf("95%% Credible Interval<br>Study Variation"))

  ### Percent study variation & percent tolerance table
  title <- if (options[["tolerance"]]) gettextf("%% Study Variation & %% Tolerance") else gettextf("%% Study Variation")
  percTable <- createJaspTable(title = title)
  percTable$position <- 2
  container[["percStudyVarTable"]] <- percTable

  percTable$addColumnInfo(name = "sourceName",     title = gettext("Source"),                      type = "string")
  percTable$addColumnInfo(name = "meansPercStudy", title = gettextf("Mean<br>%% Study Variation"), type = "number")
  percTable$addColumnInfo(name = "lowerPercStudy", title = gettext("Lower"),                       type = "number", overtitle = gettextf("95%% Credible Interval<br>%% Study Variation"))
  percTable$addColumnInfo(name = "upperPercStudy", title = gettext("Upper"),                       type = "number", overtitle = gettextf("95%% Credible Interval<br>%% Study Variation"))
  if (options[["tolerance"]]) {
    percTable$addColumnInfo(name = "meansPercTol", title = gettextf("Mean<br>%% Tolerance"), type = "number")
    percTable$addColumnInfo(name = "lowerPercTol", title = gettext("Lower"),                 type = "number", overtitle = gettextf("95%% Credible Interval<br>%% Tolerance"))
    percTable$addColumnInfo(name = "upperPercTol", title = gettext("Upper"),                 type = "number", overtitle = gettextf("95%% Credible Interval<br>%% Tolerance"))
  }

  if (!ready)
    return()

  stdDat <- cbind(.msabGaugeEvalColumns(draws, options, "sd"),
                  .msabGaugeEvalColumns(draws, options, "studyVar")[, -1])
  .msabAddCommonFootnotes(stdTable, stdDat[, -1], draws)
  stdTable$setData(stdDat)
  stdTable$addFootnote(gettext("Credible intervals are estimated based on the MCMC samples."))
  stdTable$addFootnote(gettextf("Study variation is calculated as std. dev. <span>&#215;</span> %s", round(.msabStudyVarMultiplier(options), 2)))

  nDistinct <- .msabDistinctCategories(draws)
  stdTable$addFootnote(gettextf("Number of distinct categories: %1$s (95%% CrI: %2$s, %3$s). It is computed as 1.41 <span>&#215;</span> &sigma;<sub>Part</sub> / &sigma;<sub>Gauge</sub> per posterior sample and, unlike the AIAG convention, not truncated to an integer.",
                                round(nDistinct[["mean"]], 2), round(nDistinct[["lower"]], 2), round(nDistinct[["upper"]], 2)))

  percDat <- .msabGaugeEvalColumns(draws, options, "percStudyVar")
  if (options[["tolerance"]])
    percDat <- cbind(percDat, .msabGaugeEvalColumns(draws, options, "percTol")[, -1])
  percTable$setData(percDat)
  percTable$addFootnote(gettext("Credible intervals are estimated based on the MCMC samples."))
}

.msabGaugeEvalColumns <- function(draws, options, type = c("sd", "studyVar", "percStudyVar", "percTol")) {
  type    <- match.arg(type)
  summary <- .msabSummariseDraws(.msabTransformDraws(draws, options, type))
  suffix  <- switch(type, "sd" = "Std", "studyVar" = "StudyVar", "percStudyVar" = "PercStudy", "percTol" = "PercTol")

  res <- summary[, c("sourceName", "mean", "lower", "upper")]
  colnames(res) <- c("sourceName", paste0(c("means", "lower", "upper"), suffix))
  return(res)
}

#### Plots ----
.msabPriorPlot <- function(jaspResults, options) {
  if (!is.null(jaspResults[["priorPlot"]]))
    return()

  container <- createJaspContainer(title = gettext("Prior Distribution"))
  container$position <- 5
  container$dependOn(c("rscalePrior", "priorPlot", "report"))
  jaspResults[["priorPlot"]] <- container

  gPrior <- createJaspPlot(title = gettext("g-prior"), width = 600, height = 320)
  container[["plot"]] <- gPrior

  # g ~ Inv-chi^2(nu = 1, tau = r^2), i.e. InvGamma(1/2, r^2/2)
  xUpper  <- extraDistr::qinvchisq(0.75, nu = 1, tau = options[["rscalePrior"]]^2)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, xUpper))

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(fun = extraDistr::dinvchisq,
                           args = list(nu = 1, tau = options[["rscalePrior"]]^2),
                           linewidth = 1) +
    ggplot2::scale_y_continuous(gettext("Density")) +
    ggplot2::scale_x_continuous("g", breaks = xBreaks, limits = c(0, xUpper)) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  gPrior$plotObject <- p
}

.msabVarCompPlot <- function(jaspResults, draws, options, ready, plotOnly = FALSE) {
  if (!plotOnly) {
    if (!is.null(jaspResults[["varCompPlot"]]))
      return()

    varCompPlot <- createJaspPlot(title = gettext("Components of variation"), width = 850, height = 500)
    varCompPlot$position <- 7
    varCompPlot$dependOn(c(.msabComponentDeps, .msabStudyVarDeps, .msabToleranceDeps, "varianceComponentsGraph", "report"))
    jaspResults[["varCompPlot"]] <- varCompPlot

    if (!ready)
      return()
  }

  # rows in the order expected by the plotting function
  ids <- if (options[["type3"]]) c("gauge", "repeatability", "part") else c("gauge", "repeatability", "reproducibility", "part")
  summaries <- lapply(c("percContrib", "percStudyVar", if (options[["tolerance"]]) "percTol"), function(type) {
    s <- .msabSummariseDraws(.msabTransformDraws(draws, options, type))
    s[match(ids, s[["source"]]), ]
  })

  errorbarDf <- do.call(rbind, lapply(summaries, function(s) s[, c("lower", "upper")]))

  # the plotting function expects a reproducibility row also for type 3 studies
  means <- lapply(summaries, function(s) if (options[["type3"]]) append(s[["mean"]], NA, after = 2) else s[["mean"]])

  p <- .gaugeVarCompGraph(means[[1]], means[[2]], if (options[["tolerance"]]) means[[3]] else NA,
                          errorbarDf = errorbarDf, Type3 = options[["type3"]])

  if (plotOnly)
    return(p)

  varCompPlot$plotObject <- p
}

.msabTrafficLightPlot <- function(jaspResults, draws, options, ready, plotOnly = FALSE) {
  if (!plotOnly) {
    if (!is.null(jaspResults[["trafficPlot"]]))
      return()

    container <- createJaspContainer(title = gettext("Traffic light chart"))
    container$position <- 15
    container$dependOn(c(.msabComponentDeps, .msabStudyVarDeps, .msabToleranceDeps, "trafficLightChart", "report"))
    jaspResults[["trafficPlot"]] <- container

    if (!ready) {
      container[["plot"]] <- createJaspPlot(width = 1000)
      return()
    }
  }

  gaugeSummary <- function(type) {
    s <- .msabSummariseDraws(.msabTransformDraws(draws, options, type))
    s[s[["source"]] == "gauge", ]
  }

  percStudyVar <- gaugeSummary("percStudyVar")
  studyVarCi   <- data.frame(lower = percStudyVar[["lower"]], upper = percStudyVar[["upper"]])

  if (!options[["tolerance"]]) {
    p <- .trafficplot(StudyVar = percStudyVar[["mean"]], ToleranceUsed = FALSE, ToleranceVar = 0,
                      options = options, ready = TRUE, StudyVarCi = studyVarCi, ggPlot = plotOnly)
  } else {
    percTol <- gaugeSummary("percTol")
    p <- .trafficplot(StudyVar = percStudyVar[["mean"]], ToleranceUsed = TRUE, ToleranceVar = percTol[["mean"]],
                      options = options, ready = TRUE, StudyVarCi = studyVarCi,
                      TolCi = data.frame(lower = percTol[["lower"]], upper = percTol[["upper"]]),
                      ggPlot = plotOnly)
  }

  if (plotOnly)
    return(p)

  container[["trafficPlot"]] <- p
}

.msabContourPlot <- function(jaspResults, data, draws, options, vars, ready) {
  if (!is.null(jaspResults[["contourPlot"]]))
    return()

  container <- createJaspContainer(title = gettext("Contour Plot"))
  container$position <- 16
  container$dependOn(c(.msabComponentDeps, "contourPlot", "contourLSL", "contourUSL", "report"))
  jaspResults[["contourPlot"]] <- container

  risksTable <- createJaspTable(title = gettext("Producer's (\u03b4) and Consumer's (\u03b2) Risk"))
  risksTable$position <- 1
  container[["table"]] <- risksTable

  risksTable$addColumnInfo(name = "risks", title = gettext("Risk"),  type = "string")
  risksTable$addColumnInfo(name = "means", title = gettext("Mean"),  type = "number")
  risksTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = gettextf("95%% Credible Interval"))
  risksTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = gettextf("95%% Credible Interval"))

  plot <- createJaspPlot(width = 600, height = 600)
  plot$position <- 2
  container[["plot"]] <- plot

  if (!ready)
    return()

  LSL <- options[["contourLSL"]]
  USL <- options[["contourUSL"]]
  mu  <- mean(data[["long"]][[vars[["measurementLong"]]]])

  # bivariate normal of the true value (variance: part) and the measurement (variance: total)
  covMat  <- matrix(c(mean(draws[["part"]]), mean(draws[["part"]]),
                      mean(draws[["part"]]), mean(draws[["total"]])), nrow = 2)
  ellipse <- try(as.data.frame(ellipse::ellipse(covMat, centre = c(mu, mu), level = 0.95)))

  if (isTryError(ellipse)) {
    plot$setError(gettextf("Failed to calculate contour: %s", .rmNewLine(ellipse)))
  } else {
    colnames(ellipse) <- c("x", "y")
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LSL, USL, ellipse[["x"]]))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LSL, USL, ellipse[["y"]]))

    plot$plotObject <- ggplot2::ggplot(ellipse, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_vline(xintercept = c(LSL, USL), linetype = "dashed", color = "black", linewidth = 1) +
      ggplot2::geom_hline(yintercept = c(LSL, USL), linetype = "dashed", color = "black", linewidth = 1) +
      ggplot2::geom_path(alpha = 0.5, colour = "steelblue", linewidth = 1) +
      ggplot2::scale_x_continuous(name = gettext("True Value"), breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = gettext("Measurement"), breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::coord_equal() +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe(sides = "bl")
  }

  risks <- .msabRisks(draws, mu, LSL, USL)
  if (isTryError(risks)) {
    risksTable$setError(gettextf("Risks could not be computed: %s<br>Try adjusting the specification limits.", .rmNewLine(risks)))
    return()
  }

  risksTable$setData(risks)
  risksTable$addFootnote(gettextf("Risks are computed for the specification limits LSL = %1$s and USL = %2$s. Make sure these match the specification limits of the process.",
                                  LSL, USL))
}

# producer's risk (delta): P(measurement out of spec | true value in spec)
# consumer's risk (beta):  P(measurement in spec | true value out of spec)
.msabRisks <- function(draws, mu, LSL, USL, maxDraws = 8000) {
  # thin the draws: the Monte Carlo error on the mean and the 95% CrI is negligible
  idx <- unique(round(seq(1, nrow(draws), length.out = min(maxDraws, nrow(draws)))))
  sdP <- sqrt(draws[["part"]][idx])
  sdT <- sqrt(draws[["total"]][idx])

  inSpec <- function(sd) pnorm(USL, mu, sd) - pnorm(LSL, mu, sd)
  pTrue  <- inSpec(sdP) # P(true value in spec)
  pMeas  <- inSpec(sdT) # P(measurement in spec)

  # P(true value in spec and measurement in spec)
  pBoth <- try(vapply(seq_along(idx), function(i) {
    covMat <- matrix(c(sdP[i]^2, sdP[i]^2, sdP[i]^2, sdT[i]^2), nrow = 2)
    mvtnorm::pmvnorm(lower = c(LSL, LSL), upper = c(USL, USL), mean = c(mu, mu), sigma = covMat)[1]
  }, numeric(1)))

  if (isTryError(pBoth))
    return(pBoth)

  delta <- (pTrue - pBoth) / pTrue
  beta  <- (pMeas - pBoth) / (1 - pTrue)

  if (anyNA(delta) || anyNA(beta) || any(!is.finite(c(delta, beta)))) {
    res <- gettext("Non-finite values introduced during calculation.")
    class(res) <- "try-error"
    return(res)
  }

  data.frame(risks = c("\u03b4", "\u03b2"),
             means = c(mean(delta), mean(beta)),
             lower = c(quantile(delta, 0.025, names = FALSE), quantile(beta, 0.025, names = FALSE)),
             upper = c(quantile(delta, 0.975, names = FALSE), quantile(beta, 0.975, names = FALSE)))
}

.msabRChart <- function(jaspResults, data, vars, ready) {
  if (!is.null(jaspResults[["rChart"]]))
    return()

  container <- createJaspContainer(gettext("Range chart by operator"))
  container$position <- 9
  container$dependOn(c(.msabDataDeps, "rChart", "report"))
  jaspResults[["rChart"]] <- container
  container[["plot"]] <- createJaspPlot(width = 1200, height = 500)

  if (!ready)
    return()

  chart <- .msabControlChart(data[["wide"]], vars, "R")
  container[["plot"]]$plotObject <- chart$plotObject
  container[["table"]] <- chart$table
}

.msabXbarChart <- function(jaspResults, data, vars, ready) {
  if (!is.null(jaspResults[["xBarChart"]]))
    return()

  container <- createJaspContainer(gettext("Average chart by operator"))
  container$position <- 10
  container$dependOn(c(.msabDataDeps, "xBarChart", "report"))
  jaspResults[["xBarChart"]] <- container
  container[["plot"]] <- createJaspPlot(width = 1200, height = 500)

  if (!ready)
    return()

  chart <- .msabControlChart(data[["wide"]], vars, "xBar")
  container[["plot"]]$plotObject <- chart$plotObject
  container[["table"]] <- chart$table
}

.msabControlChart <- function(dataWide, vars, plotType = c("R", "xBar")) {
  plotType  <- match.arg(plotType)
  operators <- vars[["operators"]]
  .controlChart(dataset                   = dataWide[c(vars[["measurementsWide"]], operators)],
                plotType                  = plotType,
                ruleList                  = .getRuleListSubgroupCharts(list(testSet = "jaspDefault"), plotType),
                xBarSdType                = "r",
                stages                    = operators,
                xAxisLabels               = dataWide[[vars[["parts"]]]][order(dataWide[[operators]])],
                stagesSeparateCalculation = FALSE)
}

.msabScatterPlot <- function(jaspResults, data, options, vars, ready) {
  if (!is.null(jaspResults[["gaugeScatterOperators"]]))
    return()

  plot <- createJaspPlot(title = gettext("Matrix plot for operators"), width = 700, height = 700)
  plot$position <- 11
  plot$dependOn(c(.msabDataDeps, "scatterPlot", "scatterPlotFitLine", "scatterPlotOriginLine", "report"))
  jaspResults[["gaugeScatterOperators"]] <- plot

  if (!ready)
    return()

  plot$plotObject <- .gaugeScatterPlotOperators(jaspResults, data[["wide"]], vars[["measurementsWide"]],
                                                vars[["parts"]], vars[["operators"]], options, ready)$plotObject
}

.msabMeasureByPartPlot <- function(jaspResults, data, options, vars, ready) {
  if (!is.null(jaspResults[["gaugeByPart"]]))
    return()

  plot <- createJaspPlot(title = gettext("Measurements by part"), width = 700, height = 300)
  plot$position <- 12
  plot$dependOn(c(.msabDataDeps, "partMeasurementPlot", "partMeasurementPlotAllValues", "report"))
  jaspResults[["gaugeByPart"]] <- plot

  if (!ready)
    return()

  plot$plotObject <- .gaugeByPartGraphPlotObject(data[["wide"]], vars[["measurementsWide"]], vars[["parts"]], vars[["operators"]],
                                                 displayAll = options[["partMeasurementPlotAllValues"]])
}

.msabMeasureByOperatorPlot <- function(jaspResults, data, options, vars, ready) {
  if (!is.null(jaspResults[["gaugeByOperator"]]))
    return()

  plot <- createJaspPlot(title = gettext("Measurements by operator"), width = 600, height = 600)
  plot$position <- 13
  plot$dependOn(c(.msabDataDeps, "operatorMeasurementPlot", "report"))
  jaspResults[["gaugeByOperator"]] <- plot

  if (!ready)
    return()

  plot$plotObject <- .gaugeByOperatorGraphPlotObject(data[["wide"]], vars[["measurementsWide"]], vars[["parts"]], vars[["operators"]],
                                                     options, Type3 = options[["type3"]])
}

.msabPartByOperatorPlot <- function(jaspResults, data, options, vars, ready) {
  if (!is.null(jaspResults[["gaugeByInteraction"]]))
    return()

  plot <- createJaspPlot(title = gettext("Part by operator interaction"), width = 700, height = 400)
  plot$position <- 14
  plot$dependOn(c(.msabDataDeps, "partByOperatorMeasurementPlot", "report"))
  jaspResults[["gaugeByInteraction"]] <- plot

  if (!ready)
    return()

  plot$plotObject <- .gaugeByInteractionGraphPlotFunction(data[["wide"]], vars[["measurementsWide"]], vars[["parts"]], vars[["operators"]],
                                                          options, Type3 = options[["type3"]], ggPlot = TRUE)
}

#### Posterior distributions ----
.msabPosteriors <- function(jaspResults, samples, draws, options, ready) {
  if (!is.null(jaspResults[["posteriorSummaries"]]))
    return()

  container <- createJaspContainer(title = gettext("Posterior Distributions"))
  container$position <- 8
  container$dependOn(c(.msabComponentDeps, .msabStudyVarDeps, .msabToleranceDeps, .msabPosteriorDeps, "posteriorPlot", "report"))
  jaspResults[["posteriorSummaries"]] <- container

  table <- createJaspTable(title = gettext("Posterior Summary"))
  table$position <- 1
  container[["postSummary"]] <- table

  cutOffMode <- options[["posteriorCiType"]] == "custom" && options[["customCiType"]] == "customCiCutOff"
  cutOff     <- options[["posteriorCiCutOff"]]

  table$addColumnInfo(name = "parameter", title = gettext("Source"), type = "string")
  if (options[["posteriorPointEstimate"]]) {
    pointTitle <- switch(options[["posteriorPointEstimateType"]],
                         "mean"   = gettext("Mean"),
                         "mode"   = gettext("Mode"),
                         "median" = gettext("Median"))
    table$addColumnInfo(name = "pointEstimate", title = pointTitle, type = "number")
  }
  if (options[["posteriorCi"]]) {
    if (cutOffMode) {
      table$addColumnInfo(name = "belowCutOff", title = gettextf("p(x < %s | data)", cutOff), type = "number")
    } else {
      mass      <- if (options[["posteriorCiType"]] == "custom") options[["posteriorCiUpper"]] - options[["posteriorCiLower"]] else options[["posteriorCiMass"]]
      overtitle <- gettextf("%s%% Credible Interval", round(100 * mass, 1))
      table$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number", overtitle = overtitle)
      table$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number", overtitle = overtitle)
    }
  }

  if (!ready) {
    container[["plot"]] <- createJaspPlot(width = 600, height = 320)
    return()
  }

  posteriorSamples <- .msabPosteriorSamples(samples, draws, options)
  ids              <- colnames(posteriorSamples)
  labels           <- if (options[["posteriorPlotType"]] == "var") .msabParameterLabels(ids) else .msabSourceLabels(ids)

  if (options[["setSeed"]])
    set.seed(options[["seed"]])

  dist      <- .msabDistFunctions(options[["distType"]])
  bounded   <- .msabBoundedPercentage(options)
  rows      <- vector("list", length(ids))
  anyFailed <- FALSE

  for (i in seq_along(ids)) {
    x   <- posteriorSamples[, ids[i]]
    # both distributions have positive support; truncated draws (historical sd) form a point mass at the bound
    fit <- try(dist$fit(x[x > 0 & (!bounded | x < 100)], bounded))

    plot <- createJaspPlot(title = labels[i], width = 600, height = 320)
    plot$position <- i + 1
    container[[ids[i]]] <- plot

    if (isTryError(fit)) {
      anyFailed <- TRUE
      rows[[i]] <- data.frame(parameter = labels[i])
      plot$setError(gettextf("The %1$s distribution could not be fit to the samples: %2$s", .msabDistName(options[["distType"]]), .rmNewLine(fit)))
      next
    }

    stats     <- .msabPosteriorStats(fit, dist, options)
    rows[[i]] <- data.frame(parameter = labels[i], as.list(stats))

    p <- try(.msabPosteriorPlot(x, fit, dist, stats, labels[i], bounded, options))
    if (isTryError(p)) {
      plot$setError(.rmNewLine(p))
    } else {
      plot$plotObject <- p
    }
  }

  fillDat <- .msabRbindFill(rows)

  if (options[["posteriorCi"]]) {
    if (cutOffMode) {
      checked <- .checkProbabilityEstimates(fillDat[["belowCutOff"]])
      fillDat[["belowCutOff"]] <- checked[["estimates"]]
      if (checked[["large"]])
        table$addFootnote(gettextf("p(x < %s | data) could not be calculated for some sources. The cut-off is probably too large or too small.", cutOff),
                          symbol = gettext("<b>Warning:</b>"))
      if (checked[["negative"]])
        table$addFootnote(gettextf("Some estimates of p(x < %s | data) were negative and, therefore, removed. Try a different cut-off or distribution.", cutOff),
                          symbol = gettext("<b>Warning:</b>"))
    } else {
      table$addFootnote(gettext("Credible intervals are estimated based on the distribution fit to the MCMC samples."))
    }
  }

  if (options[["posteriorPlotType"]] != "var")
    .msabAddCommonFootnotes(table, fillDat[, -1, drop = FALSE], draws)

  if (anyFailed)
    table$addFootnote(gettextf("The %s distribution could not be fit to the samples of some sources. Try selecting another distribution.", .msabDistName(options[["distType"]])),
                      symbol = gettext("<b>Warning:</b>"))

  table$setData(fillDat)
}

# samples used for the posterior plots, named by internal ids
.msabPosteriorSamples <- function(samples, draws, options) {
  if (options[["posteriorPlotType"]] == "var")
    return(.arrayToMat(samples))

  mat <- .msabTransformDraws(draws, options, options[["posteriorPlotType"]])

  # constant columns (e.g. total variation in %Contribution) have no distribution
  mat[, apply(mat, 2, function(col) any(col != col[1])), drop = FALSE]
}

# percentages that cannot exceed 100%
.msabBoundedPercentage <- function(options) {
  options[["posteriorPlotType"]] %in% c("percContrib", "percStudyVar") && options[["processVariationReference"]] != "historicalSd"
}

.msabDistName <- function(distType) {
  switch(distType,
         "gig"     = gettext("generalized inverse Gaussian"),
         "metalog" = gettext("metalog"))
}

# fit, density, cdf and quantile functions per distribution
.msabDistFunctions <- function(distType) {
  switch(distType,
         "gig" = list(
           # the GIG quantile function is slow and unstable, so quantiles are based on random draws
           fit       = function(x, bounded) {
             param <- GeneralizedHyperbolic::gigFit(x)$param
             list(param = param, randData = GeneralizedHyperbolic::rgig(1e5, param = param))
           },
           densityAt = function(fit, x) GeneralizedHyperbolic::dgig(x, param = fit$param),
           grid      = function(fit) {
             x <- quantile(fit$randData, seq(0.0005, 0.9995, length.out = 500), names = FALSE)
             data.frame(x = x, y = GeneralizedHyperbolic::dgig(x, param = fit$param))
           },
           p         = function(fit, q) mean(fit$randData < q),
           q         = function(fit, p) quantile(fit$randData, p, names = FALSE),
           hpd       = function(fit, mass) HDInterval::hdi(fit$randData, credMass = mass),
           mean      = function(fit) GeneralizedHyperbolic::gigMean(param = fit$param),
           mode      = function(fit) GeneralizedHyperbolic::gigMode(param = fit$param)
         ),
         "metalog" = list(
           fit       = function(x, bounded) {
             if (bounded) {
               rmetalog::metalog(x, bounds = c(0, 100), boundedness = "b", term_lower_bound = 5, term_limit = 5)
             } else {
               rmetalog::metalog(x, bounds = 0, boundedness = "sl", term_lower_bound = 5, term_limit = 5)
             }
           },
           # density from the quantile function (dmetalog is slow): f(Q(p)) = 1 / Q'(p)
           densityAt = function(fit, x) {
             p <- rmetalog::pmetalog(m = fit, q = x, term = fit$params$term_limit)
             1 / .msabMetalogQuantileDerivative(fit, pmin(pmax(p, 1e-5), 1 - 1e-5))
           },
           grid      = function(fit) {
             p <- seq(0.0005, 0.9995, length.out = 500)
             data.frame(x = .msabMetalogQuantile(fit, p), y = 1 / .msabMetalogQuantileDerivative(fit, p))
           },
           p         = function(fit, q) rmetalog::pmetalog(m = fit, q = q, term = fit$params$term_limit),
           q         = function(fit, p) .msabMetalogQuantile(fit, p),
           hpd       = function(fit, mass) HDInterval::hdi(function(p) .msabMetalogQuantile(fit, p), credMass = mass),
           mean      = function(fit) integrate(function(p) .msabMetalogQuantile(fit, p), lower = 0, upper = 1)$value,
           mode      = function(fit) {
             p <- optimize(function(p) 1 / .msabMetalogQuantileDerivative(fit, p), interval = c(0.0005, 0.9995), maximum = TRUE)$maximum
             .msabMetalogQuantile(fit, p)
           }
         ))
}

.msabMetalogQuantile <- function(fit, p) {
  rmetalog::qmetalog(m = fit, y = p, term = fit$params$term_limit)
}

.msabMetalogQuantileDerivative <- function(fit, p, h = 1e-6) {
  (.msabMetalogQuantile(fit, p + h) - .msabMetalogQuantile(fit, p - h)) / (2 * h)
}

.msabPosteriorStats <- function(fit, dist, options) {
  safe  <- function(expr, n = 1) { res <- try(expr, silent = TRUE); if (isTryError(res) || length(res) != n) rep(NA_real_, n) else as.numeric(res) }
  stats <- list()

  if (options[["posteriorPointEstimate"]]) {
    stats[["pointEstimate"]] <- switch(options[["posteriorPointEstimateType"]],
                                       "mean"   = safe(dist$mean(fit)),
                                       "median" = safe(dist$q(fit, 0.5)),
                                       "mode"   = safe(dist$mode(fit)))
  }

  if (options[["posteriorCi"]]) {
    ciType <- options[["posteriorCiType"]]
    if (ciType == "custom" && options[["customCiType"]] == "customCiCutOff") {
      stats[["belowCutOff"]] <- safe(suppressWarnings(dist$p(fit, options[["posteriorCiCutOff"]])))
    } else {
      mass <- options[["posteriorCiMass"]]
      ci <- switch(ciType,
                   "central" = safe(dist$q(fit, c((1 - mass) / 2, (1 + mass) / 2)), 2),
                   "HPD"     = safe(dist$hpd(fit, mass), 2),
                   "custom"  = safe(dist$q(fit, c(options[["posteriorCiLower"]], options[["posteriorCiUpper"]])), 2))
      stats[["ciLower"]] <- ci[1]
      stats[["ciUpper"]] <- ci[2]
    }
  }

  return(stats)
}

.msabPosteriorPlot <- function(x, fit, dist, stats, label, bounded, options) {
  # x-axis: up to the 99% quantile (and the CrI), or the full range for percentages bounded at 100
  xUpper  <- if (bounded) 100 else max(dist$q(fit, 0.99), stats[["ciUpper"]], na.rm = TRUE)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, xUpper))
  xLims   <- range(xBreaks)

  # density evaluated once on a grid within the support
  densityDf <- dist$grid(fit)
  densityDf <- densityDf[is.finite(densityDf[["y"]]) & densityDf[["x"]] >= xLims[1] & densityDf[["x"]] <= xLims[2], ]

  p       <- ggplot2::ggplot()
  maxDens <- max(densityDf[["y"]])

  if (options[["posteriorHistogram"]]) {
    breaks  <- seq(xLims[1], xLims[2], length.out = 31)
    counts  <- tabulate(findInterval(x, breaks, rightmost.closed = TRUE), nbins = length(breaks) - 1)
    histDf  <- data.frame(x = breaks, y = c(counts, counts[length(counts)]) / (length(x) * diff(breaks)[1]))
    maxDens <- max(maxDens, histDf[["y"]])
    p <- p + ggplot2::geom_step(data = histDf, mapping = ggplot2::aes(x = x, y = y), direction = "hv", linewidth = 1)
  }

  if (options[["posteriorCi"]]) {
    shade <- if (is.null(stats[["belowCutOff"]])) {
      c(stats[["ciLower"]], stats[["ciUpper"]])
    } else if (!is.na(stats[["belowCutOff"]])) {
      c(xLims[1], options[["posteriorCiCutOff"]])
    } else {
      NA
    }

    if (!anyNA(shade)) {
      inside <- densityDf[densityDf[["x"]] > shade[1] & densityDf[["x"]] < shade[2], ]
      edges  <- data.frame(x = shade, y = dist$densityAt(fit, shade))
      areaDf <- rbind(edges[1, ], inside, edges[2, ])
      areaDf <- areaDf[is.finite(areaDf[["y"]]), ]
      p <- p + ggplot2::geom_area(data = areaDf, mapping = ggplot2::aes(x = x, y = y), fill = "grey")
    }
  }

  p <- p + ggplot2::geom_line(data = densityDf, mapping = ggplot2::aes(x = x, y = y), linewidth = 1)

  if (options[["posteriorPointEstimate"]] && !is.na(stats[["pointEstimate"]])) {
    pointDf <- data.frame(x = stats[["pointEstimate"]], y = dist$densityAt(fit, stats[["pointEstimate"]]))
    p <- p + ggplot2::geom_point(data = pointDf, mapping = ggplot2::aes(x = x, y = y),
                                 shape = 21, size = 4, fill = "grey", stroke = 1)
  }

  xLab <- switch(options[["posteriorPlotType"]],
                 "var"          = gettextf("Variance (%s)", label),
                 "percContrib"  = gettextf("%% Contribution"),
                 "percStudyVar" = gettextf("%% Study Variation"),
                 "percTol"      = gettextf("%% Tolerance"))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, maxDens))

  p +
    ggplot2::scale_x_continuous(name = xLab, breaks = xBreaks, limits = xLims) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe(sides = "bl")
}

.msabRbindFill <- function(rows) {
  cols <- unique(unlist(lapply(rows, colnames)))
  rows <- lapply(rows, function(r) { r[setdiff(cols, colnames(r))] <- NA; r[cols] })
  do.call(rbind, rows)
}

#### MCMC diagnostics ----
.msabDiagnostics <- function(jaspResults, samples, options, ready) {
  if (!is.null(jaspResults[["mcmcDiagnostics"]]))
    return()

  container <- createJaspContainer(title = gettext("MCMC diagnostics"))
  container$position <- 6
  container$dependOn(c(.msabModelDeps, "diagnosticsTable", "diagnosticsPlots", "diagnosticsPlotType", "report"))
  jaspResults[["mcmcDiagnostics"]] <- container

  if (options[["diagnosticsTable"]]) {
    table <- createJaspTable()
    table$position <- 1
    container[["table"]] <- table

    table$addColumnInfo(name = "parameter",         title = gettext("Parameter"),   type = "string")
    table$addColumnInfo(name = "essBulk",           title = gettext("ESS (Bulk)"),  type = "number")
    table$addColumnInfo(name = "essTail",           title = gettext("ESS (Tail)"),  type = "number")
    table$addColumnInfo(name = "rhat",              title = gettext("Rhat"),        type = "number")
    table$addColumnInfo(name = "mcseMean",          title = gettext("MCSE (Mean)"), type = "number")
    table$addColumnInfo(name = "mcseQuantileLower", title = "0.025",                type = "number", overtitle = gettext("MCSE (Quantiles)"))
    table$addColumnInfo(name = "mcseQuantileUpper", title = "0.975",                type = "number", overtitle = gettext("MCSE (Quantiles)"))

    if (ready) {
      fillDat <- .msabDiagnosticsTableData(samples)
      .msabAddCommonFootnotes(table, fillDat[, -1], NULL)
      if (anyNA(fillDat))
        table$addFootnote(gettext("Some diagnostics could not be calculated. Results might be unreliable."),
                          symbol = gettext("<b>Warning:</b>"))
      table$setData(fillDat)
    }
  }

  if (!options[["diagnosticsPlots"]])
    return()

  if (!ready) {
    container[["plot"]] <- createJaspPlot(width = 600, height = 320)
    return()
  }

  ids    <- dimnames(samples)[[3]]
  labels <- .msabParameterLabels(ids)
  plotFun <- switch(options[["diagnosticsPlotType"]],
                    "trace"   = .msabTracePlot,
                    "autocor" = .msabAutocorPlot,
                    "density" = .msabDensityDiagnosticsPlot)

  for (i in seq_along(ids)) {
    plot <- createJaspPlot(title = labels[i], width = 600, height = 320)
    plot$position <- i + 1
    container[[ids[i]]] <- plot

    p <- try(plotFun(.msabChainMatrix(samples, ids[i]), labels[i]))
    if (isTryError(p)) {
      plot$setError(.rmNewLine(p))
    } else {
      plot$plotObject <- p
    }
  }
}

.msabDiagnosticsTableData <- function(samples) {
  ids <- dimnames(samples)[[3]]
  res <- lapply(ids, function(id) {
    x   <- .msabChainMatrix(samples, id)
    mcq <- posterior::mcse_quantile(x, probs = c(0.025, 0.975))
    data.frame(essBulk           = posterior::ess_bulk(x),
               essTail           = posterior::ess_tail(x),
               rhat              = posterior::rhat(x),
               mcseMean          = posterior::mcse_mean(x),
               mcseQuantileLower = mcq[[1]],
               mcseQuantileUpper = mcq[[2]])
  })
  cbind(parameter = .msabParameterLabels(ids, html = TRUE), do.call(rbind, res))
}

# iterations x chains matrix of one parameter
.msabChainMatrix <- function(samples, id) {
  matrix(samples[, , id], ncol = dim(samples)[2])
}

.msabChainsLong <- function(x) {
  data.frame(iteration = rep(seq_len(nrow(x)), ncol(x)),
             value     = as.vector(x),
             chain     = factor(rep(seq_len(ncol(x)), each = nrow(x))))
}

.msabTracePlot <- function(x, label) {
  df      <- .msabChainsLong(x)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(df[["iteration"]]))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(df[["value"]]))

  ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = value, colour = chain)) +
    ggplot2::geom_line(linewidth = 0.3) +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain")) +
    ggplot2::scale_x_continuous(name = gettext("Iteration"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettextf("Variance (%s)", label), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
}

.msabAutocorPlot <- function(x, label) {
  df <- do.call(rbind, lapply(seq_len(ncol(x)), function(i) {
    a <- acf(x[, i], lag.max = 20, plot = FALSE)
    data.frame(lag = as.vector(a$lag), acf = as.vector(a$acf), chain = i)
  }))
  df[["chain"]] <- factor(df[["chain"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, df[["acf"]]))

  ggplot2::ggplot(df, ggplot2::aes(x = lag, y = acf, colour = chain)) +
    ggplot2::geom_hline(yintercept = 0, alpha = 0.5) +
    ggplot2::geom_line() +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain")) +
    ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = seq(0, 20, 5), limits = c(0, 20)) +
    ggplot2::scale_y_continuous(name = gettext("Autocorrelation"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
}

.msabDensityDiagnosticsPlot <- function(x, label) {
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(quantile(x, c(0.001, 0.999), names = FALSE))
  xLims   <- range(xBreaks)

  df <- do.call(rbind, lapply(seq_len(ncol(x)), function(i) {
    d <- density(x[, i], from = xLims[1], to = xLims[2], n = 512)
    data.frame(x = d$x, y = d$y, chain = i)
  }))
  df[["chain"]] <- factor(df[["chain"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, df[["y"]]))

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = chain)) +
    ggplot2::geom_line() +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain")) +
    ggplot2::scale_x_continuous(name = gettextf("Variance (%s)", label), breaks = xBreaks, limits = xLims) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
}

#### Report ----
.msabReport <- function(jaspResults, data, draws, options, vars, ready) {
  if (!is.null(jaspResults[["report"]]))
    return()

  type3      <- options[["type3"]]
  components <- c(gaugeTable            = options[["reportGaugeTable"]],
                  variationComponents   = options[["reportVariationComponents"]],
                  measurementsByPart    = options[["reportMeasurementsByPartPlot"]],
                  rChart                = options[["reportRChartByOperator"]] && !type3,
                  measurementsOperator  = options[["reportMeasurementsByOperatorPlot"]] && !type3,
                  averageChart          = options[["reportAverageChartByOperator"]] && !type3,
                  partByOperator        = options[["reportPartByOperatorPlot"]] && !type3,
                  trafficLight          = options[["reportTrafficLightChart"]],
                  metaData              = options[["reportMetaData"]])
  nElements <- sum(components)

  reportPlot <- createJaspPlot(title = gettext("Gauge r&R report"), width = 1250, height = max(1, ceiling(nElements / 2)) * 500)
  reportPlot$dependOn(c(.msabComponentDeps, .msabStudyVarDeps, .msabToleranceDeps, .msabReportDeps))
  jaspResults[["report"]] <- reportPlot

  if (nElements == 0) {
    reportPlot$setError(gettext("No report components selected."))
    return()
  }

  if (!ready)
    return()

  dataWide     <- data[["wide"]]
  measurements <- vars[["measurementsWide"]]
  parts        <- vars[["parts"]]
  operators    <- vars[["operators"]]

  plots <- list()
  if (components[["variationComponents"]])
    plots[[length(plots) + 1]] <- .msabVarCompPlot(jaspResults, draws, options, ready, plotOnly = TRUE)
  if (components[["measurementsByPart"]])
    plots[[length(plots) + 1]] <- .gaugeByPartGraphPlotObject(dataWide, measurements, parts, operators, displayAll = FALSE)
  if (components[["rChart"]])
    plots[[length(plots) + 1]] <- .msabControlChart(dataWide, vars, "R")$plotObject
  if (components[["measurementsOperator"]])
    plots[[length(plots) + 1]] <- .gaugeByOperatorGraphPlotObject(dataWide, measurements, parts, operators, options, Type3 = type3)
  if (components[["averageChart"]])
    plots[[length(plots) + 1]] <- .msabControlChart(dataWide, vars, "xBar")$plotObject
  if (components[["partByOperator"]])
    plots[[length(plots) + 1]] <- .gaugeByInteractionGraphPlotFunction(dataWide, measurements, parts, operators, options, Type3 = type3, ggPlot = TRUE)
  if (components[["trafficLight"]]) {
    trafficPlots <- .msabTrafficLightPlot(jaspResults, draws, options, ready, plotOnly = TRUE)
    if (options[["tolerance"]]) {
      plots[[length(plots) + 1]] <- trafficPlots$p1
      plots[[length(plots) + 1]] <- trafficPlots$p2
    } else {
      plots[[length(plots) + 1]] <- trafficPlots
    }
  }

  tables      <- NULL
  tableTitles <- ""
  if (components[["gaugeTable"]]) {
    tables      <- .msabReportTables(draws, options)
    tableTitles <- if (options[["tolerance"]]) list(list(gettext("Gauge evaluation"), ""), "") else list(list(gettext("Gauge evaluation"), ""))
  }

  reportPlot$plotObject <- .qcReport(text = if (options[["reportMetaData"]]) .getReportMetaData(options) else NULL,
                                     plots = plots, tables = tables, textMaxRows = 8,
                                     tableTitles = tableTitles, reportTitle = .getReportTitle(options), tableSize = 6)
}

.getReportTitle <- function(options) {
  if (!options[["reportTitle"]])
    return("")
  if (options[["reportTitleText"]] == "") gettext("Gauge r&R report") else options[["reportTitleText"]]
}

.getReportMetaData <- function(options) {
  text <- c()
  if (options[["reportPartName"]])       text <- c(text, gettextf("Part name: %s", options[["reportPartNameText"]]))
  if (options[["reportGaugeName"]])      text <- c(text, gettextf("Gauge name: %s", options[["reportGaugeNameText"]]))
  if (options[["reportCharacteristic"]]) text <- c(text, gettextf("Characteristic: %s", options[["reportCharacteristicText"]]))
  if (options[["reportGaugeNumber"]])    text <- c(text, gettextf("Gauge number: %s", options[["reportGaugeNumberText"]]))
  if (options[["reportTolerance"]])      text <- c(text, gettextf("Tolerance: %s", options[["reportToleranceText"]]))
  if (options[["reportLocation"]])       text <- c(text, gettextf("Location: %s", options[["reportLocationText"]]))
  if (options[["reportPerformedBy"]])    text <- c(text, gettextf("Performed by: %s", options[["reportPerformedByText"]]))
  if (options[["reportDate"]])           text <- c(text, gettextf("Date: %s", options[["reportDateText"]]))
  return(text)
}

.msabReportTables <- function(draws, options) {
  types    <- c("sd", "studyVar", "percStudyVar", if (options[["tolerance"]]) "percTol")
  colNames <- c(gettext("Source"), gettext("Std. dev."), gettext("Study variation"), gettextf("%%Study variation"),
                if (options[["tolerance"]]) gettextf("%%Tolerance"))

  columns <- lapply(types, function(type) {
    s <- .msabSummariseDraws(.msabTransformDraws(draws, options, type))
    .msabFormatEstimate(s[["mean"]], s[["lower"]], s[["upper"]])
  })
  out <- data.frame(.msabSourceLabels(colnames(draws)), columns)
  colnames(out) <- colNames

  nDistinct   <- .msabDistinctCategories(draws)
  nDistinctDf <- data.frame(gettext("Number of distinct categories"),
                            .msabFormatEstimate(nDistinct[["mean"]], nDistinct[["lower"]], nDistinct[["upper"]]))
  colnames(nDistinctDf) <- NULL

  # split data frame as it would otherwise be cut off in the report
  if (options[["tolerance"]]) {
    tolOut <- out[, c(1, ncol(out))]
    out    <- out[, -ncol(out)]
    return(list(list(out, nDistinctDf), tolOut))
  }

  return(list(list(out, nDistinctDf)))
}

.msabFormatEstimate <- function(mean, lower, upper) {
  ifelse(is.na(lower), as.character(round(mean, 2)),
         paste0(round(mean, 2), " (", round(lower, 2), ", ", round(upper, 2), ")"))
}

#### Helpers ----
.checkProbabilityEstimates <- function(estimates) {
  negative <- !is.na(estimates) & estimates < 0
  list(large     = anyNA(estimates),
       negative  = any(negative),
       estimates = replace(estimates, negative, NA))
}

.hasExtremeValues <- function(df) {
  nums <- suppressWarnings(as.numeric(unlist(df, use.names = FALSE)))
  nums <- nums[!is.na(nums)]
  list(large = any(abs(nums) > 1e50),
       small = any(abs(nums) < 1e-50 & nums != 0))
}

.rmNewLine <- function(msg) {
  gsub("\\n", "", gsub("\\n ", "", msg))
}
