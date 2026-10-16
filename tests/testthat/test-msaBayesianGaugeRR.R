context("[Quality Control] Bayesian Gauge r&R")
.numDecimals <- 2
## tests long-format
### automatic model selection & posterior on variances (generalized inverse Gaussian)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$tolerance <- TRUE
options$priorPlot <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "var"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$contourPlot <- TRUE
options$contourLSL <- -10
options$contourUSL <- 0
options$rChart <- TRUE
options$xBarChart <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotFitLine <- TRUE
options$scatterPlotOriginLine <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$partByOperatorMeasurementPlot <- TRUE
options$trafficLightChart <- TRUE
options$distType <- "gig"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L1 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.12877675990061e+66, 0.774215429048362, "Null model", 1, "",
                                       "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                       0.1132125753614, 1.16629601468045, "Parts + Operators"))
})

test_that("L1 Contour plot matches", {
  plotName <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Contour plot")
})

test_that("L1 Producer's (δ) and Consumer's (β) Risk table results match", {
  table <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00736477179637162, 0.0216744399885818, "<unicode>", 0.0499081777154749,
                                       0.0466530277680386, 0.108715280572508, "<unicode>", 0.215164057882205
                                      ))
})

test_that("L1 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.670919564911975, 3.0245352573645, "Total gauge r&amp;R", 10.9190929658937,
                                       0.428250179262592, 1.36021150836429, "Repeatability", 2.88066205015795,
                                       0.128212597943151, 1.66432374900021, "Reproducibility", 9.17536520523994,
                                       0.128212597943151, 1.66432374900021, "Operator", 9.17536520523994,
                                       89.0809070341063, 96.9754647426355, "Part-to-part", 99.329080435088,
                                       "", 100, "Total variation", ""))
})

test_that("L1 Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 part-by-operator-interaction")
})

test_that("L1 Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 measurements-by-operator")
})

test_that("L1 Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 measurements-by-part")
})

test_that("L1 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.19096793143039, 22.1323186178753, 16.0625851727784, 31.6417520174887,
                                       "Total gauge r&amp;R", 33.0440494785342, 63.2188661898839, 6.54408266139483,
                                       19.029002036245, 11.350372563902, 22.1820672082641, "Repeatability",
                                       16.9725132030173, 26.0101475724349, 3.58067865526675, 8.33600694551753,
                                       10.6346110073191, 21.0635408862679, "Reproducibility", 30.2908603863182,
                                       58.7048697914693, 3.58067865526675, 8.33600694551753, 10.6346110073191,
                                       21.0635408862679, "Operator", 30.2908603863182, 58.7048697914693,
                                       94.3826821629762, 131.211179325445, 98.4543125061381, 203.211928839979,
                                       "Part-to-part", 99.663975655716, 326.423894741774, "", 134.744882991994,
                                       100, 206.191090810824, "Total variation", "", 327.99950826692
                                      ))
})

test_that("L1 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.368871976964589, 2.21323186178753, 0.527362533624812, 3.16417520174887,
                                       "Total gauge r&amp;R", 1.0536477698314, 6.32188661898839, 0.317150033937417,
                                       1.9029002036245, 0.369701120137735, 2.21820672082641, "Repeatability",
                                       0.433502459540581, 2.60101475724349, 0.138933449091959, 0.833600694551753,
                                       0.351059014771132, 2.10635408862679, "Reproducibility", 0.978414496524488,
                                       5.87048697914693, 0.138933449091959, 0.833600694551753, 0.351059014771132,
                                       2.10635408862679, "Operator", 0.978414496524488, 5.87048697914693,
                                       2.18685298875742, 13.1211179325445, 3.38686548066632, 20.3211928839979,
                                       "Part-to-part", 5.44039824569624, 32.6423894741774, 2.24574804986656,
                                       13.4744882991994, 3.4365181801804, 20.6191090810824, "Total variation",
                                       5.46665847111534, 32.799950826692))
})

test_that("L1 Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 matrix-plot-for-operators")
})

test_that("L1 Trace plot operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot operators")
})

test_that("L1 Trace plot parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot parts")
})

test_that("L1 Trace plot error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot error")
})

test_that("L1 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12976.7847365407, 14513.9171783411, 0.0658033899141875, 0.0312997467081693,
                                       0.372522174123887, "<unicode><sup>2</sup><sub>Part</sub>", 0.999953053021056,
                                       8704.25937264595, 8233.86226424337, 0.00740967850958981, 0.000262324047248233,
                                       0.0480102637825961, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       0.999974760804342, 9546.85887636171, 12886.8436428875, 0.000228240041946567,
                                       0.000325831496649699, 0.000854132423074902, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00071398092664))
})

test_that("L1 g-prior plot matches", {
  plotName <- results[["results"]][["priorPlot"]][["collection"]][["priorPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 g-prior")
})

test_that("L1 rChart matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 rChart")
})

test_that("L1 Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("L1 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Traffic light chart")
})

test_that("L1 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 components-of-variation")
})

test_that("L1 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.136066535389768, 0.328096142001867, 0.644397114715373, "Total gauge r&amp;R",
                                       1.11017362680325, 0.100584144121899, 0.137562980979213, 0.0223909092194328,
                                       "Repeatability", 0.187924382599008, 0.0193025032807373, 0.190533161022654,
                                       0.643297759565438, "Reproducibility", 0.957295283604225, 0.0193025032807373,
                                       0.190533161022654, 0.643297759565438, "Operator", 0.957295283604225,
                                       4.78232600045095, 12.2276330755706, 7.37228614940217, "Part-to-part",
                                       29.5979330733141, 5.04338430452398, 12.5557292175724, 7.40434881294641,
                                       "Total variation", 29.8843550382391))
})

test_that("L1 Error plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 error")
})

test_that("L1 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 operator")
})

test_that("L1 Part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 part")
})

test_that("L1 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.74553753942056, 30.6258717515955, "Part", 12.2162981090291,
                                       0.0194816499149477, 0.95544020657414, "Operator", 0.190667747178609,
                                       0.100053378595352, 0.186966922560894, "Error", 0.137558351971457
                                      ))
})

test_that("L1 xBar chart matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 xBar chart")
})

test_that("L1 Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Operator A", "Point 1", "", "Point 2", "", "Point 3", "", "Point 4",
                                       "", "Point 5", "", "Point 6", "", "Point 7", "", "Point 8",
                                       "", "Point 9", "", "Point 10", "Operator B", "Point 1", "",
                                       "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "",
                                       "Point 6", "", "Point 7", "", "Point 8", "", "Point 9", "",
                                       "Point 10", "Operator C", "Point 1", "", "Point 2", "", "Point 3",
                                       "", "Point 4", "", "Point 5", "", "Point 6", "", "Point 7",
                                       "", "Point 8", "", "Point 9", "", "Point 10"))
})


### full model, autocorrelation plot & posterior on %study var (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$estimationType <- "manual"
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percStudyVar"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "autocor"
options$distType <- "metalog"
options$modelType <- "fullModel"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L2 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.12877675990061e+66, 0.774215429048362, "Null model", 1, "",
                                       "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                       0.1132125753614, 1.16629601468045, "Parts + Operators"))
})

test_that("L2 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.696105466222007, 3.04116657815206, "Total gauge r&amp;R", 9.73852489722835,
                                       0.3639888862482, 1.18311924743632, "Repeatability", 2.51622369169823,
                                       0.251013167874466, 1.85804733071574, "Reproducibility", 8.14234365484943,
                                       0.112863258114111, 1.4672112708297, "Operator", 7.68044140031819,
                                       90.2614751027717, 96.9588334218479, "Part-to-part", 99.303894533778,
                                       "", 100, "Total variation", ""))
})

test_that("L2 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.34329351054366, 22.9954931389201, 16.319713319632, 32.2225009276222,
                                       "Total gauge r&amp;R", 31.2066096920135, 59.3116929800999, 6.0331491463114,
                                       17.6299809977431, 10.582775821833, 20.7071456112151, "Repeatability",
                                       15.8626091513384, 24.385071120885, 5.01012140250975, 12.4010519770202,
                                       12.0165078363917, 23.8294731700082, "Reproducibility", 28.5347921741103,
                                       55.4120432462635, 3.35951273364438, 7.86335740593823, 10.0709711490773,
                                       20.03492322479, "Operator", 27.7136076661396, 53.8158942993182,
                                       95.0060393350966, 131.007166464703, 98.4483187369879, 203.476784324812,
                                       "Part-to-part", 99.6513394459787, 333.198763701663, "", 134.767611968993,
                                       100, 206.501410466378, "Total variation", "", 335.590495280262
                                      ))
})

test_that("L2 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.383258218982001, 2.29954931389201, 0.537041682127037, 3.22225009276222,
                                       "Total gauge r&amp;R", 0.988528216334998, 5.93116929800999,
                                       0.293833016629052, 1.76299809977431, 0.345119093520251, 2.07071456112151,
                                       "Repeatability", 0.40641785201475, 2.4385071120885, 0.206684199617003,
                                       1.24010519770202, 0.397157886166804, 2.38294731700082, "Reproducibility",
                                       0.923534054104391, 5.54120432462635, 0.131055956765637, 0.786335740593823,
                                       0.333915387079834, 2.003492322479, "Operator", 0.896931571655304,
                                       5.38158942993182, 2.18345277441171, 13.1007166464703, 3.39127973874686,
                                       20.3476784324812, "Part-to-part", 5.55331272836105, 33.3198763701663,
                                       2.24612686614989, 13.4767611968993, 3.44169017443964, 20.6501410466378,
                                       "Total variation", 5.5931749213377, 33.5590495280262))
})

test_that("L2 Autocor plot operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot operators")
})

test_that("L2 Autocor plot parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot parts")
})

test_that("L2 Autocor plot inter matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_interaction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot inter")
})

test_that("L2 Autocor plot error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot error")
})

test_that("L2 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13191.8410414534, 14342.6406255755, 0.0622637924744842, 0.0378012113276895,
                                       0.492225265263849, "<unicode><sup>2</sup><sub>Part</sub>", 1.00005949661552,
                                       8573.74492676368, 9410.35310861475, 0.0148774342912111, 0.000185098091545953,
                                       0.0332933774694423, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       1.00018007210306, 2346.13428708614, 4754.33114737657, 0.00043997991681228,
                                       0.000250252868232187, 0.00189935114910479, "<unicode><sup>2</sup><sub>Part <unicode> Operator</sub>",
                                       1.00024263600134, 6409.52548193589, 9877.77075844831, 0.000252767339423204,
                                       0.000254706253481139, 0.000709946988142918, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00015212415386))
})

test_that("L2 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Traffic light chart")
})

test_that("L2 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 components-of-variation")
})

test_that("L2 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.146886862419837, 0.342528063914258, 1.58585858811382, "Total gauge r&amp;R",
                                       0.977188035312025, 0.0863378416613291, 0.119934941826869, 0.0202149557233692,
                                       "Repeatability", 0.165175470437101, 0.042718358381045, 0.222593122087389,
                                       1.58533514873101, "Reproducibility", 0.852915153751286, 0.0171756638884518,
                                       0.182861384576996, 1.58475768334045, "Operator", 0.804486244429589,
                                       4.76746602210277, 12.252490721534, 7.06879918980905, "Part-to-part",
                                       30.8392835887576, 5.04508589897889, 12.5950187854483, 7.25246033276211,
                                       "Total variation", 31.2836058155292))
})

test_that("L2 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 operator")
})

test_that("L2 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 part-to-part")
})

test_that("L2 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 repeatability")
})

test_that("L2 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 reproducibility")
})

test_that("L2 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 total-gauge-r-r")
})

test_that("L2 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.41064754985555, 32.5571141494677, "Total gauge r&amp;R", 16.3732796776501,
                                       6.04641169302359, 15.7159469512571, "Repeatability", 10.577436978073,
                                       5.08963858806674, 29.0743342985016, "Reproducibility", 12.0278695881536,
                                       3.39403289930916, 27.8877916998344, "Operator", 10.0579341876222,
                                       94.7729906533299, 99.6443899107024, "Part-to-part", 98.4432906994884
                                      ))
})

### type 3, density diagnostics plot & posterior on %Tolerance (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$partLongFormat <- "Parts"
options$type3 <- TRUE
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percTol"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L3 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.21824945358147e+67, 4.52862079579288e-05, "Null model", 1, "",
                                       "Parts"))
})

test_that("L3 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.486660998980841, 1.57812882489574, "Total gauge r&amp;R", 3.31935853093493,
                                       0.486660998980841, 1.57812882489574, "Repeatability", 3.31935853093493,
                                       96.6806414690651, 98.4218711751043, "Part-to-part", 99.5133390010192,
                                       "", 100, "Total variation", ""))
})

test_that("L3 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.97610906534218, 20.3690235963092, 12.2240799868402, 23.748053216033,
                                       "Total gauge r&amp;R", 18.2191068137777, 27.8369834027741, 6.97610906534218,
                                       20.3690235963092, 12.2240799868402, 23.748053216033, "Repeatability",
                                       18.2191068137777, 27.8369834027741, 98.3263146207893, 131.405161770277,
                                       99.2070911544107, 203.640368012146, "Part-to-part", 99.7563727292203,
                                       334.591586444311, "", 133.578417870217, 100, 205.107533242308,
                                       "Total variation", "", 335.288690363817))
})

test_that("L3 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.339483726605153, 2.03690235963092, 0.395800886933883, 2.3748053216033,
                                       "Total gauge r&amp;R", 0.463949723379567, 2.7836983402774, 0.339483726605153,
                                       2.03690235963092, 0.395800886933883, 2.3748053216033, "Repeatability",
                                       0.463949723379567, 2.7836983402774, 2.19008602950462, 13.1405161770277,
                                       3.39400613353576, 20.3640368012146, "Part-to-part", 5.57652644073852,
                                       33.4591586444311, 2.22630696450361, 13.3578417870217, 3.41845888737181,
                                       20.5107533242308, "Total variation", 5.58814483939695, 33.5288690363817
                                      ))
})

test_that("L3 Density plot type3 parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Density plot type3 parts")
})

test_that("L3 Density plot type 3 error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Density plot type 3 error")
})

test_that("L3 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13013.8840798061, 13434.1168244218, 0.0641205840760172, 0.0456636982148231,
                                       0.466833124753949, "<unicode><sup>2</sup><sub>Part</sub>", 0.999911614893083,
                                       10264.3177964902, 13098.7327637057, 0.000253246417063138, 0.000424909254671792,
                                       0.000947491447941651, "<unicode><sup>2</sup><sub>Error</sub>",
                                       0.999947379869504))
})

test_that("L3 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Traffic light chart")
})

test_that("L3 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 components-of-variation")
})

test_that("L3 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.115249200662092, 0.15765892633313, 0.0254995397510439, "Total gauge r&amp;R",
                                       0.215249345973368, 0.115249200662092, 0.15765892633313, 0.0254995397510439,
                                       "Repeatability", 0.215249345973368, 4.7964768221962, 12.2669633998433,
                                       7.04810792330225, "Part-to-part", 31.0976471553251, 4.95644270972451,
                                       12.4246223261764, 7.047978206467, "Total variation", 31.2273628685889
                                      ))
})

test_that("L3 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 part-to-part")
})

test_that("L3 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 repeatability")
})

test_that("L3 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 total-gauge-r-r")
})

test_that("L3 Total variation plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_total"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 total-variation")
})

test_that("L3 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(20.4255240578229, 27.7936833384141, "Total gauge r&amp;R", 23.7462166121691,
                                       20.4255240578229, 27.7936833384141, "Repeatability", 23.7462166121691,
                                       132.494221789949, 328.601362747611, "Part-to-part", 203.47063965151,
                                       134.688309385621, 329.570001781454, "Total variation", 204.959377455091
                                      ))
})


### historical SD & posterior on %Contribution (metalog)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 1.5
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percContrib"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L4 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.12877675990061e+66, 0.774215429048362, "Null model", 1, "",
                                       "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                       0.1132125753614, 1.16629601468045, "Parts + Operators"))
})

test_that("L4 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.04740157287856, 14.5820507556385, "Total gauge r&amp;R", 49.3410500801445,
                                       4.47040640541774, 6.11391026574281, "Repeatability", 8.35219478217815,
                                       0.857889034699435, 8.46814048989574, "Reproducibility", 42.5464570490767,
                                       0.857889034699435, 8.46814048989574, "Operator", 42.5464570490767,
                                       50.6589499198555, 86.5601443205738, "Part-to-part", 93.9525984271214,
                                       "", 100, "Total variation", ""))
})

test_that("L4 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(24.5914651309726, 22.1323186178753, 35.1575022416541, 31.6417520174887,
                                       "Total gauge r&amp;R", 70.2431846554265, 63.2188661898839, 21.1433355958278,
                                       19.029002036245, 24.6467413425156, 22.1820672082641, "Repeatability",
                                       28.9001639693721, 26.0101475724349, 9.26222993946393, 8.33600694551753,
                                       23.4039343180755, 21.0635408862679, "Reproducibility", 65.2276331016325,
                                       58.7048697914693, 9.26222993946393, 8.33600694551753, 23.4039343180755,
                                       21.0635408862679, "Operator", 65.2276331016325, 58.7048697914693,
                                       71.1751008074451, 64.0575907267006, 92.4357630195515, 83.1921867175963,
                                       "Part-to-part", 96.9291485710678, 87.2362337139611, "", "",
                                       100, 90, "Total variation", "", ""))
})

test_that("L4 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.368871976964589, 2.21323186178753, 0.527362533624812, 3.16417520174887,
                                       "Total gauge r&amp;R", 1.0536477698314, 6.32188661898839, 0.317150033937417,
                                       1.9029002036245, 0.369701120137735, 2.21820672082641, "Repeatability",
                                       0.433502459540581, 2.60101475724349, 0.138933449091959, 0.833600694551753,
                                       0.351059014771132, 2.10635408862679, "Reproducibility", 0.978414496524488,
                                       5.87048697914693, 0.138933449091959, 0.833600694551753, 0.351059014771132,
                                       2.10635408862679, "Operator", 0.978414496524488, 5.87048697914693,
                                       1.06762651211168, 6.40575907267006, 1.38653644529327, 8.31921867175963,
                                       "Part-to-part", 1.45393722856602, 8.7236233713961, "", "", 1.5,
                                       9, "Total variation", "", ""))
})

test_that("L4 Density plot histSd operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd operators")
})

test_that("L4 Density plot histSd parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd parts")
})

test_that("L4 Density plot histSd error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd error")
})

test_that("L4 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12976.7847365407, 14513.9171783411, 0.0658033899141875, 0.0312997467081693,
                                       0.372522174123887, "<unicode><sup>2</sup><sub>Part</sub>", 0.999953053021056,
                                       8704.25937264595, 8233.86226424337, 0.00740967850958981, 0.000262324047248233,
                                       0.0480102637825961, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       0.999974760804342, 9546.85887636171, 12886.8436428875, 0.000228240041946567,
                                       0.000325831496649699, 0.000854132423074902, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00071398092664))
})

test_that("L4 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Traffic light chart")
})

test_that("L4 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 components-of-variation")
})

test_that("L4 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.136066535389768, 0.328096142001867, 0.644397114715373, "Total gauge r&amp;R",
                                       1.11017362680325, 0.100584144121899, 0.137562980979213, 0.0223909092194328,
                                       "Repeatability", 0.187924382599008, 0.0193025032807373, 0.190533161022654,
                                       0.643297759565438, "Reproducibility", 0.957295283604225, 0.0193025032807373,
                                       0.190533161022654, 0.643297759565438, "Operator", 0.957295283604225,
                                       1.13982637319675, 1.94760324721291, 0.281041018716366, "Part-to-part",
                                       2.11393346461023, "", 2.25, "", "Total variation", ""))
})

test_that("L4 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 operator")
})

test_that("L4 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 repeatability")
})

test_that("L4 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 reproducibility")
})

test_that("L4 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 total-gauge-r-r")
})

test_that("L4 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.08226611815527, 49.5040664786821, "Total gauge r&amp;R", 14.9268377329309,
                                       4.50197095328446, 8.30300612767847, "Repeatability", 6.11251161411179,
                                       0.877820377220899, 41.7675610876332, "Reproducibility", 9.25863309749667,
                                       0.877820377220899, 41.7675610876332, "Operator", 9.25863309749667,
                                       49.0200204142767, 94.4511926749533, "Part-to-part", 86.6635262027822
                                      ))
})


### report
test_that("L Gauge r&R report plot matches", {
  options <- analysisOptions("msaBayesianGaugeRR")
  options$measurementLongFormat <- "Dm"
  options$operatorLongFormat <- "Operators"
  options$partLongFormat <- "Parts"
  options$tolerance <- TRUE
  options$posteriorPlotType <- "var"
  options$report <- TRUE
  options$reportRChartByOperator <- TRUE
  options$reportMeasurementsByOperatorPlot <- TRUE
  options$reportAverageChartByOperator <- TRUE
  options$reportPartByOperatorPlot <- TRUE
  options$mcmcChains <- 2
  options$customCiType <- "customCiQuantiles"
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L gauge-r-r-report")
})



## tests wide format
#local_snapshotter(snap_dir = "_snaps/msaBayesianGaugeRR/wide format/")
### automatic model selection & posterior on variances (generalized inverse Gaussian)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$tolerance <- TRUE
options$priorPlot <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "var"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$contourPlot <- TRUE
options$contourLSL <- 5
options$contourUSL <- 12
options$rChart <- TRUE
options$xBarChart <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotFitLine <- TRUE
options$scatterPlotOriginLine <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$partByOperatorMeasurementPlot <- TRUE
options$trafficLightChart <- TRUE
options$distType <- "gig"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W1 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603774384, 0.765529237919063, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                       0.0121495135306724, 1.17106508065889, "Part + Operator"))
})

test_that("W1 Contour plot wide matches", {
  plotName <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Contour plot wide")
})

test_that("W1 Producer's (δ) and Consumer's (β) Risk table results match", {
  table <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0264547436114185, 0.078598838947759, "<unicode>", 0.215676443034412,
                                       0.19026036243428, 0.310865868502054, "<unicode>", 0.412944723908465
                                      ))
})

test_that("W1 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12.2901782443658, 33.9014036146912, "Total gauge r&amp;R", 69.6440151508561,
                                       7.14052888380169, 19.7619230995554, "Repeatability", 35.4296094318763,
                                       1.92046043721284, 14.1394805151358, "Reproducibility", 56.2653322281232,
                                       1.92046043721284, 14.1394805151358, "Operator", 56.2653322281232,
                                       30.3559848491439, 66.0985963853088, "Part-to-part", 87.7098217556342,
                                       "", 100, "Total variation", ""))
})

test_that("W1 Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 part-by-operator-interaction")
})

test_that("W1 Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 measurements-by-operator")
})

test_that("W1 Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 measurements-by-part")
})

test_that("W1 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(35.0573504621692, 53.9822543314093, 56.9756927711812, 74.5858358557081,
                                       "Total gauge r&amp;R", 83.4529897613737, 140.952081451527, 26.72176806239,
                                       47.4576812697827, 43.6436739509928, 55.3625843445522, "Repeatability",
                                       59.522776671, 64.8869817538909, 13.8580677942577, 18.5275401463483,
                                       34.2775415929221, 46.3925507299324, "Reproducibility", 75.0102199813039,
                                       129.21538892197, 13.8580677942577, 18.5275401463483, 34.2775415929221,
                                       46.3925507299324, "Operator", 75.0102199813039, 129.21538892197,
                                       55.0962653047302, 67.1277172456173, 80.7121290580858, 106.474021209311,
                                       "Part-to-part", 93.653521958023, 171.970852710948, "", 93.4460758436266,
                                       100, 132.012623514198, "Total variation", "", 203.682676176965
                                      ))
})

test_that("W1 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.899704238856822, 5.39822543314093, 1.2430972642618, 7.45858358557081,
                                       "Total gauge r&amp;R", 2.34920135752546, 14.0952081451527, 0.790961354496379,
                                       4.74576812697827, 0.92270973907587, 5.53625843445522, "Repeatability",
                                       1.08144969589818, 6.48869817538909, 0.308792335772471, 1.85275401463483,
                                       0.773209178832206, 4.63925507299324, "Reproducibility", 2.15358981536617,
                                       12.921538892197, 0.308792335772471, 1.85275401463483, 0.773209178832206,
                                       4.63925507299324, "Operator", 2.15358981536617, 12.921538892197,
                                       1.11879528742696, 6.71277172456173, 1.77456702015519, 10.6474021209311,
                                       "Part-to-part", 2.8661808785158, 17.1970852710948, 1.55743459739378,
                                       9.34460758436266, 2.20021039190329, 13.2012623514198, "Total variation",
                                       3.39471126961608, 20.3682676176965))
})

test_that("W1 Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 matrix-plot-for-operators")
})

test_that("W1 Trace plot operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot operators wide")
})

test_that("W1 Trace plot parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot parts wide")
})

test_that("W1 Trace plot error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot error wide")
})

test_that("W1 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(10990.2660894303, 13770.3657749733, 0.0193456745474261, 0.0119209324153606,
                                       0.159871964522456, "<unicode><sup>2</sup><sub>Part</sub>", 0.999998507723977,
                                       8980.32124794736, 9124.13201671554, 0.0290836748383371, 0.00154538841049666,
                                       0.184998044953287, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       0.999970549982465, 9666.44207052295, 12489.9032027756, 0.00140813483425186,
                                       0.0019773737049108, 0.00356207292085309, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00076049669098))
})

test_that("W1 g-prior plot matches", {
  plotName <- results[["results"]][["priorPlot"]][["collection"]][["priorPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 g-prior")
})

test_that("W1 rChart wide matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 rChart wide")
})

test_that("W1 Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("C", "Point 10"))
})

test_that("W1 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Traffic light chart wide")
})

test_that("W1 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 components-of-variation")
})

test_that("W1 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.809467717550865, 1.74531081286552, 2.62090979862725, "Total gauge r&amp;R",
                                       5.51874702080536, 0.625619864307605, 0.856852295704607, 0.138877939178542,
                                       "Repeatability", 1.16953344480314, 0.0953527067029777, 0.888458517160912,
                                       2.61073443792635, "Reproducibility", 4.63794939194786, 0.0953527067029777,
                                       0.888458517160912, 2.61073443792635, "Operator", 4.63794939194786,
                                       1.2517028955058, 3.36784555242765, 2.07561166974137, "Part-to-part",
                                       8.21499283790811, 2.42560252873995, 5.11315636529317, 3.34075049828905,
                                       "Total variation", 11.524064604294))
})

test_that("W1 Error plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 error")
})

test_that("W1 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 operator")
})

test_that("W1 Part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 part")
})

test_that("W1 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.26704200867585, 8.70378773355763, "Part", 3.37362847505173,
                                       0.0966365619269659, 4.55424772931473, "Operator", 0.888472069958861,
                                       0.602932621355345, 0.842616713321816, "Error", 0.856776319403849
                                      ))
})

test_that("W1 xBar chart wide matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 xBar chart wide")
})

test_that("W1 Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                       "B", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                       "C", "Point 1", "", "Point 3", "", "Point 5", "", "Point 10"
                                      ))
})

### full model, autocorrelation plot & posterior on %study var (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$estimationType <- "manual"
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percStudyVar"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "autocor"
options$distType <- "metalog"
options$modelType <- "fullModel"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W2 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603774384, 0.765529237919063, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                       0.0121495135306724, 1.17106508065889, "Part + Operator"))
})

test_that("W2 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13.3553093571204, 36.2180484937395, "Total gauge r&amp;R", 69.2692706289203,
                                       6.57090520910721, 18.3355937497356, "Repeatability", 32.6937820977469,
                                       4.03815334489656, 17.8824547440039, "Reproducibility", 56.0865051419617,
                                       1.81952511320772, 13.598775017784, "Operator", 52.9266985517355,
                                       30.7307293710797, 63.7819515062605, "Part-to-part", 86.6446906428796,
                                       "", 100, "Total variation", ""))
})

test_that("W2 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.5449166680215, 56.7884807905523, 59.0198226823284, 77.822217228875,
                                       "Total gauge r&amp;R", 83.228162630399, 137.270361658595, 25.6337768544991,
                                       45.9168865243715, 42.0562631948515, 53.6949744952597, "Repeatability",
                                       57.1784768011171, 63.0056819142684, 20.0951569908419, 28.0092171340985,
                                       40.0005673924917, 53.9800292675623, "Reproducibility", 74.8909239651688,
                                       125.900245645802, 13.4889773992381, 18.204048466041, 33.6239546719455,
                                       46.0134920541663, "Operator", 72.7507376684603, 123.325253762174,
                                       55.4353039039937, 65.5925236678122, 79.2596383177368, 105.30155648011,
                                       "Part-to-part", 93.0831298574889, 175.998102871786, "", 94.5918257181484,
                                       100, 133.014364793134, "Total variation", "", 206.045843438775
                                      ))
})

test_that("W2 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.946474679842539, 5.67884807905523, 1.29703695381458, 7.7822217228875,
                                       "Total gauge r&amp;R", 2.28783936097658, 13.7270361658595, 0.765281442072858,
                                       4.59168865243715, 0.894916241587662, 5.36949744952597, "Repeatability",
                                       1.05009469857114, 6.30056819142684, 0.466820285568309, 2.80092171340985,
                                       0.899667154459371, 5.39800292675623, "Reproducibility", 2.09833742743003,
                                       12.5900245645802, 0.303400807767351, 1.8204048466041, 0.766891534236105,
                                       4.60134920541663, "Operator", 2.05542089603624, 12.3325253762174,
                                       1.09320872779687, 6.55925236678122, 1.75502594133517, 10.530155648011,
                                       "Part-to-part", 2.93330171452977, 17.5998102871786, 1.57653042863581,
                                       9.45918257181484, 2.21690607988557, 13.3014364793134, "Total variation",
                                       3.43409739064626, 20.6045843438775))
})

test_that("W2 Autocor plot operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot operators wide")
})

test_that("W2 Autocor plot parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot parts wide")
})

test_that("W2 Autocor plot inter wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_interaction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot inter wide")
})

test_that("W2 Autocor plot error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot error wide")
})

test_that("W2 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(10718.9977250569, 13208.9872645514, 0.0189883745061443, 0.0107170782335658,
                                       0.155580481652752, "<unicode><sup>2</sup><sub>Part</sub>", 1.00030834423142,
                                       8813.54777436966, 9477.82091097726, 0.116986275041296, 0.00109665489435784,
                                       0.16001179303652, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       1.00024411956321, 2528.65899040024, 4911.43662201896, 0.00200690136162596,
                                       0.000919872751968552, 0.00829090771954971, "<unicode><sup>2</sup><sub>Part <unicode> Operator</sub>",
                                       1.00078219210747, 6953.86141218597, 10328.3968231786, 0.00159888943185258,
                                       0.00200537333725614, 0.00405717500994585, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00023859236731))
})

test_that("W2 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Traffic light chart wide")
})

test_that("W2 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 components-of-variation")
})

test_that("W2 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.895814319585344, 2.03257406266536, 13.358368192055, "Total gauge r&amp;R",
                                       5.23420895783674, 0.585655685620189, 0.806176089444896, 0.132646222205457,
                                       "Repeatability", 1.10269887598515, 0.21792118000945, 1.22639797322047,
                                       13.3537231106705, "Reproducibility", 4.40301996485171, 0.0920520502345046,
                                       1.03670712030723, 13.3511092409608, "Operator", 4.22475509937313,
                                       1.19510532262555, 3.30222101071894, 2.01118548920259, "Part-to-part",
                                       8.60425895333312, 2.48544819241527, 5.3347950733843, 13.5115760573386,
                                       "Total variation", 11.7930248959608))
})

test_that("W2 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 operator")
})

test_that("W2 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 part-to-part")
})

test_that("W2 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 repeatability")
})

test_that("W2 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 reproducibility")
})

test_that("W2 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 total-gauge-r-r")
})

test_that("W2 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.6765556450027, 84.6233465765735, "Total gauge r&amp;R", 59.0855764448839,
                                       25.5060894690653, 56.8546515182814, "Repeatability", 42.0468636689062,
                                       20.1227289132326, 75.7177182498445, "Reproducibility", 40.0561474130257,
                                       13.7721047163308, 73.6333178040631, "Operator", 33.6812325833645,
                                       54.1269029906323, 93.0640167805956, "Part-to-part", 79.2111087019902
                                      ))
})

### type 3, density diagnostics plot & posterior on %Tolerance (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$partWideFormat <- "Part"
options$type3 <- TRUE
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percTol"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W3 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98096672972405178368, 0.000349979419027281, "Null model", 1, "",
                                       "Part"))
})

test_that("W3 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.83081567952283, 23.7598919702467, "Total gauge r&amp;R", 41.8274496817368,
                                       8.83081567952283, 23.7598919702467, "Repeatability", 41.8274496817368,
                                       58.1725503182632, 76.2401080297533, "Part-to-part", 91.1691843204772,
                                       "", 100, "Total variation", ""))
})

test_that("W3 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(29.7166882611562, 48.0241014671244, 47.9003801961148, 55.9991052269219,
                                       "Total gauge r&amp;R", 64.6741445105544, 65.6920462384277, 29.7166882611562,
                                       48.0241014671244, 47.9003801961148, 55.9991052269219, "Repeatability",
                                       64.6741445105544, 65.6920462384277, 76.2709317618788, 67.5499445080637,
                                       87.171102537986, 106.583932335271, "Part-to-part", 95.4825556392114,
                                       175.73087948947, "", 87.086935848688, 100, 121.077346756447,
                                       "Total variation", "", 184.656155967787))
})

test_that("W3 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.80040169111874, 4.80241014671244, 0.933318420448699, 5.59991052269219,
                                       "Total gauge r&amp;R", 1.09486743730713, 6.56920462384277, 0.80040169111874,
                                       4.80241014671244, 0.933318420448699, 5.59991052269219, "Repeatability",
                                       1.09486743730713, 6.56920462384277, 1.12583240846773, 6.75499445080637,
                                       1.77639887225452, 10.6583932335271, "Part-to-part", 2.92884799149116,
                                       17.573087948947, 1.45144893081147, 8.7086935848688, 2.01795577927412,
                                       12.1077346756447, "Total variation", 3.07760259946312, 18.4656155967787
                                      ))
})

test_that("W3 Density plot type3 parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Density plot type3 parts wide")
})

test_that("W3 Density plot type 3 error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Density plot type 3 error wide")
})

test_that("W3 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11707.3772753072, 12898.8791981331, 0.0185384976342472, 0.0117758505186744,
                                       0.133935146076771, "<unicode><sup>2</sup><sub>Part</sub>", 0.999912246299417,
                                       10252.2089186134, 12928.747430743, 0.00140504193471407, 0.00203664967824041,
                                       0.00556308506258751, "<unicode><sup>2</sup><sub>Error</sub>",
                                       0.999972050770322))
})

test_that("W3 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Traffic light chart wide")
})

test_that("W3 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 components-of-variation")
})

test_that("W3 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.64064286759136, 0.876622085936741, 0.141450061477035, "Total gauge r&amp;R",
                                       1.19873470534066, 0.64064286759136, 0.876622085936741, 0.141450061477035,
                                       "Repeatability", 1.19873470534066, 1.267498611964, 3.37071057882139,
                                       1.97733245942889, "Part-to-part", 8.57815073257139, 2.10670399913398,
                                       4.24733266475813, 1.9814272402398, "Total variation", 9.47163777543305
                                      ))
})

test_that("W3 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 part-to-part")
})

test_that("W3 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 repeatability")
})

test_that("W3 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 total-gauge-r-r")
})

test_that("W3 Total variation plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_total"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 total-variation")
})

test_that("W3 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(48.1714882535511, 65.4777848056589, "Total gauge r&amp;R", 55.985917347559,
                                       48.1714882535511, 65.4777848056589, "Repeatability", 55.985917347559,
                                       68.0697507873156, 173.397787709277, "Part-to-part", 106.53503836118,
                                       87.6024563014078, 182.690390900529, "Total variation", 121.016213906352
                                      ))
})

### historical SD & posterior on %Contribution (metalog)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 1.5
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percContrib"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
options$diagnosticsPlots <- TRUE
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W4 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603774384, 0.765529237919063, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                       0.0121495135306724, 1.17106508065889, "Part + Operator"))
})

test_that("W4 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(35.9763430022607, 77.5693694606897, "Total gauge r&amp;R", 245.277645369127,
                                       27.8053273025602, 38.0823242535381, "Repeatability", 51.9792642134731,
                                       4.2378980756879, 39.4870452071516, "Reproducibility", 206.131084086572,
                                       4.2378980756879, 39.4870452071516, "Operator", 206.131084086572,
                                       0, 37.4632712661338, "Part-to-part", 64.0236569977393, "", 100,
                                       "Total variation", ""))
})

test_that("W4 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(59.9802825904548, 53.9822543314093, 82.8731509507868, 74.5858358557081,
                                       "Total gauge r&amp;R", 156.61342383503, 140.952081451527, 52.7307569664252,
                                       47.4576812697827, 61.513982605058, 55.3625843445522, "Repeatability",
                                       72.0966463932121, 64.8869817538909, 20.5861557181648, 18.5275401463483,
                                       51.5472785888137, 46.3925507299324, "Reproducibility", 143.572654357744,
                                       129.21538892197, 20.5861557181648, 18.5275401463483, 51.5472785888137,
                                       46.3925507299324, "Operator", 143.572654357744, 129.21538892197,
                                       0, 0, 55.9721076150717, 50.3748968535646, "Part-to-part", 80.014784255408,
                                       72.0133058298672, "", "", 100, 90, "Total variation", "", ""
                                      ))
})

test_that("W4 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.899704238856822, 5.39822543314093, 1.2430972642618, 7.45858358557081,
                                       "Total gauge r&amp;R", 2.34920135752546, 14.0952081451527, 0.790961354496379,
                                       4.74576812697827, 0.92270973907587, 5.53625843445522, "Repeatability",
                                       1.08144969589818, 6.48869817538909, 0.308792335772471, 1.85275401463483,
                                       0.773209178832206, 4.63925507299324, "Reproducibility", 2.15358981536617,
                                       12.921538892197, 0.308792335772471, 1.85275401463483, 0.773209178832206,
                                       4.63925507299324, "Operator", 2.15358981536617, 12.921538892197,
                                       0, 0, 0.839581614226076, 5.03748968535646, "Part-to-part", 1.20022176383112,
                                       7.20133058298672, "", "", 1.5, 9, "Total variation", "", ""
                                      ))
})

test_that("W4 Density plot histSd operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd operators wide")
})

test_that("W4 Density plot histSd parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd parts wide")
})

test_that("W4 Density plot histSd error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd error wide")
})

test_that("W4 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(10990.2660894303, 13770.3657749733, 0.0193456745474261, 0.0119209324153606,
                                       0.159871964522456, "<unicode><sup>2</sup><sub>Part</sub>", 0.999998507723977,
                                       8980.32124794736, 9124.13201671554, 0.0290836748383371, 0.00154538841049666,
                                       0.184998044953287, "<unicode><sup>2</sup><sub>Operator</sub>",
                                       0.999970549982465, 9666.44207052295, 12489.9032027756, 0.00140813483425186,
                                       0.0019773737049108, 0.00356207292085309, "<unicode><sup>2</sup><sub>Error</sub>",
                                       1.00076049669098))
})

test_that("W4 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Traffic light chart wide")
})

test_that("W4 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 components-of-variation")
})

test_that("W4 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.809467717550865, 1.74531081286552, 2.62090979862725, "Total gauge r&amp;R",
                                       5.51874702080536, 0.625619864307605, 0.856852295704607, 0.138877939178542,
                                       "Repeatability", 1.16953344480314, 0.0953527067029777, 0.888458517160912,
                                       2.61073443792635, "Reproducibility", 4.63794939194786, 0.0953527067029777,
                                       0.888458517160912, 2.61073443792635, "Operator", 4.63794939194786,
                                       0, 0.84292360348801, 0.449525312232019, "Part-to-part", 1.44053228244913,
                                       "", 2.25, "", "Total variation", ""))
})

test_that("W4 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 operator")
})

test_that("W4 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 repeatability")
})

test_that("W4 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 reproducibility")
})

test_that("W4 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_gauge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 total-gauge-r-r")
})

test_that("W4 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.2368952637831, 244.371445479639, "Total gauge r&amp;R", 78.8433555056541,
                                       28.0908740417142, 51.6414577310351, "Repeatability", 38.0679862161104,
                                       4.39833689598487, 195.349115755055, "Reproducibility", 41.8962049145278,
                                       4.39833689598487, 195.349115755055, "Operator", 41.8962049145278,
                                       6.57451560452529, 63.9780337635583, "Part-to-part", 42.8639381882304
                                      ))
})

### report
test_that("W Gauge r&R report plot matches", {
  options <- analysisOptions("msaBayesianGaugeRR")
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
  options$operatorWideFormat <- "Operator"
  options$partWideFormat <- "Part"
  options$tolerance <- TRUE
  options$posteriorPlotType <- "var"
  options$report <- TRUE
  options$reportRChartByOperator <- TRUE
  options$reportMeasurementsByOperatorPlot <- TRUE
  options$reportAverageChartByOperator <- TRUE
  options$reportPartByOperatorPlot <- TRUE
  options$mcmcChains <- 2
  options$customCiType <- "customCiQuantiles"
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W gauge-r-r-report")
})


## validation and edge cases
.msabTestOptions <- function() {
  options <- analysisOptions("msaBayesianGaugeRR")
  options$measurementLongFormat <- "Dm"
  options$operatorLongFormat <- "Operators"
  options$partLongFormat <- "Parts"
  options$mcmcChains <- 2
  options$mcmcIterations <- 2000
  options$mcmcBurnin <- 500
  options$customCiType <- "customCiQuantiles"
  options$posteriorPlotType <- "var"
  options$distType <- "metalog"
  options$diagnosticsPlots <- FALSE
  options
}
.msabTestData <- function() {
  read.csv(testthat::test_path("datasets", "msaGaugeRRCrossed", "msaGaugeRRCrossed_long.csv"), fileEncoding = "UTF-8-BOM")
}

test_that("Diagnostics plots without diagnostics table do not crash", {
  options <- .msabTestOptions()
  options$diagnosticsTable <- FALSE
  options$diagnosticsPlots <- TRUE
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  diagnostics <- results[["results"]][["mcmcDiagnostics"]][["collection"]]
  expect_setequal(names(diagnostics), paste0("mcmcDiagnostics_", c("part", "operator", "error")))
})

test_that("Diagnostics plots are shown as placeholders when not ready", {
  options <- .msabTestOptions()
  options$measurementLongFormat <- ""
  options$diagnosticsPlots <- TRUE
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_false(is.null(results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_plot"]]))
})

test_that("Historical standard deviation smaller than gauge r&R gives a validation error", {
  options <- .msabTestOptions()
  options$processVariationReference <- "historicalSd"
  options$historicalSdValue <- 0.1
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "historical standard deviation", fixed = TRUE)
})

test_that("Historical standard deviation close to gauge r&R truncates the part variance", {
  options <- .msabTestOptions()
  options$processVariationReference <- "historicalSd"
  options$historicalSdValue <- 0.62
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  footnotes <- vapply(results[["results"]][["varCompTable"]][["footnotes"]], `[[`, character(1), "text")
  expect_true(any(grepl("set to zero", footnotes, fixed = TRUE)))
})

test_that("Unbalanced design gives a validation error", {
  options <- .msabTestOptions()
  dataset <- .msabTestData()
  dataset <- dataset[!(dataset$Parts == 1 & dataset$Operators == "Operator A"), ]
  results <- runAnalysis("msaBayesianGaugeRR", dataset, options, view = FALSE)
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "Design is not balanced", fixed = TRUE)
})

test_that("Unequal number of replicates gives a validation error", {
  options <- .msabTestOptions()
  results <- runAnalysis("msaBayesianGaugeRR", .msabTestData()[-1, ], options, view = FALSE)
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "Number of replicates differ", fixed = TRUE)
})

test_that("Missing values give a validation error", {
  options <- .msabTestOptions()
  options$measurementLongFormat <- "DmMissing5"
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "Missing values", fixed = TRUE)
})

test_that("A single replicate per part and operator gives a validation error", {
  options <- .msabTestOptions()
  dataset <- .msabTestData()
  dataset <- dataset[!duplicated(dataset[c("Parts", "Operators")]), ]
  results <- runAnalysis("msaBayesianGaugeRR", dataset, options, view = FALSE)
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "At least two measurements", fixed = TRUE)
})

test_that("Manual main effects model is used for estimation", {
  options <- .msabTestOptions()
  options$estimationType <- "manual"
  options$modelType <- "mainEffectsOnly"
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  footnotes <- vapply(results[["results"]][["varCompTable"]][["footnotes"]], `[[`, character(1), "text")
  expect_true(any(grepl("only including the main effects", footnotes, fixed = TRUE)))
  expect_length(results[["results"]][["BFtable"]][["data"]], 3)
})

test_that("Posterior summary with custom cut-off matches", {
  options <- .msabTestOptions()
  options$RRTable <- FALSE
  options$posteriorPlot <- TRUE
  options$posteriorCi <- TRUE
  options$posteriorCiType <- "custom"
  options$customCiType <- "customCiCutOff"
  options$posteriorCiCutOff <- 0.5
  options$setSeed <- TRUE
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table, list(1.81245579436666e-06, "Part", 12.0617425458701, 0.945745216564934,
                                       "Operator", 0.17576435210119, 0.999998251703199, "Error", 0.137046603167453
                                      ))
})

test_that("Report without gauge evaluation table is created", {
  options <- .msabTestOptions()
  options$report <- TRUE
  options$reportGaugeTable <- FALSE
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_identical(results[["results"]][["report"]][["status"]], "complete")
})
