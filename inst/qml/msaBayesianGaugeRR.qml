// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
	columns:	1
	info:		qsTr("Gauge repeatability and reproducibility (gauge r&R) quantifies how much of the observed variation in measurements is caused by the measurement system. The measurement system variation consists of repeatability (equipment variation) and reproducibility (operator variation).") + "\n\n"
				+ qsTr("This analysis offers a Bayesian implementation of gauge r&R based on a linear random effects model (random effects ANOVA; Rouder et al., 2012). The model is fit with the BayesFactor package and the variance components are estimated from Markov chain Monte Carlo (MCMC) samples of the posterior distribution. All summaries are reported as posterior means with 95% credible intervals.") + "\n\n"
				+ "## " + qsTr("Output") + "\n"
				+ "- " + qsTr("Model Comparison: Bayes factors (BF<sub>10</sub>) in favor of the full model compared to the model in each row, with the percent error of the BF estimate.") + "\n"
				+ "- " + qsTr("Variance Components: posterior mean, standard deviation and credible interval of each variance component.") + "\n"
				+ "- " + qsTr("% Contribution to Total Variation: posterior summaries of the contribution of each component to the total variation.") + "\n"
				+ "- " + qsTr("Standard Deviation & Study Variation: posterior summaries of the standard deviations and study variation. The number of distinct categories (1.41 × σ<sub>Part</sub>/σ<sub>Gauge</sub>) is reported as a posterior mean with credible interval and, unlike the AIAG convention, is not truncated to an integer.") + "\n"
				+ "- " + qsTr("% Study Variation & % Tolerance: posterior summaries of the percent study variation and percent tolerance.") + "\n"
				+ "- " + qsTr("MCMC diagnostics: effective sample sizes (ESS) in the bulk and the tails of the distribution, Rhat (values larger than 1.01 indicate potential convergence problems; Vehtari et al., 2021), and Monte Carlo standard errors (MCSE) of the mean and the 2.5% and 97.5% quantiles.") + "\n"
				+ "- " + qsTr("Posterior Summary: point estimates and credible intervals based on the distribution fit to the MCMC samples.") + "\n"
				+ "- " + qsTr("Producer's and Consumer's Risk: posterior summaries of the probability that a good part is rejected (δ) and that a bad part is accepted (β).")
	infoBottom:	"## " + qsTr("References") + "\n"
				+ "- Automotive Industry Action Group (2005). *Statistical process control (SPC) – Reference manual*. AIAG.\n"
				+ "- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). *Statistical process control handbook*. SKF group.\n"
				+ "- Duncan, A. J. (1986). *Quality control and industrial statistics*. Richard D. Irwin, Inc.\n"
				+ "- Mader, D. P., Prins, J., & Lampe, R. E. (1999). The economic impact of measurement error. *Quality Engineering, 11*(4), 563–574. https://doi.org/10.1080/08982119908919276\n"
				+ "- Montgomery, D. C. (2013). *Introduction to statistical quality control* (7th ed.). John Wiley & Sons.\n"
				+ "- Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*(5), 356–374. https://doi.org/10.1016/j.jmp.2012.08.001\n"
				+ "- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021). Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC. *Bayesian Analysis, 16*(2), 667–718. https://doi.org/10.1214/20-BA1221\n"
				+ "\n---\n"
				+ "## " + qsTr("R Packages") + "\n"
				+ "- BayesFactor\n"
				+ "- dplyr\n"
				+ "- ellipse\n"
				+ "- extraDistr\n"
				+ "- GeneralizedHyperbolic\n"
				+ "- ggplot2\n"
				+ "- HDInterval\n"
				+ "- jaspBase\n"
				+ "- jaspGraphs\n"
				+ "- mvtnorm\n"
				+ "- posterior\n"
				+ "- rmetalog\n"
				+ "- tidyr\n"

	DropDown
	{
		name:				"dataFormat"
		label:				qsTr("Data format")
		id:					dataFormat
		indexDefaultValue:	0
		info:				qsTr("Format of the data: all measurements in a single column (\"Single column\") or the replicate measurements of each part and operator across columns of a single row (\"Across rows\").")
		values:
		[
			{ label: qsTr("Single column"),	value: "longFormat" },
			{ label: qsTr("Across rows"),	value: "wideFormat" }
		]
	}

	VariablesForm
	{
		id:			variablesFormLongFormat
		visible:	dataFormat.currentValue === "longFormat"

		AvailableVariablesList
		{
			name:	"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:			"measurementLongFormat"
			title:			qsTr("Measurement")
			id:				measurementLongFormat
			singleVariable:	true
			allowedColumns:	["scale"]
			info:			qsTr("The observations collected from the measurement system.")
		}

		AssignedVariablesList
		{
			name:			"operatorLongFormat"
			title:			qsTr("Operator")
			id:				operatorLongFormat
			singleVariable:	true
			allowedColumns:	["nominal"]
			enabled:		!type3.checked
			info:			qsTr("The operators in the measurement system. Not used in a Type 3 study.")
		}

		AssignedVariablesList
		{
			name:			"partLongFormat"
			title:			qsTr("Part")
			singleVariable:	true
			allowedColumns:	["nominal"]
			info:			qsTr("The parts of the measurement system.")
		}
	}

	VariablesForm
	{
		id:			variablesFormWideFormat
		visible:	dataFormat.currentValue === "wideFormat"

		AvailableVariablesList
		{
			name:	"variablesFormWideFormat"
		}

		AssignedVariablesList
		{
			name:			"measurementsWideFormat"
			title:			qsTr("Measurements")
			id:				measurementsWideFormat
			singleVariable:	false
			allowedColumns:	["scale"]
			info:			qsTr("The replicate measurements of each part and operator, one column per replicate.")
		}

		AssignedVariablesList
		{
			name:			"operatorWideFormat"
			title:			qsTr("Operator")
			id:				operatorWideFormat
			singleVariable:	true
			allowedColumns:	["nominal"]
			enabled:		!type3.checked
			info:			qsTr("The operators in the measurement system. Not used in a Type 3 study.")
		}

		AssignedVariablesList
		{
			name:			"partWideFormat"
			title:			qsTr("Part")
			singleVariable:	true
			allowedColumns:	["nominal"]
			info:			qsTr("The parts of the measurement system.")
		}
	}

	CheckBox
	{
		name:	"type3"
		id:		type3
		label:	qsTr("Type 3 study (automatic equipment)")
		info:	qsTr("Type 3 study for measurement systems without operator influence (e.g., automatic equipment). Only part and repeatability variation are estimated, and operator-related outputs are not available.")
	}

	Group
	{
		title:	qsTr("Analysis options")

		DropDown
		{
			name:				"estimationType"
			label:				qsTr("Estimation")
			id:					estimationType
			indexDefaultValue:	0
			visible:			!type3.checked
			info:				qsTr("Automatic: the model including the part × operator interaction (full model) is used if its Bayes factor against the main effects model exceeds the cut-off; otherwise the main effects model is used. Manual: choose the model yourself.")
			values:
			[
				{ label: qsTr("Automatic"),	value: "automatic" },
				{ label: qsTr("Manual"),	value: "manual" }
			]
		}

		DoubleField
		{
			name:			"bfFavorFull"
			label:			qsTr("Cut-off BF in favor of full model")
			id:				bfFavorFull
			defaultValue:	1
			min:			0
			inclusive:		JASP.None
			decimals:		3
			visible:		!type3.checked && estimationType.currentValue === "automatic"
			info:			qsTr("Bayes factor in favor of the full model that needs to be exceeded for the full model to be chosen over the main effects model.")
		}

		RadioButtonGroup
		{
			name:		"modelType"
			visible:	!type3.checked && estimationType.currentValue === "manual"
			info:		qsTr("Model used for estimation in manual mode.")

			RadioButton
			{
				name:		"fullModel"
				label:		qsTr("Full model")
				id:			fullModel
				checked:	true
				info:		qsTr("Model including part, operator and the part × operator interaction.")
			}

			RadioButton
			{
				name:	"mainEffectsOnly"
				label:	qsTr("Main effects only")
				id:		mainEffectsOnly
				info:	qsTr("Model including only part and operator.")
			}
		}

		DropDown
		{
			name:				"processVariationReference"
			label:				qsTr("Process variation based on")
			id:					variationReference
			indexDefaultValue:	0
			info:				qsTr("Base the process variation on the variation estimated from the study, or on a historically known standard deviation. In the latter case, the total variation is fixed at the historical variance and the part-to-part variation is the remainder after subtracting the gauge r&R variation.")
			values:
			[
				{ label: qsTr("Study variation"),				value: "studySd" },
				{ label: qsTr("Historical standard deviation"),	value: "historicalSd" }
			]
		}

		DoubleField
		{
			name:			"historicalSdValue"
			label:			qsTr("Historical standard deviation")
			defaultValue:	3
			min:			0
			inclusive:		JASP.None
			decimals:		9
			visible:		variationReference.currentValue === "historicalSd"
			info:			qsTr("Historically known standard deviation of the process.")
		}

		CheckBox
		{
			name:				"tolerance"
			label:				qsTr("Tolerance width")
			id:					tolerance
			childrenOnSameRow:	true
			info:				qsTr("Include the tolerance width to compute the percent tolerance.")

			DoubleField
			{
				name:			"toleranceValue"
				id:				toleranceValue
				defaultValue:	10
				min:			0
				inclusive:		JASP.None
				decimals:		9
			}
		}

		DropDown
		{
			name:				"studyVarianceMultiplierType"
			label:				qsTr("Study var. multiplier type")
			id:					studyVarianceMultiplierType
			indexDefaultValue:	0
			info:				qsTr("The study variation is the standard deviation times a multiplier, either given directly (Std. Deviation) or as the percentage of a normal distribution it should cover (Percent).")
			values:
			[
				{ label: qsTr("Std. Deviation"),	value: "sd" },
				{ label: qsTr("Percent"),			value: "percent" }
			]
		}

		DoubleField
		{
			name:			"studyVarianceMultiplierValue"
			label:			qsTr("Study var. multiplier value")
			fieldWidth:		60
			defaultValue:	6
			min:			0
			max:			100
			inclusive:		JASP.None
			decimals:		3
			info:			qsTr("Value of the study variation multiplier.")
		}

		CheckBox
		{
			name:		"RRTable"
			label:		qsTr("r&R table")
			checked:	true
			info:		qsTr("Show the model comparison, variance components, % contribution and gauge evaluation tables.")
		}
	}

	Section
	{
		title:		qsTr("Plots")
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"priorPlot"
				label:		qsTr("Prior")
				checked:	false
				info:		qsTr("Plot the g-prior on the standardized variance components (an inverse chi-square distribution scaled by the r scale).")
			}

			CheckBox
			{
				name:		"posteriorPlot"
				label:		qsTr("Posterior")
				checked:	false
				info:		qsTr("Plot the posterior distributions of the variances, % contribution, % study variation or % tolerance, together with a posterior summary table. A distribution is fit to the MCMC samples (see Advanced options).")

				DropDown
				{
					name:	"posteriorPlotType"
					label:	""
					id:		posteriorPlotType
					values:	tolerance.checked ? [
						{ label: qsTr("Variances"),			value: "var" },
						{ label: qsTr("%Contribution"),		value: "percContrib" },
						{ label: qsTr("%Study variation"),	value: "percStudyVar" },
						{ label: qsTr("%Tolerance"),		value: "percTol" }
					] : [
						{ label: qsTr("Variances"),			value: "var" },
						{ label: qsTr("%Contribution"),		value: "percContrib" },
						{ label: qsTr("%Study variation"),	value: "percStudyVar" }
					]
				}

				CheckBox
				{
					label:	qsTr("Display histogram")
					name:	"posteriorHistogram"
					info:	qsTr("Display a histogram of the MCMC samples.")
				}

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"posteriorPointEstimate"
					childrenOnSameRow:	true
					checked:			true
					info:				qsTr("Display the posterior mean, median or mode.")

					DropDown
					{
						name:	"posteriorPointEstimateType"
						label:	""
						values:
						[
							{ label: qsTr("mean"),		value: "mean" },
							{ label: qsTr("median"),	value: "median" },
							{ label: qsTr("mode"),		value: "mode" }
						]
					}
				}

				CheckBox
				{
					name:				"posteriorCi"
					label:				qsTr("CI")
					id:					posteriorCi
					childrenOnSameRow:	true
					checked:			false
					info:				qsTr("Display a central, highest posterior density (HPD) or custom credible interval. A custom interval is defined by its quantiles, or by a cut-off for which the posterior probability of lower values is computed.")

					DropDown
					{
						name:	"posteriorCiType"
						label:	""
						id:		posteriorCiType
						values:
						[
							{ label: qsTr("central"),	value: "central" },
							{ label: qsTr("HPD"),		value: "HPD" },
							{ label: qsTr("custom"),	value: "custom" }
						]
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		posteriorCiType.currentValue === "central" || posteriorCiType.currentValue === "HPD"
						enabled:		posteriorCi.checked
						name:			"posteriorCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			1
						max:			100
						inclusive:		JASP.MinOnly
						info:			qsTr("Posterior mass covered by the credible interval.")
					}

					RadioButtonGroup
					{
						name:		"customCiType"
						visible:	posteriorCiType.currentValue === "custom"
						enabled:	posteriorCi.checked

						RadioButton
						{
							name:		"customCiQuantiles"
							label:		qsTr("Quantiles")
							checked:	true
							info:		qsTr("Credible interval between the lower and upper quantile.")

							Group
							{
								columns:	2

								DoubleField
								{
									name:			"posteriorCiLower"
									label:			qsTr("Lower")
									id:				posteriorCiLower
									fieldWidth:		50
									defaultValue:	0.025
									min:			0
									max:			posteriorCiUpper.value
									inclusive:		JASP.None
								}

								DoubleField
								{
									name:			"posteriorCiUpper"
									label:			qsTr("Upper")
									id:				posteriorCiUpper
									fieldWidth:		50
									defaultValue:	0.975
									min:			posteriorCiLower.value
									max:			1
									inclusive:		JASP.None
								}
							}
						}

						RadioButton
						{
							name:	"customCiCutOff"
							label:	qsTr("Cut-off")
							info:	qsTr("Posterior probability that the quantity is smaller than the cut-off.")

							DoubleField
							{
								name:			"posteriorCiCutOff"
								fieldWidth:		50
								defaultValue:	10
								decimals:		3
								min:			0
								inclusive:		JASP.None
							}
						}
					}
				}
			}

			CheckBox
			{
				name:	"contourPlot"
				label:	qsTr("Contour plot")
				info:	qsTr("95% contour of the bivariate normal distribution of true values and measurements, based on the mean of the measurements and the posterior mean part and total variation (Mader et al., 1999). A table shows the producer's and consumer's risk for the given specification limits.")

				DoubleField
				{
					name:			"contourLSL"
					label:			qsTr("Lower specification limit")
					id:				contourLSL
					fieldWidth:		60
					negativeValues:	true
					defaultValue:	-1
					max:			contourUSL.value
					inclusive:		JASP.None
				}

				DoubleField
				{
					name:			"contourUSL"
					label:			qsTr("Upper specification limit")
					id:				contourUSL
					fieldWidth:		60
					defaultValue:	1
					negativeValues:	true
					min:			contourLSL.value
					inclusive:		JASP.None
				}
			}
		}

		Group
		{
			CheckBox
			{
				name:		"varianceComponentsGraph"
				label:		qsTr("Components of variation")
				checked:	true
				info:		qsTr("Bar chart of the components of variation with 95% credible intervals.")
			}

			CheckBox
			{
				name:		"rChart"
				id:			rChart
				label:		qsTr("Range charts by operator")
				enabled:	!type3.checked
				info:		qsTr("Control chart of the ranges per part, split by operator.")
			}

			CheckBox
			{
				name:		"xBarChart"
				id:			xBarChart
				label:		qsTr("Average chart by operator")
				enabled:	!type3.checked
				info:		qsTr("Control chart of the averages per part, split by operator.")
			}

			CheckBox
			{
				name:		"scatterPlot"
				label:		qsTr("Scatter plots operators")
				id:			scatterPlot
				enabled:	!type3.checked
				info:		qsTr("Scatter plot matrix comparing the measurements of the operators.")

				CheckBox
				{
					name:	"scatterPlotFitLine"
					label:	qsTr("Fit line")
				}

				CheckBox
				{
					name:	"scatterPlotOriginLine"
					label:	qsTr("Show origin line")
				}
			}

			CheckBox
			{
				name:	"partMeasurementPlot"
				label:	qsTr("Measurements by part plot")
				info:	qsTr("Average measurement for each part.")

				CheckBox
				{
					name:	"partMeasurementPlotAllValues"
					label:	qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name:		"operatorMeasurementPlot"
				id:			operatorMeasurementPlot
				label:		qsTr("Measurements by operator plot")
				enabled:	!type3.checked
				info:		qsTr("Boxplots of the measurements by operator.")
			}

			CheckBox
			{
				name:		"partByOperatorMeasurementPlot"
				id:			partByOperatorMeasurementPlot
				label:		qsTr("Part × operator interaction plot")
				enabled:	!type3.checked
				info:		qsTr("Average measurement for each part with separate lines for each operator.")
			}

			CheckBox
			{
				name:	"trafficLightChart"
				label:	qsTr("Traffic light chart")
				info:	qsTr("Traffic light charts of the % study variation and % tolerance of the total gauge r&R, with 95% credible intervals.")
			}
		}
	}

	Section
	{
		title:		qsTr("MCMC diagnostics")
		columns:	1

		CheckBox
		{
			label:		qsTr("Diagnostics table")
			name:		"diagnosticsTable"
			checked:	true
			info:		qsTr("Table with convergence diagnostics for each parameter.")
		}

		CheckBox
		{
			label:				qsTr("Plots")
			name:				"diagnosticsPlots"
			checked:			false
			childrenOnSameRow:	true
			info:				qsTr("Diagnostic plots per parameter and chain: trace plots, autocorrelations or densities.")

			DropDown
			{
				name:				"diagnosticsPlotType"
				indexDefaultValue:	0
				values:
				[
					{ label: qsTr("Traceplot"),			value: "trace" },
					{ label: qsTr("Autocorrelation"),	value: "autocor" },
					{ label: qsTr("Density"),			value: "density" }
				]
			}
		}
	}

	Section
	{
		title:	qsTr("Advanced options")

		Group
		{
			title:	qsTr("Priors")

			DoubleField
			{
				name:			"rscalePrior"
				label:			qsTr("r scale prior")
				defaultValue:	1
				min:			0
				max:			10
				inclusive:		JASP.MaxOnly
				decimals:		3
				info:			qsTr("Square root of the scale of the inverse chi-square g-prior on the random effects.")
			}
		}

		Group
		{
			SetSeed {}
		}

		Group
		{
			title:	qsTr("MCMC options")

			IntegerField
			{
				name:			"mcmcChains"
				label:			qsTr("Chains")
				defaultValue:	4
				min:			1
				max:			10
				info:			qsTr("Number of MCMC chains.")
			}

			IntegerField
			{
				name:			"mcmcIterations"
				label:			qsTr("Iterations per chain")
				id:				mcmcIterations
				defaultValue:	10000
				min:			Math.max(mcmcBurnin.value * 2, 100)
				max:			100000
				fieldWidth:		60
				info:			qsTr("Number of iterations of each chain, including the burn-in.")
			}

			IntegerField
			{
				name:			"mcmcBurnin"
				label:			qsTr("Burn-in per chain")
				defaultValue:	2000
				id:				mcmcBurnin
				min:			1
				max:			mcmcIterations.value / 2
				fieldWidth:		60
				info:			qsTr("Number of initial samples discarded from each chain.")
			}
		}

		Group
		{
			title:	qsTr("Distribution fit to MCMC samples")

			DropDown
			{
				name:				"distType"
				label:				qsTr("Distribution")
				indexDefaultValue:	0
				info:				qsTr("Distribution fit to the MCMC samples for the posterior plots and summaries. For variance posteriors, either a generalized inverse Gaussian or a metalog distribution; for all other posteriors, the metalog distribution.")
				values:				posteriorPlotType.currentValue === "var" ? [
					{ label: qsTr("Generalized inverse Gaussian"),	value: "gig" },
					{ label: qsTr("Metalog"),						value: "metalog" }
				] : [
					{ label: qsTr("Metalog"),						value: "metalog" }
				]
			}
		}
	}

	Section
	{
		title:	qsTr("Report options")

		CheckBox
		{
			name:		"report"
			label:		qsTr("Show Report")
			id:			report
			columns:	1
			info:		qsTr("Combine the selected components into a single gauge r&R report. Other output is hidden while the report is shown.")

			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns:	2
				info:		qsTr("Show the metadata below in the report.")

				CheckBox
				{
					name:				"reportTitle"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportTitleText"
						label:				qsTr("Title")
						id:					reportTitleText
						placeholderText:	qsTr("Gauge r&R report")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportPartName"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportPartNameText"
						label:				qsTr("Part name")
						id:					reportPartNameText
						placeholderText:	qsTr("Name")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportGaugeName"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportGaugeNameText"
						label:				qsTr("Gauge name")
						id:					reportGaugeNameText
						placeholderText:	qsTr("Name")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportCharacteristic"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportCharacteristicText"
						label:				qsTr("Characteristic")
						id:					reportCharacteristicText
						placeholderText:	qsTr("Characteristic")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportGaugeNumber"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportGaugeNumberText"
						label:				qsTr("Gauge number")
						id:					reportGaugeNumberText
						placeholderText:	qsTr("Number")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportTolerance"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportToleranceText"
						label:				qsTr("Tolerance")
						id:					reportToleranceText
						placeholderText:	qsTr("Tolerance")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportLocation"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportLocationText"
						label:				qsTr("Location")
						id:					reportLocationText
						placeholderText:	qsTr("Location")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportPerformedBy"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportPerformedByText"
						label:				qsTr("Performed by")
						id:					reportPerformedByText
						placeholderText:	qsTr("Analyst")
						fieldWidth:			100
					}
				}

				CheckBox
				{
					name:				"reportDate"
					checked:			true
					childrenOnSameRow:	true

					TextField
					{
						name:				"reportDateText"
						label:				qsTr("Date")
						id:					reportDate
						placeholderText:	qsTr("Date")
						fieldWidth:			100
					}
				}
			}

			Group
			{
				title:	qsTr("Select Report Components")

				CheckBox
				{
					name:		"reportGaugeTable"
					label:		qsTr("Show gauge evaluation table")
					checked:	true
				}

				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}

				CheckBox
				{
					name:		"reportMeasurementsByPartPlot"
					label:		qsTr("Show measurements by part")
					checked:	true
				}

				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show range charts by operator")
					checked:	true
					enabled:	!type3.checked
				}

				CheckBox
				{
					name:		"reportMeasurementsByOperatorPlot"
					label:		qsTr("Show measurements by operator")
					checked:	true
					enabled:	!type3.checked
				}

				CheckBox
				{
					name:		"reportAverageChartByOperator"
					label:		qsTr("Show average charts by operator")
					checked:	true
					enabled:	!type3.checked
				}

				CheckBox
				{
					name:		"reportPartByOperatorPlot"
					label:		qsTr("Show part × operator interaction")
					checked:	true
					enabled:	!type3.checked
				}

				CheckBox
				{
					name:		"reportTrafficLightChart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}
}
