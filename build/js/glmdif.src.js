
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"ui":"The raw data with rows as test takers and item, grouping, and matching variables as columns.\n","R":"The raw data with rows as test takers and item, grouping, and matching variables as columns\n"}},{"name":"item","title":"Items","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"ui":"Items which are to be assessed for DIF.\n","R":"A vector of strings naming the item columns from `data` which are to be assessed for DIF\n"}},{"name":"group","title":"Grouping variable","type":"Variable","suggested":["nominal"],"permitted":["factor","numeric"],"description":{"ui":"The grouping variable.\n","R":"A string naming the grouping variable from `data`\n"}},{"name":"matchVar","title":"Matching variable","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"description":{"ui":"The matching variable.\n","R":"A string naming the matching variable from `data`\n"}},{"name":"anchor","title":"Anchor item(s)","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"ui":"The anchor items for use in purification. This will be ignored if an external matching variable is supplied.\n","R":"a vector of strings naming the anchor item columns from `data` for use in purification. This will be ignored if an external matching variable is supplied\n"}},{"name":"groupType","title":"Group type","type":"List","options":[{"name":"group","title":"Discrete Groups"},{"name":"cont","title":"Continuous Groups"}],"description":{"ui":"Either \"discrete\" (default) to specify that group membership is made of two (or more than two) groups, or \"continuous\" to indicate that group membership is based on a continuous criterion. \n","R":"Either \"discrete\" (default) to specify that group membership is made of two (or more than two) groups, or \"continuous\" to indicate that group membership is based on a continuous criterion. \n"}},{"name":"twoGroups","title":"Binary Grouping","type":"Bool","default":false},{"name":"difFlagScale","title":"Evaluation scale","type":"List","options":[{"name":"zt","title":"Zumbo-Thomas"},{"name":"jg","title":"Jodoin-Gierl"}],"description":{"ui":"The effect size criterion scale to be used in assigning 'level' to flagged items.\n","R":"The effect size criterion scale to be used in assigning 'level' to flagged items\n"}},{"name":"designAnalysis","type":"Bool","title":"Design analysis","default":false,"description":{"ui":"True/False, perform a design analysis. NB: Computationally intensive.\n","R":"True/False, perform a design analysis. NB: Computationally intensive\n"}},{"name":"designAnalysisEffectType","type":"List","title":"Effect type","options":[{"name":"nagR2","title":"Δ Naeglekirke R²"}],"default":"nagR2","description":{"ui":"In progress\n","R":"In progress\n"}},{"name":"designAnalysisSigOnly","type":"Bool","title":"Constant alpha","default":true,"description":{"ui":"True/False, should only items which have been flagged for exhibitting DIF be considered in the Design Analysis?\n","R":"True/False, should only items which have been flagged for exhibitting DIF be considered in the Design Analysis?\n"}},{"name":"bootSims","title":"Bootstrap N","type":"Number","default":1000,"description":{"ui":"Number of bootstrap simulations to perform.\n","R":"Number of bootstrap simulations to perform \n"}},{"name":"power","type":"Bool","title":"Observed power","default":false,"description":{"ui":"True/False, display the empirical observed power.","R":"True/False, display the empirical observed power\n"}},{"name":"D","title":"Hypothesized true effect size","type":"String","default":"","description":{"ui":"A character string indicating the hypothesized True Effect to be used in Design Analysis. Left blank will default to the category thresholds of the DIF scale selected.\n","R":"A character string indicating the hypothesized True Effect to be used in Design Analysis. Left blank will default to the category thresholds of the DIF scale selected\n"}},{"name":"type","title":"Type","type":"List","options":[{"name":"udif","title":"Uniform DIF"},{"name":"nudif","title":"Non-Uniform DIF"},{"name":"both","title":"Uniform and Non-Uniform DIF"}],"default":"both","description":{"ui":"A character string specifying which DIF effects must be tested. Possible values are \"both\" (default), \"udif\" and \"nudif\".\n","R":"A character string specifying which DIF effects must be tested. Possible values are \"both\" (default), \"udif\" and \"nudif\"\n"}},{"name":"criterion","title":"Flagging criterion","type":"List","options":[{"name":"Wald","title":"Wald Statistic"},{"name":"LRT","title":"Likelihood Ratio Test"}],"description":{"ui":"A character string specifying which DIF statistic is computed. Possible values are \"LRT\" (default) or \"Wald\".\n","R":"A character string specifying which DIF statistic is computed. Possible values are \"LRT\" (default) or \"Wald\"\n"}},{"name":"nagEff","title":"Naeglekirke R²","type":"Bool","default":true},{"name":"coeffEff","title":"Regression coefficients","type":"Bool","default":false},{"name":"alpha","title":"Alpha","type":"Number","default":0.05,"description":{"ui":"Significance level.\n","R":"Significance level\n"}},{"name":"purify","title":"Item purification","type":"Bool","default":false,"description":{"ui":"Should the method be used iteratively to purify the set of anchor items? (default is FALSE). Ignored if an external matching variable is supplied.\n","R":"Should the method be used iteratively to purify the set of anchor items? (default is FALSE). Ignored if an external matching variable is supplied\n"}},{"name":"nIter","title":"Number of iterations","type":"Number","default":10,"description":{"ui":"The maximal number of iterations in the item purification process. (default is 10).\n","R":"The maximal number of iterations in the item purification process. (default is 10)  \n"}},{"name":"pAdjustMethod","title":"P-value adjustment method","type":"List","options":[{"name":"bonferroni","title":"Bonferroni"},{"name":"holm","title":"Holm"},{"name":"hochberg","title":"Hochberg"},{"name":"hommel","title":"Hommel"},{"name":"BH","title":"Benjamini-Hochberg"},{"name":"BY","title":"Benjamini-Yekutieli"},{"name":"none","title":"None"}],"default":"none","description":{"ui":"Either none (default) or the acronym of the method for p-value adjustment for multiple comparisons\n","R":"Either none (default) or the acronym of the method for p-value adjustment for multiple comparisons.\n"}},{"name":"plotVarsICC","type":"Variables","description":{"ui":"Items for plotting Item Response Curves.\n","R":"A vector of strings naming the item columns for plotting Item Response Curves\n"}}];

const view = View.extend({
    jus: "2.0",

    events: [

	],

	update: require('./glmDIF.events').update

});

view.layout = ui.extend({

    label: "Binary Differential Item Functioning",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			label: "Data variables",
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Item(s) for Analysis",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "item",
							isTarget: true,
							events: [
								{ execute: require('./glmDIF.events').onChange_item }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Anchor Items",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "anchor",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Matching Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "matchVar",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Grouping Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "group",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "DIF Analysis",
			margin: "small",
			collapsed: true,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							controls: [
								{
									type: DefaultControls.ComboBox,
									name: "type"
								},
								{
									type: DefaultControls.ComboBox,
									name: "criterion"
								},
								{
									type: DefaultControls.ComboBox,
									name: "groupType"
								},
								{
									type: DefaultControls.CheckBox,
									name: "twoGroups"
								},
								{
									type: DefaultControls.ComboBox,
									name: "difFlagScale"
								},
								{
									type: DefaultControls.TextBox,
									name: "alpha",
									format: FormatDef.number
								},
								{
									type: DefaultControls.ComboBox,
									name: "pAdjustMethod"
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							margin: "large",
							controls: [
								{
									type: DefaultControls.CheckBox,
									name: "nagEff"
								},
								{
									type: DefaultControls.CheckBox,
									name: "coeffEff"
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							margin: "large",
							controls: [
								{
									type: DefaultControls.CheckBox,
									name: "purify",
									enable: "(anchor)"
								},
								{
									type: DefaultControls.TextBox,
									name: "nIter",
									format: FormatDef.number,
									enable: "(purify)"
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Design Analysis",
			margin: "small",
			collapsed: true,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							label: "N.B. | Computationally Instensive",
							margin: "large",
							controls: [
								{
									type: DefaultControls.CheckBox,
									name: "designAnalysis"
								},
								{
									type: DefaultControls.CheckBox,
									name: "designAnalysisSigOnly",
									enable: "(designAnalysis)"
								},
								{
									type: DefaultControls.CheckBox,
									name: "power",
									enable: "(designAnalysis)"
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					controls: [
						{
							type: DefaultControls.ComboBox,
							name: "designAnalysisEffectType"
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					controls: [
						{
							type: DefaultControls.TextBox,
							name: "D",
							format: FormatDef.string,
							enable: "(designAnalysis)"
						},
						{
							type: DefaultControls.TextBox,
							name: "bootSims",
							format: FormatDef.number,
							enable: "(designAnalysis)"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Item Response Curves",
			margin: "small",
			collapsed: true,
			controls: [
				{
					type: DefaultControls.VariableSupplier,
					name: "plotVarsICCSupplier",
					populate: "manual",
					persistentItems: false,
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									name: "plotVarsICC",
									isTarget: true
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
