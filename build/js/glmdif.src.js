
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"item","title":"Items","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a vector of strings naming the item columns from `data`"}},{"name":"group","title":"Grouping Variable","type":"Variable","suggested":["continuous"],"permitted":["factor","numeric"],"description":{"R":"a string naming the grouping variable from `data`"}},{"name":"matchVar","title":"Matching Variable","type":"Variable","suggested":["continuous"],"permitted":["factor","numeric"],"description":{"R":"a string naming the matching variable from `data`"}},{"name":"anchor","title":"Anchor Items","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the anchor item columns from `data`"}},{"name":"focal","title":"Reference Group","type":"String","description":"a string indicating which group(s) are to be considered focal groups"},{"name":"groupType","title":"Group Type","type":"List","options":[{"name":"group","title":"Discrete Groups"},{"name":"cont","title":"Continuous Groups"}],"description":"either \"discrete\" (default) to specify that group membership is made of two (or more than two) groups, or \"continuous\" to indicate that group membership is based on a continuous criterion."},{"name":"difFlagScale","title":"Evaluation Scale","type":"List","options":[{"name":"zt","title":"Zumbo-Thomas"},{"name":"jg","title":"Jodoin-Gierl"}]},{"name":"designAnalysis","type":"Bool","title":"Design Analysis","default":false},{"name":"designAnalysisSigOnly","type":"Bool","title":"Flagged items only","default":true},{"name":"power","type":"Bool","title":"Observed Power","default":false},{"name":"D","title":"Hypothesized True Effect Size","type":"String","default":""},{"name":"type","title":"Type","type":"List","options":[{"name":"udif","title":"Uniform DIF"},{"name":"nudif","title":"Non-Uniform DIF"},{"name":"both","title":"Uniform and Non-Uniform DIF"}],"default":"both","description":"a character string specifying which DIF effects must be tested. Possible values are \"both\" (default), \"udif\" and \"nudif\""},{"name":"criterion","title":"Flagging Criterion","type":"List","options":[{"name":"Wald","title":"Wald Statistic"},{"name":"LRT","title":"Likelihood Ratio Test"}],"description":"a character string specifying which DIF statistic is computed. Possible values are \"LRT\" (default) or \"Wald\""},{"name":"alpha","title":"Alpha","type":"Number","default":0.05,"description":"significance level"},{"name":"purify","title":"Item Purification","type":"Bool","default":false,"description":"should the method be used iteratively to purify the set of anchor items? (default is FALSE). Ignored if match is not \"score\""},{"name":"nIter","title":"Number of Iterations","type":"Number","default":10,"description":"the maximal number of iterations in the item purification process. (default is 10)"},{"name":"pAdjustMethod","title":"P-value Adjustment method","type":"List","options":[{"name":"bonferroni","title":"Bonferroni"},{"name":"holm","title":"Holm"},{"name":"hochberg","title":"Hochberg"},{"name":"hommel","title":"Hommel"},{"name":"BH","title":"Benjamini-Hochberg"},{"name":"BY","title":"Benjamini-Yekutieli"},{"name":"none","title":"None"}],"default":"BH","description":"either NULL (default) or the acronym of the method for p-value adjustment for multiple comparisons"},{"name":"plotVarsICC","type":"Variables"}];

const view = View.extend({
    jus: "2.0",

    events: [

	],

	update: require('./glmDIF.events').update

});

view.layout = ui.extend({

    label: "Differential Item Functioning",
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
					label: "Grouping Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "group",
							maxItemCount: 1,
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
					label: "Anchor Items",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "anchor",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "DIF Analysis",
			margin: "large",
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
									name: "difFlagScale"
								},
								{
									type: DefaultControls.ComboBox,
									name: "type"
								},
								{
									type: DefaultControls.ComboBox,
									name: "criterion"
								},
								{
									type: DefaultControls.TextBox,
									name: "alpha",
									format: FormatDef.number
								},
								{
									type: DefaultControls.ComboBox,
									name: "pAdjustMethod"
								},
								{
									type: DefaultControls.Label,
									label: "________________",
									controls: [
										{
											type: DefaultControls.ComboBox,
											name: "groupType"
										},
										{
											type: DefaultControls.TextBox,
											name: "focal",
											format: FormatDef.string
										}
									]
								},
								{
									type: DefaultControls.Label,
									label: "________________",
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
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Design Analysis",
			margin: "large",
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
								},
								{
									type: DefaultControls.TextBox,
									name: "D",
									format: FormatDef.string
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Item Characteristic Curves",
			margin: "large",
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
