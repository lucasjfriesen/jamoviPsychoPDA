
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"labelVar","title":"Label","type":"Variable","suggested":["nominal"]},{"name":"hypTrueCor","title":"Hypothesised True Correlation","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"observedSE","title":"Observed Standard Error","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"observedCor","title":"Observed Correlation","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"n","title":"N","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"alpha","type":"Number","title":"Alpha","default":0.05},{"name":"sensHyp","title":"Sensitivity - Hypothesized True Effect","type":"Bool","default":false},{"name":"sensN","title":"Sensitivity - Sample Size","type":"Bool","default":false},{"name":"sensObs","title":"Sensitivity - Observed Correlation","type":"Bool","default":false},{"name":"HTEViz","title":"Sensitivity - Scatter Viz HTE","type":"Bool","default":false},{"name":"bootSims","type":"Number","title":"Number of Simulations","default":10000},{"name":"lengthOut","type":"Number","title":"lengthOut","default":1000},{"name":"corType","title":"Correlation Type","type":"List","options":[{"name":"pearson","title":"Pearson"},{"name":"spearman","title":"Spearman"}]}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Correlations",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Label",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "labelVar",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Observed Correlation",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "observedCor",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Observed Standard Error",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "observedSE",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "N",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "n",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Hypothesised True Correlation",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "hypTrueCor",
							maxItemCount: 1,
							isTarget: true
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
					type: DefaultControls.TextBox,
					name: "alpha",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					name: "bootSims",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					name: "lengthOut",
					format: FormatDef.number
				},
				{
					type: DefaultControls.ComboBox,
					name: "corType"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			label: "Sensitivity Analysis",
			margin: "small",
			collapsed: true,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					controls: [
						{
							type: DefaultControls.CheckBox,
							name: "sensHyp"
						},
						{
							type: DefaultControls.CheckBox,
							name: "sensN"
						},
						{
							type: DefaultControls.CheckBox,
							name: "sensObs"
						},
						{
							type: DefaultControls.CheckBox,
							name: "HTEViz"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
