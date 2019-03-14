
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"labelVar","title":"Label","type":"Variable"},{"name":"hypTrueEff","title":"Hypothesised True Effect Size","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"observedSE","title":"Observed Standard Error","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"observedP","title":"Observed P-Value","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"n","title":"N","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"alpha","type":"Number","title":"Alpha","default":0.05},{"name":"nSims","type":"Number","title":"Number of Simulations","default":10000},{"name":"sensHyp","title":"Sensitivity - Hypothesized True Effect","type":"Bool","default":true},{"name":"sensN","title":"Sensitivity - Sample Size","type":"Bool","default":true},{"name":"sensSE","title":"Sensitivity - Standard Error","type":"Bool","default":true},{"name":"sensScatter","title":"Sensitivity - Scatter Viz","type":"Bool","default":true}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Mean Differences",
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
					label: "Hypothesised True Effect Size",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "hypTrueEff",
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
					label: "Observed P-Value",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "observedP",
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
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					name: "nSims",
					format: FormatDef.number
				}
			]
		},
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
					name: "sensSE"
				},
				{
					type: DefaultControls.CheckBox,
					name: "sensScatter"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
