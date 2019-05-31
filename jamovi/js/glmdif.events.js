const events = {
    update: function(ui) {
        updatePlotVarsICCSupplier(ui, this);
    },

    onChange_item: function(ui) {
        updatePlotVarsICCSupplier(ui, this);
    },
    
    onUpdate_plotVarsSupplier: function(ui) {
        updatePlotVarsICCSupplier(ui, this);
    },
};

let updatePlotVarsICCSupplier = function(ui, context) {
    let b1 = context.cloneArray(ui.item.value(), []);
    b1 = context.valuesToItems(b1, FormatDef.variable);
    ui.plotVarsICCSupplier.setValue(b1);
};


module.exports = events;

