const events = {
    update: function(ui) {
        populatePlotVarsICCSupplier(ui, this);
    },

    onChange_item: function(ui) {
        populatePlotVarsICCSupplier(ui, this);
    }
};

let populatePlotVarsICCSupplier = function(ui, context) {
    let b1 = context.cloneArray(ui.item.value(), []);
    b1 = context.valuesToItems(b1, FormatDef.variable);
    ui.plotVarsICCSupplier.setValue(b1);
};


module.exports = events;

