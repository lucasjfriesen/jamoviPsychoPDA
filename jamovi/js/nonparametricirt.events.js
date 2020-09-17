const events = {
    update: function(ui) {
        updateplotItemsSupplier(ui, this);
    },

    onChange_item: function(ui) {
        updateplotItemsSupplier(ui, this);
    },
    
    onUpdate_plotVarsSupplier: function(ui) {
        updateplotItemsupplier(ui, this);
    },
};

let updateplotItemsSupplier = function(ui, context) {
    let b1 = context.cloneArray(ui.item.value(), []);
    b1 = context.valuesToItems(b1, FormatDef.variable);
    ui.plotItemsSupplier.setValue(b1);
};


module.exports = events;

