
exports.init = function(app) {

    
    app.ports.requestSelection.subscribe(function() {
        var selectedText = window.getSelection().toString().trim();
        if (selectedText) {
            app.ports.receiveSelection.send(selectedText);
        }
    });


};
