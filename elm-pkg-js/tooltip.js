
exports.init = function(app) {

    const tooltips = {};

    app.ports.showTooltip.subscribe(function(elementId) {
        if (tooltips[elementId]) {
            clearTimeout(tooltips[elementId]);
        }
        tooltips[elementId] = setTimeout(() => {
            const element = document.getElementById(elementId);
            if (element) {
                element.title = element.dataset.fullText; // Usa el texto completo como tooltip.
            }
        }, 1000); // Esperar un segundo antes de mostrar el tooltip.
    });

};
