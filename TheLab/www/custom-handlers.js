// Custom message handlers for checkbox tracking
Shiny.addCustomMessageHandler('race_selection_handler', function(message) {
  eval(message.js_code);
});