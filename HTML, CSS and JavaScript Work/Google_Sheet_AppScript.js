function updateValues() {
  var spreadsheet = SpreadsheetApp.openById('17zlYgIPHEeRtOXUfwCb4Nr40oFVNON-LoqPSyFpWtCM');
  var sheetNames = ['Daily loads - VIC']; // Example sheet name

  var today = new Date();
  today.setHours(0, 0, 0, 0);
  console.log(today);

  sheetNames.forEach(function(sheetName) {
    var sheet = spreadsheet.getSheetByName(sheetName);
    var lastRow = sheet.getLastRow();
    var lastColumn = sheet.getLastColumn();

    var datesRange = sheet.getRange(2, 1, 1, lastColumn);
    var dates = datesRange.getValues()[0];

    var todayColumnIndex = dates.findIndex(function(date) {
      var dateObj = new Date(date);
      dateObj.setHours(0, 0, 0, 0);
      return dateObj.getTime() === today.getTime();
    });

    if (todayColumnIndex !== -1) {
      var dataRange = sheet.getRange(3, 1, lastRow - 2, todayColumnIndex + 1);
      var formulas = dataRange.getFormulas();
      var values = dataRange.getValues();

      for (var i = 0; i < formulas.length; i++) {
        for (var j = 0; j < formulas[i].length; j++) {
          if (formulas[i][j].startsWith('=')) { // If the cell contains a formula
            values[i][j] = values[i][j]; // Replace formula with its current value
          }
        }
      }

      // Write the updated values back to the sheet, replacing formulas with values
      dataRange.setValues(values);
    }
  });
}
