function updateValues() {
  var spreadsheet = SpreadsheetApp.openById('1xGocgnBRsZiC7zuuaBai_w6uJ0WJiNFETDCpeVlheXc');
  var sheetNames = ['Daily loads - VIC', 'Daily loads - NSW', 'Daily loads - QLD', 'Daily loads - SA/NT/WA']; // Example sheet name

  // Get today's date without the time portion
  var today = new Date();
  today.setHours(0, 0, 0, 0);

  sheetNames.forEach(function(sheetName) {
    var sheet = spreadsheet.getSheetByName(sheetName);
    var lastRow = sheet.getLastRow();
    var lastColumn = sheet.getLastColumn();

    // Get the range of dates in the second row
    var datesRange = sheet.getRange(2, 1, 1, lastColumn);
    var dates = datesRange.getValues()[0];

    // Find the index of the last column before today's date
    var lastColumnBeforeToday = -1;
    for (var i = 0; i < dates.length; i++) {
      var dateObj = new Date(dates[i]);
      dateObj.setHours(0, 0, 0, 0);
      if (dateObj < today) {
        lastColumnBeforeToday = i;
      }
    }

    // Only proceed if there's at least one column before today's date
    if (lastColumnBeforeToday !== -1) {
      // Get the range up to the last column before today
      var dataRange = sheet.getRange(3, 1, lastRow - 2, lastColumnBeforeToday + 1);
      var formulas = dataRange.getFormulas();
      var values = dataRange.getValues();

      // Replace formulas with their current values in the range before today
      for (var row = 0; row < formulas.length; row++) {
        for (var col = 0; col <= lastColumnBeforeToday; col++) {
          if (formulas[row][col].startsWith('=')) { 
            values[row][col] = values[row][col]; 
          }
        }
      }

      // Write the updated values back to the sheet
      dataRange.setValues(values);
    }
  });
}
    }
  });
}
