var fs = require('fs');

module.exports = {
  loadData: function(day) {
    return fs.readFileSync(`../../days/${day.toString().padStart(2, '0')}/input.txt`, 'utf8');
  }
}
