const fs = require('fs');
global.XMLHttpRequest = require("xhr2");
const app = require("./bundle.js").Elm.Main.init({});


// insert = function (index, items) { this.splice.apply(this, [index, 0].concat(items)); }

app.ports.log.subscribe((a) => process.stdout.write(a));
app.ports.result.subscribe((report) =>
  fs.readFile('stats.json', 'utf8', function readFileCallback(err, input) {
    const driver = process.env.DRIVER_NAME || "Unknown Driver2";
    let stats = err ? { drivers: [], data: {} } : JSON.parse(input);
    let index = stats.drivers.indexOf(driver);
    if (index === -1) {
      index = stats.drivers.push(driver) - 1;
    }
    stats.data = Object.keys(report)
      .reduce((acc, key) => {
        if (!acc[key]) { acc[key] = [] }
        acc[key][index] = report[key];
        return acc
      }, stats.data);
    // process.exitCode = errors.length
    fs.writeFileSync('stats.json', JSON.stringify(stats), 'utf8'/*, callback*/);
  })
)