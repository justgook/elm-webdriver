const fs = require('fs');
const path = require('path');
global.XMLHttpRequest = require("xhr2");
const app = require("./bundle.js").Elm.Main.init({});

app.ports.log.subscribe((a) => process.stdout.write(a));
app.ports.result.subscribe((report) => {
  const driver = process.env.DRIVER_NAME || "Unknown Driver2";
  const file = path.resolve(__dirname, 'stats.json')
  const stats = fs.existsSync(file)
    ? JSON.parse(fs.readFileSync('stats.json', 'utf8'))
    : { drivers: [], data: {} };


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

  fs.writeFileSync(file, JSON.stringify(stats), 'utf8');
})