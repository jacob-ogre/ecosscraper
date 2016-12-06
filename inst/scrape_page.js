// This must be executed with phantomjs
// Scrapes a web page using phantomjs and saves the HTML to file
// phantomjs scrape_page.js <optsList>
//
// 'optsList' is a JSON array containing configurations for each page to be
// scraped. Each configuration object needs to contain at least properties
// "url" and "file". For instance:
// [{"url":"https://ecos.fws.gov/ecp0/profile/speciesProfile?spcode=A000", 
//   "file":"MYOSOD_ECOS.html"}]

var webPage = require('webpage');
var system = require('system');

var page = webPage.create();
var fs = require('fs');

var args = system.args;

if (args.length < 2) {
  console.log(
    'Usage:\n' +
    '  phantomjs scrape_page.js <optsList>\n' +
    '\n' +
    'optsList is a JSON array containing configuration for each scrape.\n' +
    'For instance:\n' +
    '\'[{"url":"url1.html","file":"file1.png"},{"url":"url2.html","file":"fil2.png","zoom":2}]\'');
  phantom.exit(1);
}

// console.log(args[1]);

var optsList = JSON.parse(args[1])[0];

page.open(optsList.url, function (status) {
  var content = page.content;
  try {
    fs.write(optsList.file, content, 'w');
  } catch(e) {
    console.log(e);
  }
  phantom.exit();
});
