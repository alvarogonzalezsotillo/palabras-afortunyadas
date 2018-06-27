let fs = require('fs');
let rl = require('readline');


let lineReader = rl.createInterface({
    input: fs.createReadStream('./CREA_total.TXT')
});

let palabras = [];

lineReader.on('line', function (line) {
    let regex = /.* ([a-z]*) .*/
    console.log('Line from file:', line);
    let match = regex.exec(line)
    if( match ){
        console.log( match[0] );
    }
});

