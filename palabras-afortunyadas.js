// 1) Se libró de los malos bichos y se convirtió en un buen ambiente (8 letras)
// 2) No estará solo a la hora de reclamar sus derechos (7 letras)
// 3) Hacer filigranas en el canto (5 letras)
// 4) Especialistas en telas de fibras naturales (6 letras)

let fs = require('fs');
let rl = require('readline');


let lineReader = rl.createInterface({
    input: fs.createReadStream('./CREA_total.TXT','latin1')
});

function quitaAcentos(str){
    let acentos = [
        ["á","a"],
        ["é","e"],
        ["í","i"],
        ["ó","o"],
        ["ú","u"],
        ["ü","u"]
    ];

    for( let i = 0 ; i < acentos.length ; i += 1){
        let regex = new RegExp(acentos[i][0],"g" )
        str = str.replace( regex, acentos[i][1] );
    }
    return str;
}

//console.log( "Acentos:" + quitaAcentos("hólá cigüeña"));
//process.exit(0);


let palabras = [];

const log = function(){};

lineReader.on('line', function (line) {
    //const log = function(s){ console.log(s); };
    
    const regex = /(\s*)((?:\d|\.)*)\s*((?:[a-z]|ñ)*).*/
    line = quitaAcentos(line);
    log('Line from file:'+ line);
    const match = regex.exec(line)
    if( match ){
        log( match[3] );
    }
    palabras.push(match[3]);
});

lineReader.on('close', function(){
  console.log("palabras:" + palabras.length);    
})


