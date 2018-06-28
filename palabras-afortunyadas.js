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

function palabrasConLetras(palabras,letras){
    return palabras.filter( p => p.length == letras);
}

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
    console.log("Palabras leidas:" + palabras.length);
    fs.writeFileSync("./palabras.json", JSON.stringify(palabras));
    palabrasEncadenadas(palabras, [8,7,5,6], 2);
})


function filtraPalabras( aceptadas, candidatas, letrasComunes ){
    const ret = [];
    for( let a = 0 ; a < aceptadas.length ; a += 1 ){
        const aceptada = aceptadas[a];
        console.log("aceptada:" + aceptada );
        const fin = aceptada[aceptada.length-1].substr(-letrasComunes)
        console.log("fin:" + fin );
        if(fin.length<letrasComunes){
            continue;
        }
        for( let c = 0 ; c < candidatas.length ; c += 1){
            const candidata = candidatas[c];
            console.log("candidata:" + candidata );
            const inicio = candidata.substr(0,letrasComunes);
            console.log("inicio:" + inicio + " aceptada:" + aceptada + "fin:" + fin );
            if( fin == inicio ){
                const nuevaAceptada = aceptada.push(candidata);
                ret.push( nuevaAceptada );
                console.log( nuevaAceptada);
            }
        }
    }
}

function preprocesaPalabras(palabras, letrasComunes ){
    const log = function(){};
    //const log = function(msg){ console.log(msg); };
    
    const inicios = {};
    const finales = {};
    for( let a = 0 ; a < palabras.length ; a += 1 ){
        const palabra = palabras[a];
        log("palabra:" + palabra );
        const inicio = palabra.substr(0,letrasComunes);
        const fin = palabra.substr(-letrasComunes)
        log( "inicio:" + inicio + "  fin:" + fin );

        if( inicio.length < letrasComunes || fin.length < letrasComunes ){
            continue;
        }
        
        let arrayInicio = inicios[inicio];
        if( typeof arrayInicio == "undefined" ){
            arrayInicio = [];
            inicios[inicio] = arrayInicio;
        }
        arrayInicio.push(palabra);

        let arrayFin = finales[fin];
        if( typeof arrayFin == "undefined" ){
            arrayFin = [];
            finales[fin] = arrayFin;
        }
        arrayFin.push(palabra);

    }


    const ret = { inicios: inicios, finales: finales }; 
    fs.writeFileSync("./palabras-preprocesadas.json", JSON.stringify(ret));
   
    return ret;
}


function palabrasEncadenadas(palabras, letras, letrasComunes){
    //const palabrasConLongitud = letras.map( l => palabrasConLetras(palabras,l) );
    const preprocesadas = preprocesaPalabras(palabras,letrasComunes);
    console.log( preprocesadas );
    
}

