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
    const palabra = match[3];
    palabras.push(match[3]);
});

lineReader.on('close', function(){
    console.log("Palabras leidas:" + palabras.length);
    fs.writeFileSync("./palabras.json", JSON.stringify(palabras,null,2));

    palabras = palabras.slice(0);
    palabras.push("molinete");
    palabras.sort();
    console.log("Palabras ordenadas:" + palabras.length);
    palabrasEncadenadas(palabras, [8,9,9,9], 2);
})



function preprocesaPalabras(palabras, letrasComunes ){
    const log = function(){};
    //const log = function(msg){ console.log(msg); };
    
    const iniciosPorNumeroDeLetras = [];
    for( let a = 0 ; a < palabras.length ; a += 1 ){
        const palabra = palabras[a];
        log("palabra:" + palabra );
        const inicio = palabra.substr(0,letrasComunes);
        log( "inicio:" + inicio );

        if( inicio.length < letrasComunes ){
            continue;
        }

        let inicios = iniciosPorNumeroDeLetras[palabra.length];
        if( typeof inicios == "undefined" ){
            inicios = {};
            iniciosPorNumeroDeLetras[palabra.length] = inicios;
        }
        
        let arrayInicio = inicios[inicio];
        if( typeof arrayInicio == "undefined" ){
            arrayInicio = [];
            inicios[inicio] = arrayInicio;
        }
        arrayInicio.push(palabra);

    }

    let longitud = 1;
    let porLongitud = palabrasConLetras(palabras,longitud);
    iniciosPorNumeroDeLetras.todas = [];
    while( porLongitud.length > 0 ){
        iniciosPorNumeroDeLetras.todas[longitud] = porLongitud;
        longitud += 1;
        porLongitud = palabrasConLetras(palabras,longitud);
    }

    for( let i = 0 ; i < iniciosPorNumeroDeLetras.length ; i += 1 ){
        fs.writeFileSync("./palabras-preprocesadas." + i + ".json", JSON.stringify(iniciosPorNumeroDeLetras[i],0,2));
    }
   
    return iniciosPorNumeroDeLetras;
}


let ultimaCombinacion = null;
function combinacionEncontrada(combinacion){
    console.log( "------" + combinacion );
    //if( ultimaCombinacion == null ){
    //    console.log( "" + combinacion );
    //}
    //else if( combinacion[0] != ultimaCombinacion[0] ){
    //    console.log( "" + combinacion );
    //}
    //ultimaCombinacion = combinacion.slice(0);
}

function busca( nivel, combinacion, preprocesadas, letras, letrasComunes ){

    const log = function(){};
    //const log = function(msg){ console.log(msg); };

    //log( "busca: nivel:" + nivel + "  combinacion:" + combinacion );

    if( nivel == letras.length ){
        if( combinacion[0].substr(0,letrasComunes) == combinacion[nivel-1].substr(-letrasComunes) ){
            combinacionEncontrada(combinacion);
            return true;
        }
        return false;
    }


    const letrasSiguientes = letras[nivel];
    //log( "  letrasSiguientes:" + letrasSiguientes );

    let candidatas = null;
    if( nivel == 0 ){
        candidatas = preprocesadas.todas[letrasSiguientes];
    }
    else{
        const palabraAnterior = combinacion[nivel-1];
        const inicio = palabraAnterior.substr(-letrasComunes);
        //log( "  palabraAnterior:" + palabraAnterior + "  inicio:" + inicio );

        candidatas = preprocesadas[letrasSiguientes][inicio];
    }

    if( typeof candidatas != "undefined" ){

        for( let i = 0 ; i < candidatas.length ; i += 1 ){
            const candidata = candidatas[i];
            combinacion[nivel] = candidata;
            busca( nivel+1, combinacion, preprocesadas, letras, letrasComunes );
            
        }
    }
}



function palabrasEncadenadas(palabras, letras, letrasComunes){
    //const palabrasConLongitud = letras.map( l => palabrasConLetras(palabras,l) );
    const preprocesadas = preprocesaPalabras(palabras,letrasComunes);
    console.log( "Palabras preprocesadas");

    //busca( 0, [], preprocesadas, letras, letrasComunes );
    // MOLINETE 8, * 9, * 9, ASTRONOMO 9
    // ASTRONOMO 9, MOLINETE 8, * 9, * 9
    busca( 2, ["astronomo","molinete"], preprocesadas, letras, letrasComunes);
}

//domingo 15 julio 2018
// cuando le da la ventolera juega a dar vueltas sin parar 8 letras molinete
// sociedad de individuos con un gran poder devastador 9 letras ¿tertulias?
// Abrigo de marineros 9 letras  estrellas
// examina las estrellas haciendo las observaciones pertinentes 9 letras astronomo




