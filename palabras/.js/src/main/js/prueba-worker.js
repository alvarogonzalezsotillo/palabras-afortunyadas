//importScripts("./palabras/.js/target/scala-2.11/palabras-fastopt.js");

// creo la variable exports para que no falle el modulo creado por scalajs
exports = {};
require = function(m){
    console.log("Módulo no cargado: " + m);
}


importScripts("../../../target/scala-2.11/palabras-fastopt.js");
exports.WorkerMain.main();

/*
console.log("global:" + this );

this.addEventListener("message", function(e){
    console.log( "Me llega un mensaje al worker:" + e.data );
});


this.postMessage("El worker ya está listo");
*/
