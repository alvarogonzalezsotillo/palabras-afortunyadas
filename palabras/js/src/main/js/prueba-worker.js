// creo la variable exports para que no falle el modulo creado por scalajs
exports = {};
require = function(m){
    console.log("Módulo no cargado: " + m);
}


importScripts("../../../target/scala-2.11/palabras-fastopt.js");
exports.WorkerMain.main();

