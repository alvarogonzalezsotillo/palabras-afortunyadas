//importScripts("./palabras/.js/target/scala-2.11/palabras-fastopt.js");
importScripts("../../../target/scala-2.11/palabras-fastopt.js");
WorkerMain.main();

/*
console.log("global:" + this );

this.addEventListener("message", function(e){
    console.log( "Me llega un mensaje al worker:" + e.data );
});


this.postMessage("El worker ya está listo");
*/
