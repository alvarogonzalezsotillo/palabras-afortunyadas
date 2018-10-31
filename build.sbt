// VARIOS TARGETS: https://github.com/muuki88/scala-target-examples


/*
enablePlugins(ScalaJSPlugin)

name := "Palabras anagramadas"
scalaVersion := "2.12.4" // or any other Scala version >= 2.10.2

// This is an application with a main method
mainClass in  (Compile, run) := Some("rne.PalabrasAnagramadas")
scalaJSUseMainModuleInitializer := true

// DOM DESDE SCALA
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

// JQUERY
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

// SCALATEST
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"

// NO HAY LIBRERÍA PARA XML PARA SCALAJS
// HE PUESTO LOS FUENTES EN SRC
// DE TODAS FORMAS, HACE FALTA ESTA DEPENDENCIA PARA QUE FUNCIONE EL COMPILADOR
// https://stackoverflow.com/questions/41830090/scala-js-support-for-scala-xml-and-scala-compiler
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"


// USAR WEBJARS PARA JQUERY
skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"


 */

// https://github.com/portable-scala/sbt-crossproject


// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
)

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  mainClass := Some("rne.Main"),
  scalaJSModuleKind := ModuleKind.CommonJSModule, //ModuleKind.NoModule,
  libraryDependencies += "io.scalajs" %%% "nodejs" % "0.4.2",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

  //scalaJSOutputWrapper := ("", "Main.main();")
)

val jvmSettings = Seq(
  mainClass := Some("rne.Main"),
)

lazy val palabras =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .jsSettings(jsSettings) // defined in sbt-scalajs-crossproject
    .jvmSettings(jvmSettings)
    .nativeSettings(/* ... */) // defined in sbt-scala-native

// Optional in sbt 1.x (mandatory in sbt 0.13.x)
lazy val palabrasJS     = palabras.js
lazy val palabrasJVM    = palabras.jvm
lazy val palabrasNative = palabras.native

addCommandAlias("run","palabrasJVM/run")
