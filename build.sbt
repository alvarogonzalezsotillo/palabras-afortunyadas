// VARIOS TARGETS: https://github.com/muuki88/scala-target-examples


ensimeScalaVersion  in ThisBuild := "2.11.8"
scalaVersion := "2.11.8"

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val copyGeneratedJS = taskKey[Unit]("copyGeneratedJS")

def copyFile( s: String, d: String ) = {
    val src = new java.io.File( s )
    val dst = new java.io.File( d )
    sbt.IO.copyFile( src, dst )  
}

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  copyGeneratedJS := {
    //val foj = (palabrasJS / fastOptJS).value
    copyFile( "./palabras/js/target/scala-2.11/palabras-jsdeps.js", "./palabras-jsdeps.js" )
    copyFile( "./palabras/js/target/scala-2.11/palabras-fastopt.js", "./palabras-fastopt.js" )
    copyFile( "./palabras/js/target/scala-2.11/palabras-opt.js", "./palabras-opt.js" )

  }
  
)

val jsSettings = {


  Seq(
    scalaJSUseMainModuleInitializer := true,
    mainClass := Some("rne.Main"),
    scalaJSModuleKind := ModuleKind.NoModule, // para navegador
                                              //scalaJSModuleKind :=  ModuleKind.CommonJSModule, // para nodejs
    libraryDependencies += "io.scalajs" %%% "nodejs" % "0.4.2",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1",
    jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"


    //scalaJSOutputWrapper := ("", "Main.main();")
  )
}

val jvmSettings = Seq(
  mainClass := Some("rne.Main"),
)

lazy val palabras =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .jsSettings(jsSettings) // defined in sbt-scalajs-crossproject
    .jvmSettings(jvmSettings)
    .nativeSettings(/* ... */) // defined in sbt-scala-native

// Optional in sbt 1.x (mandatory in sbt 0.13.x)
lazy val palabrasJS     = palabras.js
lazy val palabrasJVM    = palabras.jvm
lazy val palabrasNative = palabras.native

addCommandAlias("run","palabrasJVM/run")
