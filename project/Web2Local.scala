
import sbt._
import Keys._

//TODO: how to add scalaOptions+="-optimize" ?

object Web2LocalBuild extends Build {
  import Resolvers._

  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    version                                   := "0.1-SNAPSHOT",
    javacOptions                             ++= Seq("-Xlint:unchecked"),
    exportJars                                := true,
    publishArtifact in (Compile, packageDoc)  := false
  )

  lazy val Web2Local = Project(
    id        = "Web2Local",
    base      = file("web2local"),
    settings = commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang"            % "scala-compiler"      % "2.10.4",
        "org.apache.httpcomponents" %  "httpclient"         %   "4.2",
        "org.apache.commons"        %  "commons-math"       %   "2.2",
        "org.jsoup"                 %  "jsoup"              %   "1.7.2",
        "org.joda"                  %  "joda-convert"       %   "1.2",
        "com.tictactec"             %  "ta-lib"             %   "0.4.0" from "http://prdownloads.sourceforge.net/ta-lib/ta-lib-0.4.0.jar",
        "joda-time"                 %  "joda-time"          %   "2.1",
//        "org.nuiton.thirdparty"     %  "Rserve"             %   "0.6-1",
//        "org.nuiton.thirdparty"     %  "REngine"            %  "0.6-1",
//        "org.nuiton.thirdparty"     %  "JRI"                %  "0.8-4",
        "org.seleniumhq.selenium"   %  "selenium-java"      %  "2.53.1",
        "net.sourceforge.htmlunit"  %  "htmlunit"           %  "2.12",
//        "org.scalatest"             %  "scalatest_2.9.2"    %   "1.9.1",
        "com.cloudphysics"          %  "jerkson_2.10"       %   "0.6.3",
        "org.scalanlp"              %  "breeze-math_2.10"   %   "0.3",
        "org.scalanlp"              % "breeze-learn_2.10"   %   "0.3",
//        "org.scalanlp"              % "breeze-process_2.10" %   "0.11.2",
        "org.scalanlp"              % "breeze-viz_2.10"     %   "0.11.2"
//        "com.typesafe.akka"         % "akka-actor_2.10"     %   "2.3.3"
//        "org.codehaus.jackson"      %  "jackson-core-asl"   % "1.9.12",
//        "org.codehaus.jackson"      %  "jackson-mapper-asl" % "1.9.12"
  )))

  lazy val listReduce = Project(
    id        = "listReduce",
    base      = file("listReduce"))

}


