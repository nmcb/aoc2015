lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion           := "3.2.0"
             , name                   := "aoc-2015"
             , version                := "0.1.0"
             , libraryDependencies   ++= Library.testDependencies
             , Compile/run/mainClass  := Main.className
    )

scalacOptions ++= Seq(       
  "-encoding", "utf8",        
  "-feature",                 
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation",
//  "-explain",
)

