name := "unfiltered-websockets"

organization := "net.databinder"

scalaVersion := "2.9.0-1"

version := "0.5.0.2"

publishTo := Option(Resolver.file("gitpages-local", Path.userHome / "tmp" / "repository"))

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-netty-server" % "0.5.0",
  "net.databinder" %% "unfiltered-netty" % "0.5.0"
)

