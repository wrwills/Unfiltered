import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "net.databinder"
  val buildScalaVersion = "2.9.0-1"
  val buildVersion      = "0.3.4-SNAPSHOT"
  val buildName         = "Unfiltered Web Toolkit"

  val buildSettings = Defaults.defaultSettings ++ Seq (organization := buildOrganization,
						       scalaVersion := buildScalaVersion,
						       version      := buildVersion,
						       shellPrompt  := ShellPrompt.buildShellPrompt,
                                                       name         := buildName)

}

object ShellPrompt {
  
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  
  val current = """\*\s+(\w+)""".r
  
  def gitBranches = ("git branch --no-color" lines_! devnull mkString)
    
    val buildShellPrompt = { 
      (state: State) => {
        val currBranch = current findFirstMatchIn gitBranches map (_ group(1)) getOrElse "-"
        val currProject = Project.extract (state).currentProject.id
        "%s:%s:%s> ".format (currProject, currBranch, BuildSettings.buildVersion)
      }
    }
  
}

object Resolvers {

  val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
  val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
  val outsideInSnapshotsRepo = "OI Snapshot Repo" at "http://aws-gem-server1:8081/nexus/content/repositories/outside-in-snapshots/"
  val outsideInReleasesRepo = "OI Releases Repo" at "http://aws-gem-server1:8081/nexus/content/repositories/outside-in-releases/"
  val twitterRepo = "Twitter Repo" at "http://maven.twttr.com/"

  val oracleResolvers = Seq(sunrepo, sunrepoGF, oraclerepo)
  val oiResolvers = Seq(outsideInSnapshotsRepo, outsideInReleasesRepo)
  val twitterResolvers = Seq(twitterRepo)
}

object Dependencies {
  val commonsCodecVersion = "1.4"
  val commonsFileuploadVersion = "1.2.1"
  val commonsIoVersion = "1.4"
  val dispatchVersion = "0.7.8"
  val jettyVersion = "7.2.2.v20101205"
  val liftJsonVersion = "2.2"
  val nettyVersion = "3.2.4.Final"
  val scalateVersion = "1.5.0"
  val scalatestVersion = "1.3"
  val servletApiVersion = "2.3"
  val specsVersion = "1.6.7"

  val commonsCodec = "commons-codec" % "commons-codec" % commonsCodecVersion
  val commonsFileupload = "commons-fileupload" % "commons-fileupload" % commonsFileuploadVersion
  val commonsIo = "commons-io" % "commons-io" % commonsIoVersion
  val databinderMime =  "net.databinder" %% "dispatch-mime" % dispatchVersion
  val databinderOAuth =  "net.databinder" %% "dispatch-oauth" % dispatchVersion
  val jettyArtefact = "org.eclipse.jetty" % "jetty-webapp" % jettyVersion
  val jettyAjp = "org.eclipse.jetty" % "jetty-ajp" % jettyVersion
  val liftJson = "net.liftweb" % "lift-json_2.8.1" % liftJsonVersion
  val nettyArtefact = "org.jboss.netty" % "netty" % nettyVersion
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % BuildSettings.buildScalaVersion % "test"
  val scalateLibs = "org.fusesource.scalate" % "scalate-core" % scalateVersion
  val scalateUtils = "org.fusesource.scalate" % "scalate-util" % scalateVersion % "test"
  val scalatest = "org.scalatest" % "scalatest" % scalatestVersion
  val servletApi = "javax.servlet" % "servlet-api" % servletApiVersion % "provided"
  val specs = "org.scala-tools.testing" % "specs_2.8.1" % specsVersion
}


object CosmosBuild extends Build {
  val buildShellPrompt = ShellPrompt.buildShellPrompt
  
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val filterDeps = Seq(servletApi)
  val jettyAjpDeps = Seq(jettyAjp)
  val jettyDeps = Seq(jettyArtefact)
  val jsonDeps = Seq(liftJson)
  val libraryDeps = Seq(commonsCodec)
  val nettyDeps = Seq(nettyArtefact)
  val oauthDeps = Seq(databinderOAuth)
  val scalaTestDeps = Seq(scalatest,databinderMime)
  val scalateDeps = Seq(scalateLibs,scalateUtils,scalaCompiler)
  val specDeps = Seq(specs,databinderMime)
  val uploadDeps = Seq(servletApi,commonsIo,commonsFileupload)

  lazy val unfiltered = Project("Unfiltered",file("."), settings = buildSettings) aggregate (library,filterP,uploads,util,jetty,jettyAjpProject,netty,nettyServer,jsonProject,specHelpers,scalaTestHelpers,scalate,websockets,oauth)
  lazy val library = Project("library",
                             file("library"),
                             settings = buildSettings ++ Seq(name := "Unfiltered Library",
                                                             libraryDependencies := libraryDeps)) dependsOn(util)
  lazy val filterP = Project("filter",
                             file("filter"),
                             settings = buildSettings ++ Seq(name := "Servlet Filter",
                                                             libraryDependencies := filterDeps)) dependsOn(library)
  lazy val uploads = Project("uploads",
                             file("uploads"),
                             settings = buildSettings ++ Seq(name := "Servlet File Upload",
                                                             libraryDependencies := uploadDeps)) dependsOn(filterP)
  lazy val util = Project("util",
                          file("util"),
                          settings = buildSettings ++ Seq(name := "Unfiltered utils"))
  lazy val jetty = Project("jetty",
                           file("jetty"),
                           settings = buildSettings ++ Seq(name := "Jetty",
                                                           libraryDependencies := jettyDeps)) dependsOn(util)
  lazy val jettyAjpProject = Project("jetty-ajp",
                                     file("jetty-ajp"),
                                     settings = buildSettings ++ Seq(name := "Jetty AJP",
                                                                     libraryDependencies := jettyAjpDeps)) dependsOn(jetty)
  lazy val nettyServer = Project("netty-server",
                                 file("netty-server"),
                                 settings = buildSettings ++ Seq(name := "Unfiltered Netty Server",
                                                                 libraryDependencies := nettyDeps)) dependsOn(util)
  lazy val netty = Project("netty",
                           file("netty"),
                           settings = buildSettings ++ Seq(name := "Unfiltered Netty")) dependsOn(nettyServer,library)
  lazy val specHelpers = Project("spec",
                                 file("spec"),
                                 settings = buildSettings ++ Seq(name := "Unfiltered Spec Helpers",
                                                                 libraryDependencies := specDeps)) dependsOn(jetty,netty)
  lazy val scalaTestHelpers = Project("scalatest",
                                      file("scalatest"),
                                      settings = buildSettings ++ Seq(name := "Unfiltered Scalatest Helpers",
                                                                      libraryDependencies := scalaTestDeps)) dependsOn(jetty,netty)
  lazy val scalate = Project("scalate",
                             file("scalate"),
                             settings = buildSettings ++ Seq(name := "Unfiltered Scalate",
                                                             libraryDependencies := scalateDeps)) dependsOn(library)
  lazy val websockets = Project("websockets",
                                file("websockets"),
                                settings = buildSettings ++ Seq(name := "Unfiltered Websockets")) dependsOn(netty)
  lazy val oauth = Project("oauth",
                           file("oauth"),
                           settings = buildSettings ++ Seq(name := "Unfiltered OAuth")) dependsOn(jetty,filterP)
  lazy val jsonProject = Project("json",
                                 file("json"),
                                 settings = buildSettings ++ Seq(name := "Unfiltered JSON",
                                                                 libraryDependencies := jsonDeps)) dependsOn(library)
}
