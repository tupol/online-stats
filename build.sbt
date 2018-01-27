
name := "online-stats"

organization := "tupol"

scalaVersion := "2.11.8"


// ------------------------------
// DEPENDENCIES AND RESOLVERS

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.apache.commons" % "commons-math3" % "3.2" % "test"
)

// ------------------------------
// TESTING
parallelExecution in Test := false

fork in Test := true

// ------------------------------
// TEST COVERAGE

//scoverage.ScoverageKeys.coverageExcludedFiles := ".*BuildInfo.*"


scalacOptions ++= Seq("-feature", "-deprecation")
