name := "Mini_ContextProject"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.2"

libraryDependencies += "com.typesafe.akka" % "akka-slf4j_2.11" %  "2.4.2"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" %  "2.4.2"

libraryDependencies += "com.h2database" % "h2" % "1.3.176"

libraryDependencies += "com.typesafe.slick" % "slick_2.11" % "2.1.0"

libraryDependencies += "com.github.tototoshi" %% "slick-joda-mapper" % "1.2.0"

libraryDependencies += "org.joda" % "joda-convert" % "1.7"

libraryDependencies += "joda-time" % "joda-time" % "2.8.2"

libraryDependencies += "com.mchange" % "c3p0" % "0.9.5.2"

libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.3"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"



