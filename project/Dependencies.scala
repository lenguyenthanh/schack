import sbt._

object Deps {

  object V {
    val cats            = "2.7.0"
    val catsParse       = "0.3.9"
    val organizeImports = "0.6.0"
  }

  object Libs {

    val cats      = "org.typelevel" %% "cats-core"  % V.cats
    val catsParse = "org.typelevel" %% "cats-parse" % V.catsParse

    val munit           = "org.scalameta"           %% "munit"            % "0.7.29"     % Test
    val scalaCheck      = "org.scalacheck"          %% "scalacheck"       % "1.16.0"     % Test
    val munitScalaCheck = "org.scalameta"           %% "munit-scalacheck" % "0.7.29"     % Test
    val compression     = "com.github.lenguyenthanh" % "compression"      % "aacf55bea2" % Test

    // scalafix rules
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
  }

}
