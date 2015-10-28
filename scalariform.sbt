import scalariform.formatter.preferences._
import ScalariformKeys._

defaultScalariformSettings
//scalariformSettings

preferences := preferences.value
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 40)
  .setPreference(CompactControlReadability, false)
  .setPreference(CompactStringConcatenation, false)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(FormatXml, true)
  .setPreference(IndentLocalDefs, false)
  .setPreference(IndentPackageBlocks, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(IndentWithTabs, false)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)
  .setPreference(PreserveDanglingCloseParenthesis, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(PreserveSpaceBeforeArguments, false)
  .setPreference(RewriteArrowSymbols, false)
  .setPreference(SpaceBeforeColon, false)
  .setPreference(SpaceInsideBrackets, false)
  .setPreference(SpaceInsideParentheses, false)
  .setPreference(SpacesWithinPatternBinders, true)
