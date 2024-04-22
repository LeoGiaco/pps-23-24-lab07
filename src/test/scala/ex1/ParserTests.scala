package ex1

import org.scalatest.matchers.should.Matchers.*
import Parsers.*

class ParserTests extends org.scalatest.flatspec.AnyFlatSpec:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  // linearization: NTCNEParser -> NonEmpty -> NotTwoConsecutive -> BasicParser -> Parser
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]  
  def sparser: Parser[Char] = "abc".charParser()

  def STNParser = new BasicParser(Set('a', 'b', 'c')) with ShorterThanN[Char](5)

  // @Test
  // def testBasicParser =
  //   assertTrue(parser.parseAll("aabc".toList))
  //   assertFalse(parser.parseAll("aabcdc".toList))
  //   assertTrue(parser.parseAll("".toList))

  "A BasicParser" should "parse correctly" in:
    parser.parseAll("aabc".toList) should be (true)
    parser.parseAll("aabcdc".toList) should be (false)
    parser.parseAll("".toList) should be (true)

  // @Test
  // def testNotEmptyParser =
  //   assertTrue(parserNE.parseAll("0101".toList))
  //   assertFalse(parserNE.parseAll("0123".toList))
  //   assertFalse(parserNE.parseAll(List()))

  "A NotEmptyParser" should "parse correctly while failing empty strings" in:
    parserNE.parseAll("0101".toList) should be (true)
    parserNE.parseAll("0123".toList) should be (false)
    parserNE.parseAll(List()) should be (false)

  // @Test
  // def testNotTwoConsecutiveParser =
  //   assertTrue(parserNTC.parseAll("XYZ".toList))
  //   assertFalse(parserNTC.parseAll("XYYZ".toList))
  //   assertTrue(parserNTC.parseAll("".toList))

  "A NotTwoConsecutiveParser" should "parse correctly while failing consecutive characters" in:
    parserNTC.parseAll("XYZ".toList) should be (true)
    parserNTC.parseAll("XYYZ".toList) should be (false)
    parserNTC.parseAll("".toList) should be (true)

  // @Test
  // def testNotEmptyAndNotTwoConsecutiveParser =
  //   assertTrue(parserNTCNE.parseAll("XYZ".toList))
  //   assertFalse(parserNTCNE.parseAll("XYYZ".toList))
  //   assertFalse(parserNTCNE.parseAll("".toList))

  "A NotEmptyAndNotTwoConsecutiveParser" should "parse correctly while failing consecutive characters in empty strings" in:
    parserNTCNE.parseAll("XYZ".toList) should be (true)
    parserNTCNE.parseAll("XYYZ".toList) should be (false)
    parserNTCNE.parseAll("".toList) should be (false)

  // @Test
  // def testStringParser =
  //   assertTrue(sparser.parseAll("aabc".toList))
  //   assertFalse(sparser.parseAll("aabcdc".toList))
  //   assertTrue(sparser.parseAll("".toList))

  "A parser from string" should "parse correctly" in:
    sparser.parseAll("aabc".toList) should be (true)
    sparser.parseAll("aabcdc".toList) should be (false)
    sparser.parseAll("".toList) should be (true)

  "A ShorterThanNParser" should "parse correctly while failing string longer than N" in:
    STNParser.parseAll("aabca".toList) should be (true)
    STNParser.parseAll("aabcaa".toList) should be (false)
    STNParser.parseAll("".toList) should be (true)
