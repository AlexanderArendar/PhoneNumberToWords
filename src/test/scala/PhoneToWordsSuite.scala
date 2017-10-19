import org.scalatest.{Matchers, GivenWhenThen, AsyncFlatSpec}
import PhoneNumberTranslator._

class PhoneToWordsSuite extends AsyncFlatSpec with GivenWhenThen with Matchers{

  "System" should "filter out words containing non-letter characters when preparing a dictionary" in{
    Given("a dictionary with words containing non-letter characters")
    val rawDictionary = loadDictionaryFromFile()
    rawDictionary.filter(_ == "home-brew") should not be(empty)
    When("system loads such dictionary and prepares it for usage ")
    val preProcessedDictionary = dictionary
    Then("words with non-letter characters are excluded")
    preProcessedDictionary.filter(_ == "home-brew") should be(empty)
    preProcessedDictionary.filter(_.contains("-")) should be(empty)
    preProcessedDictionary.filter(_.contains("'")) should be(empty)
  }

  "wordToDigits method" should "produce proper result" in{
    Given("a word from English language")
    val word = "apple"
    When("wordToDigits method is applied to this word")
    val result = wordToDigits(word)
    Then("the result shoudl be a proper phone number representing this word")
    result shouldBe "27753"
  }

  "numberToWords(wordToDigits(word))" should "contain initial word" in{
    Given("a word of English language")
    val word = "apple"
    When("a phone number encoding obtained for this word")
    val phoneNumber = wordToDigits(word)
    And("numberToWords map is queried by this phone number")
    val wordsForThePhoneNumber = numberToWordsMap(phoneNumber)
    Then("the original word should be among the returned results")
    wordsForThePhoneNumber should contain(word)
  }

  "System" should "validate provided input and throw exception if invalid" in{
    When("input is provided which contains '0' character the system should throw an IllegalArgumentException")
    assertThrows[IllegalArgumentException]{
      PhoneNumberTranslator.main(Array("34305"))
    }

    When("input is provided which contains '1' character the system should throw an IllegalArgumentException")
    assertThrows[IllegalArgumentException]{
      PhoneNumberTranslator.main(Array("31435"))
    }

    When("input is provided which contains 'a' character the system should throw an IllegalArgumentException")
    assertThrows[IllegalArgumentException]{
      PhoneNumberTranslator.main(Array("34a35"))
    }
  }

  "encodeNumber method" should "return a list containing an empty list when given an empty string input" in{
    When("the input is an empty string then result should be a List(List())")
    encodeNumber("") shouldBe List(Nil)
  }

  "encodeNumber method" should "return a list containing an empty list when given input which does not map to any words" in{
    When("the input is a phone number which does not map to any words then the result should be a List()")
    encodeNumber("777777") shouldBe Nil
  }

  "Mapping result" should "not depend on position of dash symbols in the input" in{
    Given("few versions of the same phone number with different dash position")
    val number1 = "27-753"
    val number2 = "277-53"
    val number3 = "2-77-53"
    When("system maps these two numbers to words")
    val words1 = encodeNumber(validateInput(number1))
    val words2 = encodeNumber(validateInput(number2))
    val words3 = encodeNumber(validateInput(number3))
    Then("results should be equal")
    words1 shouldBe words2
    words1 shouldBe words3
  }

  "The same phone number with dash or without dash" should "be mapped to the same words" in{
    Given("two versions of the same phone number, one of which contains a dash")
    val number1 = "27753"
    val number2 = "277-53"
    When("system maps these two numbers to words")
    val words1 = encodeNumber(validateInput(number1))
    val words2 = encodeNumber(validateInput(number2))
    Then("results should be equal")
    words1 shouldBe words2
  }

  "System" should "correctly map a valid phone number to words" in{
    Given("a valid phone number")
    val phoneNumber = "73285427"
    When("system maps it to words")
    val words = encodeNumber(validateInput(phoneNumber))
    Then("all possible combinations should be included in the result")
    words.toSet shouldBe Set(
      List("re", "at", "liar"),
      List("peat", "liar"),
      List("seat", "liar"),
      List("sect", "liar"),
      List("peculiar")
    )
  }
}