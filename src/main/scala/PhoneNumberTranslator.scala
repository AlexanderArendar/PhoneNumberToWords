import scala.io.Source

object PhoneNumberTranslator{

  /**
    * This is just an alias to make the type of the @numberToWordsMap more readable.
    */
  type PhoneNumber = String

  /**
    * This is a standard Loaner pattern to work in clean way with resources that must be closed after usage.
    * @param closeable - any resource implementing the close() method.
    * @param f - a function which operates on that resource and returns some value.
    * @tparam A
    * @tparam B
    * @return - result of function @f application.
    */
  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

  /**
    * loads dictionary from the file in application resources.
    * @return indexed sequence of words from dictionary.
    */
  def loadDictionaryFromFile():IndexedSeq[String] = using(Source.fromResource("words.txt"))(_.getLines().toIndexedSeq)

  /**
    * Validates if the provided input strings is a "valid" phone number in terms of standard dialer digits to letters coding.
    * If the input contains something apart from digits from 2 to 9 plus dash then it throws @IllegalArgumentException.
    * @param input - any String.
    * @return validated phone number stripped from dashes.
    */
  def validateInput(input:String):String =
    if(input.forall(c => (c.isDigit && c != '1' && c != '0') || c == '-'))
      input filter(_ != '-')
    else
      throw new IllegalArgumentException("Program input contains characters different from digits (2 to 9) or dash")

  /**
    * Provides standard dialer coding from digits to letters as a Map.
    */
  val digitToLetterMap:Map[Char, String] = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /**
    * Reverse mapping from letters to digits.
    */
  val letterToDigitMap:Map[Char, Char] = for{
    (digit, letters) <- digitToLetterMap
    letter <- letters
  } yield letter -> digit

  /**
    * Encodes a word to digits using dialer coding.
    * @param word - any String comprised from English letters.
    * @return - String representing a sequence of digits corresponding to provided word. Case insensitive: "APPle" will produce the same result as "appLe".
    */
  def wordToDigits(word:String):String = word.toUpperCase map letterToDigitMap

  /**
    * Dictionary of words loaded from the resource file. Lazily initialized since it consumes a lot of memory.
    * All words which contains something else than letters of English alphabet are removed from the dictionary since they are not meaningful for our task.
    * O(number_of_words_in_file) memory consumption.
    */
  lazy val dictionary:IndexedSeq[String] = loadDictionaryFromFile() filter(_.forall(_.isLetter))

  /**
    * All words from the dictionary grouped by the phone number they are mapped to and arrange as a Map.
    * Usage it to get a collection of all words which encodes to the same phone number or an empty sequence if no English word can be mapped to the provided phone number.
    */
  lazy val numberToWordsMap:Map[PhoneNumber, IndexedSeq[String]] = dictionary groupBy wordToDigits withDefaultValue(IndexedSeq.empty[String])

  /**
    * Finds all possible words combinations which encodes to the provided phone number.
    * Recursive but NOT tail recursive. So on very large inputs it will eventually produce StackOverflowException.
    * Execution time has polynomial complexity over input length so to witness the StackOverflowException one will need to wait a very long time.
    * @param phoneNumber - any valid String representing phone number.
    * @return list of lists of words. Each inner list represents a sequence of words which code to the input phone number.
    */
  def encodeNumber(phoneNumber:PhoneNumber):List[List[String]] = {
    if(phoneNumber.isEmpty) List(Nil) else{
      val result = for{
        position <- 1 to phoneNumber.size
        (left, right) = phoneNumber splitAt(position)
        firstWord <- numberToWordsMap(left)
        nextWords <- encodeNumber(right)
      } yield firstWord :: nextWords
      result.toList
    }
  }

  def main(args:Array[String]):Unit = {
    val input = validateInput(args(0))
    println("**************************result*****************************")
    encodeNumber(input) foreach (list => println(list.mkString("-")))
  }
}