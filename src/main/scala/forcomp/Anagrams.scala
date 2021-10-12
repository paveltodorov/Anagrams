package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /**
   * Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy(identity)
    .map { case (x, y) => (x, y.length) }.toList.sortWith(_._1 < _._1)

  /** Converts a sentence into its character occurrence list. */
  //def sentenceOccurrences(s: Sentence): Occurrences = s.flatMap(x => wordOccurrences(x)).sortWith(_._1 < _._1)
  /*def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => Nil
    //case head::Nil =>   
    case head::tail => mergeMapOccurences(Map[Char,Int](wordOccurrences(head): _*), sentenceOccurrences(tail)).toList.sortWith(_._1 < _._1)
  }*/
  //wordOccurrences(s.reduce((x,y) => x ++ y)).sortWith(_._1 < _._1)
  // s.map(x => Map(x:_*))

  /*def mergeMapOccurences(occurrence1: Map[Char,Int],occurrence2: List[(Char,Int)]): Map[Char,Int] = occurrence2 match{
   case Nil => occurrence1
   case (char:Char,count:Int)::tail => {
    if(occurrence1.contains(char)) occurrence1 + (char -> (count + occurrence1(char))) ++ tail
    else occurrence1 + (char -> count) ++ tail
   }
  }*/

  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => List()
    case _ => s.map(x => wordOccurrences(x)).reduce((x, y) => mergeOccurences(x, y))
  }

  def mergeOccurences(occurrence1: List[(Char, Int)], occurrence2: List[(Char, Int)]): Occurrences = (occurrence1, occurrence2) match {
    case (x, Nil) => x
    case (Nil, x) => x
    case ((char1, count1) :: tail1, (char2, count2) :: tail2) =>
      if (char1 == char2) (char1, count1 + count2) :: mergeOccurences(tail1, tail2)
      else if (char1 < char2) (char1, count1) :: mergeOccurences(tail1, (char2, count2) :: tail2)
      else (char2, count2) :: mergeOccurences((char1, count1) :: tail1, tail2)
  }
  // a b c
  // b c 

  //.groupBy(case (x,y) => x).map(case((x,y) -> z)) => (x,z)).toList.sortWith(_._1 < _._1)

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word))
  // dictionary.map(word => (wordOccurrences(word) , word)).groupBy(x => x._1)
  //Map( dictionary.map{case word => (wordOccurrences(word) , word)} : _* )//.groupBy(x => x._1)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  //def getAnagramsByOcurrencies()

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list ` ` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case List((char, count)) => List() :: (for (x <- 1 to count) yield List((char, x))).toList
    case (char, count) :: tail => {
      val tailCombinations: List[Occurrences] = combinations(tail)
      val headVariations: Occurrences = (for (x <- 1 to count) yield (char, x)).toList
      val newCombinations: List[Occurrences] = for (x <- headVariations; y <- tailCombinations) yield x :: y //tailCombinations.map(x => (char,count)::x)
      (tailCombinations ++ newCombinations) //.sortWith(_._1 < _._1)
    }
  }
  /*for((char,frequency) <- ocurrencies) {

    }*/

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val mapX = Map(x: _*)
    val mapY = Map(y: _*)
    val transformed =
      for ((char -> count) <- mapX if (!mapY.contains(char) || count - mapY(char) != 0))
        yield (if (mapY.contains(char)) (char, count - mapY(char)) else (char, count))
    transformed.toList.sortWith(_._1 < _._1)
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  /* def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val combinations:List[Occurrences] = combinations(sentenceOccurrences(sentence))
    for(combination <- combinations) {
      combination match {
        case Nil => Nil
        case (head,tail) => {
        val headAnagrams:List[Word] = dictionaryByOccurrences(head)
        val tailAnagrams: List[Sentence] = sentenceAnagrams()
        } 
      }
    }
  }*/

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
    case Nil => List(List())
    case _ => getAnagramsFromSentenceOccurrences(sentenceOccurrences(sentence))
  }

  def getAnagramsFromSentenceOccurrences(occurrences: Occurrences): List[Sentence] = {
    val sentenceCombinations: List[Occurrences] = combinations(occurrences)
    (for (combination <- sentenceCombinations) yield {
      // if((getAnagramByHeadAndTail(combination, subtract(occurrences,combination))) != Nil) 
      //println(getAnagramByHeadAndTail(combination, subtract(occurrences,combination))  + " " + combination + " " + " " + subtract(occurrences,combination))

      getAnagramByHeadAndTail(combination, subtract(occurrences, combination))
    }).flatten.filter(x => x.nonEmpty)
  }

  def getAnagramByHeadAndTail(head: Occurrences, tail: Occurrences): List[Sentence] =
    if (head.nonEmpty && dictionaryByOccurrences.contains(head)) {
      val headAnagrams = dictionaryByOccurrences(head)
      val tailAnagrams = getAnagramsFromSentenceOccurrences(tail)
      //println("head: " + headAnagrams)
      //println("tail: " + tailAnagrams)
      for (x <- headAnagrams; y <- tailAnagrams if x.nonEmpty && y.nonEmpty) yield { x :: y }
    } else if (head.isEmpty && tail.nonEmpty && dictionaryByOccurrences.contains(tail)) dictionaryByOccurrences(tail).map(x => List(x))
    else Nil

  /*def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???//getAnagramsFromSentenceOccurrences(sentenceOccurrences(sentence))

  def getAnagramsFromSentenceOccurrences(occurrences:Occurrences): List[Sentence] = {
    val sentenceCombinations:List[Occurrences] = combinations(occurrences)
    val sentencesAnagrams =  
      for(combination <- sentenceCombinations) 
      //val newSentences: List[Sentence] = combination match {
        yield { 
          combination match {
         case Nil => Nil
         case comb => {
          //println("c" + comb)
          val restOccurences:Occurrences = subtract(occurrences,comb)
          //println("restO  " + occurrences + " " + comb + " " + restOccurences)
          //println("restAnagrams" + restAnagrams)
          if(dictionaryByOccurrences.contains(comb)) {

          val headAnagrams:List[Word] = dictionaryByOccurrences(comb.sortWith(_._1 < _._1))
          val restAnagrams:List[Sentence] = getAnagramsFromSentenceOccurrences(restOccurences)
          //println(headAnagrams)
          //println((for(x <- headAnagrams; y <- restAnagrams) yield x::y) )
          restAnagrams ++ (for(x <- headAnagrams; y <- restAnagrams) yield x::y) 
        }
          else List()
        } 
      }
  } 
 // println(sentencesAnagrams)
  sentencesAnagrams.flatten
}*/

  //sentence match{
  /*case Nil => List()ordOccurrences
    case head::tail => {
      val tailAnagrams:Sentence = sentenceAnagrams(tail)


    }  */
  //}
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}

object Main extends App {
  // Print the trending tweets
  //GoogleVsApple.trending foreach println
  //println("-------------------------------------")
  //println(Anagrams.sentenceAnagrams(List("man")))
  //print(Anagrams.sentenceAnagrams(List("madam")))

  // print(Anagrams.sentenceOccurrences(List("yesa","man")))
  //val l = List("yes","man")
  //print(Anagrams.wordAnagrams("eat"))
  //print(Anagrams.combinations(Anagrams.sentenceOccurrences(l)))
}