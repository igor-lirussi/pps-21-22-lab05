package u05lab.ex2

object  Conference:
    enum Question:
        case RELEVANCE
        case SIGNIFICANCE
        case CONFIDENCE
        case FINAL


    trait ConferenceReviewing:
        def loadReview( article :Int, scores: Map[Question, Int]):Unit
        def loadReview(art: Int, rel: Int, sign: Int, conf: Int, fin: Int): Unit
        def orderedScores(art: Int, quest: Question): List[Int]
        def averageFinalScore(art: Int): Double
        def acceptedArticles(): Set[Int]
        def sortedAcceptedArticles(): List[(Int, Double)]
        def averageWeightedFinalScoreMap(): Map[Int, Double]

    case class ConferenceRev() extends ConferenceReviewing:

        var reviewList: List[(Int, Map[Question, Int])] = List()

        /** loads a review for the specified article, with complete scores as a map  */
        override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
            reviewList = reviewList:+(article, scores)

        /** loads a review for the specified article, with the 4 explicit scores */
        override def loadReview(article: Int, rel: Int, sign: Int, conf: Int, fin: Int): Unit =
            reviewList = reviewList:+(article, Map((Question.RELEVANCE, rel),(Question.SIGNIFICANCE, sign),(Question.CONFIDENCE, conf),(Question.FINAL, fin)))

        /** @return the scores given to the specified article and specified question, as an (ascending-ordered) list  */
        override def orderedScores(art: Int, quest: Question): List[Int] =
            reviewList.collect( {case (artNum, mapQV) if artNum==art && mapQV.contains(quest) => mapQV(quest) }).sorted

        /** @return the average score to question FINAL taken by the specified article */
        override def averageFinalScore(art: Int): Double =
            val accumulator = reviewList.foldLeft((0.0,0))((acc, elem) => elem match
                case (article, mapQV) if article == art => (acc._1 + mapQV(Question.FINAL), acc._2+1)
                case _ => acc
            )
            accumulator._1/accumulator._2

        def atLeastOneRelevanceScoreMoreEqualThan(score: Int)(art: Int): Boolean =
            reviewList.filter((article, mapQV) =>
                article == art && mapQV(Question.RELEVANCE) >= score).nonEmpty

        /** An article is considered accept if its averageFinalScore (not weighted) is > 5, and at least one RELEVANCE score that is >= 8.
         * @return the set of accepted articles */
        override def acceptedArticles(): Set[Int] =
            reviewList.filter((art, _) => averageFinalScore(art)>5 && atLeastOneRelevanceScoreMoreEqualThan(8)(art) ).map(_._1).toSet

        /** @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore */
        override def sortedAcceptedArticles(): List[(Int, Double)] =
            acceptedArticles().map(art=>(art, averageFinalScore(art))).toList.sortBy(_._2)

        def averageWeightedFinalScore(art:Int): Double =
            val accumulator = reviewList.foldLeft((0.0, 0))((acc, elem) => elem match
                case (article, mapQV) if article == art => (acc._1 + (mapQV(Question.CONFIDENCE)*mapQV(Question.FINAL))/10.0, acc._2 + 1)
                case _ => acc
            )
            accumulator._1 / accumulator._2

        /** @return a map from articles to their average "weighted final score", namely, the average value of CONFIDENCE*FINAL/10 */
        override def averageWeightedFinalScoreMap(): Map[Int, Double] =
            reviewList.toMap.map((art,_)=>(art, averageWeightedFinalScore(art)))



        override def toString: String = reviewList.foldLeft("\nart|REL\tSIG\tCON\tFIN")( (acc,rev)=>acc+"\n"+rev._1+" |"+
          rev._2.foldLeft("")( (acc,couple)=>acc+"\t"+couple._2) )

        //NOTE: averageFinalScore and averageWeightedFinalScore could be unified in a function for average per article getting a strategy for every review
        def averageScoreWith(scoreStrategyForArticle:Map[Question,Int]=>Double)(art: Int): Double =
            val accumulator = reviewList.foldLeft((0.0, 0))((acc, elem) => elem match
                case (article, mapQV) if article==art => (acc._1 + scoreStrategyForArticle(mapQV), acc._2 + 1)
                case _ => acc
            )
            accumulator._1 / accumulator._2

        def averageFinalScore2(art: Int): Double = averageScoreWith(mapQV=>mapQV(Question.FINAL))(art)
        def averageWeightedFinalScore2(art:Int): Double = averageScoreWith(mapQV=>mapQV(Question.CONFIDENCE)*mapQV(Question.FINAL)/10.0)(art)