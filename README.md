An attempt to translate Haskell code in [this paper](https://sebfisch.github.io/haskell-regexp/regexp-play.pdf) in the Scala language.
Nontrivial aspects (for me) have been:

 - Encoding of type class hierarchies, solved thanks to [this post](https://contributors.scala-lang.org/t/encoding-type-class-hierarchies/4626/14)
 - Rewriting the code in MarkedWeightedRegexInf.scala to support infinite regular expressions
   - the code runs the examples in the paper, however the amount of laziness (i.e., call-by-name parameters) required to run the code correctly 
     has been determined by trial and error. LazySemiring was essential for that (i.e. without it, the code would not run), not sure if the
     alternative definitions of REGW and REG-derived case classes were necessary as well



