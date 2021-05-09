module Translations.Sentences (Sentence (..),NounPhrase (..), ProperNoun (..), CommonNoun (..), VerbPhrase (..), Verb (..)) where 

data Sentence = AtomicSentence NounPhrase VerbPhrase                  -- John is running
              | NegatedSentence Sentence                              -- John is not running
              | ConditionalSentence Sentence Sentence                 -- If John is running then Jane is running 
              | BiconditionalSentence Sentence Sentence               -- John is running if and only if Jane is running
              | ConjunctiveSentence Sentence Sentence                 -- John is jumping and Jane is running
              | DisjunctiveSentence Sentence Sentence                 -- John is running or Jane is running
              deriving (Show, Eq)

data NounPhrase = Proper ProperNoun                                   -- "John"
                | Definite CommonNoun                                 -- "the animal" 
                | Indefinite CommonNoun                               -- "an apple" 
                | ConjunctiveNounPhrase NounPhrase NounPhrase         -- "an apple and a bannana"
                | DisjunctiveNounPhrase NounPhrase NounPhrase         -- "an apple or a bannana"
                deriving (Show, Eq)

data ProperNoun = ProperNoun String deriving (Show, Eq)               -- "John", "Jane"


data CommonNoun = CommonNoun String deriving (Show, Eq)               -- "animal", "apple"
 
data VerbPhrase = IntransitiveVerbPhrase Verb                         -- "is running" / "are running"
                | TransitiveVerbPhrase Verb NounPhrase                -- "is running with a tiger" / "are runing with a tiger"
                | NegatedVerbPhrase VerbPhrase                        -- "is not VP"
                | ConjunctiveVerbPhrase VerbPhrase VerbPhrase         -- " is running with a tiger and is not eating" / are not
                | DisjunctiveVerbPhrase VerbPhrase VerbPhrase         -- " is running with a tiger or is running / are not  
                deriving (Show, Eq)

data Verb = Verb String deriving (Show, Eq)                           -- "running", "running with" 


 
