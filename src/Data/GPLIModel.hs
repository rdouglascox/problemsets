module Data.GPLIModel where


data Model = Model {domain :: [Int]
                   ,referents :: [(Char, Int)]
                   ,extensions :: [(Char, [[Int]])]
                   }



