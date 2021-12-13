module Settings.PLSettings (Constructor (..), Settings (..), dSettings, settingPS02a, settingPS02b, settingPS04a, settingPS04b) where

import Data.PLprop

data Settings = Settings {minConstr :: Int -- minimum connectives in props
                         ,maxConstr :: Int -- maximum connectives in props
                         ,numProps :: Int -- how many propositions at a time
                         ,includeCons :: [Constructor] -- include connectives (possibly)
                         ,excludeCons :: [Constructor] -- exclude connectives (certainly)
                         ,basics :: String -- basics
                         ,minBranchSet :: Int -- minimum tree proof branches
                         ,maxBranchSet :: Int -- maximum tree proof branches
                         ,minPathSet :: Int -- minium path length
                         ,maxPathSet :: Int -- maximum path length
                         }

dSettings :: Settings
dSettings = Settings {minConstr = 2
                     ,maxConstr = 3
                     ,numProps = 3
                     ,includeCons = [NegConstr Negation
                                  , CondConstr Conditional
                                  , ConjConstr Conjunction
                                  , DisjConstr Disjunction
                                  , BiconConstr Biconditional]
                     ,excludeCons = []
                     ,basics = "ABC"
                     ,minBranchSet = 2
                     ,maxBranchSet = 100
                     ,minPathSet = 0
                     ,maxPathSet = 100
                     }



settingPS04a :: Settings
settingPS04a =
  dSettings
    { numProps = 2,
      basics = "PQ",
      maxConstr = 3,
      includeCons =
        [ NegConstr Negation,
          CondConstr Conditional,
          ConjConstr Conjunction,
          DisjConstr Disjunction
        ]
    }

settingPS04b :: Settings
settingPS04b =
  dSettings
    { numProps = 3,
      minBranchSet = 4,
      maxBranchSet = 5,
      maxConstr = 5
    }

settingPS02b :: Settings
settingPS02b =
  dSettings
    { numProps = 3,
      basics = "XYZ",
      minConstr = 2,
      maxConstr = 3
    }

settingPS02a :: Settings
settingPS02a =
  dSettings
    { numProps = 2,
      basics = "PQ",
      maxConstr = 3,
      includeCons =
        [ NegConstr Negation,
          CondConstr Conditional,
          ConjConstr Conjunction,
          DisjConstr Disjunction
        ]
    }


data Constructor = NegConstr (Prop -> Prop)
                 | ConjConstr (Prop -> Prop -> Prop)
                 | DisjConstr (Prop -> Prop -> Prop)
                 | CondConstr (Prop -> Prop -> Prop)
                 | BiconConstr (Prop -> Prop -> Prop)

instance Eq Constructor where
    NegConstr _ == NegConstr _ = True
    ConjConstr _ == ConjConstr _ = True
    DisjConstr _ == DisjConstr _ = True
    CondConstr _ == CondConstr _ = True
    BiconConstr _ == BiconConstr _ = True
    _ == _ = False
