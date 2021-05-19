module Settings.GPLISettings (Constructor (..), Settings (..),dSettings,settingPS07a,settingPS07b, settingPS08a, settingPS08b, settingPS09a, settingPS09b, settingPS10a, settingPS10b) where

import Trees.GPLItrees
import Data.GPLIprop

-- |Data type for settings
data Settings = Settings
  { minConst :: Int, -- minimum connectives in props
    maxConst :: Int, -- maximum connectives in props
    numProps :: Int, -- how many propositions at a time
    overSize :: Int, -- maximum propositions on tree
    includeRules :: [TreeRule], -- these rules must be applied
    excludeRules :: [TreeRule], -- these rules must not be applied
    includeCons :: [Constructor], -- include certain connectives (possibly)
    excludeCons :: [Constructor], -- exclude certain connectives (certainly)
    variables :: String, -- what are the possible variables
    constants :: String, -- what are the possible constants
    predicats :: String, -- what are the possible predicates
    minArity :: Int, -- what is the minimum arity of props
    maxArity :: Int, -- what is the maximum arity of props
    multiUni :: Int, -- ensure multiple application of uni
    minBranchSet :: Int, -- what is the minimum branches
    maxBranchSet :: Int, -- what is the maximum branches
    minPathSet :: Int, -- what is the minimum path length
    maxPathSet :: Int -- what is the maximum path length
  }

-- |Default settings
dSettings :: Settings
dSettings =
  Settings
    { minConst = 2,
      maxConst = 4,
      numProps = 3,
      overSize = 100,
      includeRules = [], -- e.g. UniversalRule or ConjunctionRule
      excludeRules = [],
      includeCons =
        [ NegConstr Negation,
          CondConstr Conditional,
          ConjConstr Conjunction,
          DisjConstr Disjunction,
          BiconConstr Biconditional,
          ExiConstr Existential,
          UniConstr Universal
        ],
      excludeCons = [],
      variables = "xyz",
      constants = "abc",
      predicats = "FGH",
      multiUni = 0,
      minArity = 1,
      maxArity = 3,
      minBranchSet = 0,
      maxBranchSet = 100,
      minPathSet = 0,
      maxPathSet = 100
    }

-- |mplequiv setting
settingPS07a :: Settings
settingPS07a =
  dSettings
    { excludeRules = [],
      numProps = 2,
      minConst = 4,
      minBranchSet = 2,
      maxBranchSet = 6,
      maxConst = 5,
      maxArity = 1,
      minArity = 1,
      predicats = "G",
      variables = "x",
      constants = "a",
      maxPathSet = 8,
      includeRules = [UniversalRule, ExistentialRule]
    }

-- mplsatg settings

settingPS07b :: Settings
settingPS07b =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 3,
      minConst = 2,
      minBranchSet = 1,
      maxBranchSet = 3,
      maxConst = 3,
      maxArity = 1,
      minArity = 1,
      predicats = "ABC",
      maxPathSet = 8,
      multiUni = 3
      --           ,includeRules = [UniversalRule]
    }


-- mplsat1 settings

settingPS08a :: Settings
settingPS08a =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 1,
      minConst = 2,
      minBranchSet = 0,
      maxBranchSet = 3,
      maxConst = 3,
      maxArity = 1,
      minArity = 1,
      predicats = "ABC",
      maxPathSet = 8
      --           ,includeRules = [UniversalRule]
    }


-- | gpltautg Settings

settingPS09a :: Settings
settingPS09a =
  dSettings
    { excludeRules = [],
      numProps = 1,
      minConst = 2,
      minBranchSet = 2,
      maxBranchSet = 6,
      maxConst = 5,
      multiUni = 2,
      maxArity = 2,
      minArity = 2,
      predicats = "G",
      variables = "x",
      constants = "ab",
      maxPathSet = 15
    }

-- |gplsatg settings

settingPS09b :: Settings
settingPS09b =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 2,
      minConst = 2,
      minBranchSet = 1,
      maxBranchSet = 3,
      maxConst = 3,
      maxArity = 2,
      minArity = 2,
      predicats = "JKL",
      maxPathSet = 8
      --           ,includeRules = [UniversalRule]
    }

-- gplsat1g settings

settingPS08b :: Settings
settingPS08b =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 1,
      minConst = 2,
      minBranchSet = 0,
      maxBranchSet = 3,
      maxConst = 3,
      maxArity = 2,
      minArity = 2,
      predicats = "JKL",
      maxPathSet = 8
      --           ,includeRules = [UniversalRule]
    }

-- | gplisatg settings

settingPS10a :: Settings
settingPS10a =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 2,
      minConst = 2,
      minBranchSet = 1,
      maxBranchSet = 4,
      maxConst = 4,
      maxArity = 2,
      minArity = 2,
      predicats = "HIJK",
      maxPathSet = 8,
      includeRules = [SubstitutionRule]
    }

-- | gplitestg settings

localSettings :: Settings
localSettings =
  dSettings
    { excludeRules = [],
      excludeCons = [],
      numProps = 2,
      minConst = 2,
      minBranchSet = 1,
      maxBranchSet = 3,
      maxConst = 3,
      maxArity = 2,
      minArity = 2,
      predicats = "RI",
      maxPathSet = 8,
      includeRules = [SubstitutionRule]
    }


-- | gplivalg settings

settingPS10b :: Settings
settingPS10b =
  dSettings
    { excludeRules = [],
      numProps = 3,
      minConst = 1,
      minBranchSet = 1,
      maxBranchSet = 50,
      maxConst = 10,
      maxArity = 2,
      minArity = 2,
      predicats = "GI",
      maxPathSet = 40,
      includeRules = [SubstitutionRule]
    }
    








data Constructor = NegConstr (Prop -> Prop)
                 | UniConstr (Char -> Prop -> Prop)
                 | ExiConstr (Char -> Prop -> Prop)
                 | ConjConstr (Prop -> Prop -> Prop)
                 | DisjConstr (Prop -> Prop -> Prop)
                 | CondConstr (Prop -> Prop -> Prop)
                 | BiconConstr (Prop -> Prop -> Prop)

instance Eq Constructor where
    NegConstr _ == NegConstr _ = True
    UniConstr _ == UniConstr _ = True
    ExiConstr _ == ExiConstr _ = True
    ConjConstr _ == ConjConstr _ = True
    DisjConstr _ == DisjConstr _ = True
    CondConstr _ == CondConstr _ = True
    BiconConstr _ == BiconConstr _ = True
    _ == _ = False
 