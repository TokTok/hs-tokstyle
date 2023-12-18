{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.SwitchIfSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "accepts a single if/else" $ do
        ast <- mustParse
            [ "bool a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else {"
            , "    return false;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast) `shouldBe` []

    it "accepts a if/else with only 2 comparisons" $ do
        ast <- mustParse
            [ "bool a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else if (b == THE_BAR) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else {"
            , "    return false;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast) `shouldBe` []

    it "ignores candidates where all branches are single statements" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast) `shouldBe` []

    it "diagnoses a series of if/else-if statements as candidate for switch" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: if-statement could be a switch [-Wswitch-if]"
            ]

    it "diagnoses a series of if/else-if statements ending in `else` as candidate for switch" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  } else {"
            , "    return 3;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: if-statement could be a switch [-Wswitch-if]"
            ]

    it "diagnoses a candidates for switch nested inside another `if`" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b != something) {"
            , "    if (b == THE_FOO) {"
            , "      print_int(b);"
            , "      return 0;"
            , "    } else if (b == THE_BAR) {"
            , "      return 1;"
            , "    } else if (b == THE_BAZ) {"
            , "      return 2;"
            , "    } else {"
            , "      return 3;"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast)
            `shouldBe`
            [ "test.c:3: if-statement could be a switch [-Wswitch-if]"
            ]

    it "diagnoses a candidates for switch nested inside an `else if`" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b != something) {"
            , "    /* nop */"
            , "  } else if (b != another_thing) {"
            , "    if (b == THE_FOO) {"
            , "      print_int(b);"
            , "      return 0;"
            , "    } else if (b == THE_BAR) {"
            , "      return 1;"
            , "    } else if (b == THE_BAZ) {"
            , "      return 2;"
            , "    } else {"
            , "      return 3;"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast)
            `shouldBe`
            [ "test.c:5: if-statement could be a switch [-Wswitch-if]"
            ]

    it "ignores if/else-if statements with different comparison targets" $ do
        ast <- mustParse
            [ "int a(int b, int c) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (c == THE_BAR) {"
            , "    return 1;"
            , "  } else if (c == THE_BAZ) {"
            , "    return 2;"
            , "  } else {"
            , "    return 2;"
            , "  }"
            , "}"
            ]
        analyseLocal ["switch-if"] ("test.c", ast) `shouldBe` []
