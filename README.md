# Tokstyle

C style checker for TokTok projects.

This project uses [cimple](https://hackage.haskell.org/package/cimple), a highly restrictive
simplified C dialect used in [c-toxcore](https://github.com/TokTok/c-toxcore). Tokstyle builds on
top of an already restrictive grammar to do additional semantic checks.

## How to write a new linter

If you want to just get started by copy/pasting an existing one, have a look at one of the many
existing linters in `src/Tokstyle/Linter/`. A simple example is `LoggerCalls.hs`. The tutorial below
walks you through the implementation of another existing linter, which happens to be the simplest
linter we currently have.

Roughly speaking, a linter is a function from `(file path, ast)` to a list of diagnostics
represented simply as `Text`. Typically, we call the linter function `analyse`.

```hs
analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
```

Most linters will be using the `TraverseAst` framework which simplifies tree traversals so you can
match over the entire tree, recursively, with just a few lines of code. You don't have to do this,
but bear in mind that you will need to manually recurse into top level objects like `PreprocIfdef`
to even get at all the top level declarations.

`TraverseAst` provides a traversal object called `AstActions` with functions for all kinds of
elements you might find in the AST. The most commonly used one is `doNode`, which operates on a
single AST node, but if you want to override other functions like `doFile` to handle an entire file
at once, or `doLexeme` if you only want to inspect the tokens, those can be changed in the
definition of your linter. Each of the `do` functions also has a plural form, e.g. `doNodes`, which
as a default is simply iterating over the list, but can be used to operate on the list as a whole.

Let's get started with a simple linter: `CompoundInit`, which checks that we don't write the
following unnecessarily verbose code:

```c
MyType foo = (MyType){0};
```

when instead, we should simply be writing

```c
MyType foo = {0};
```

### Writing the test

We'll start by writing the test first, in a classical Test-Driven Development style. Create a new
file called `test/Tokstyle/Linter/CompoundInitSpec.hs`.

* First, some boilerplate: we need `OverloadedStrings` because we'll use string literals for code
  and expected diagnostics (which are `Text` instead of `String`).
* We name our module according to what we'll be calling our linter module, suffixed with `Spec`.
* Then we import a few `Hspec` functions used in the spec below.
* Then, we need the `analyse` function which dispatches over all linters (we'll add our linter to
  it, later).
* Our last import is the `mustParse` function from `LinterSpec`, which fails the test when parsing
  fails, so we don't need to deal with parse errors. We don't expect parse errors, because we write
  the C code ourselves and it should be parseable.
* Finally, the start of our executable specification: `spec`, which is automatically called by
  `hspec-discover` and hooked up to the test runner.

```hs
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CompoundInitSpec (spec) where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyse)
import           Tokstyle.LinterSpec (mustParse)

spec :: Spec
spec = do
```

Now, for our first test, we write one with a piece of code containing the pattern we want to detect.
In this case, a function containing the above variable declaration. We write this in the `Spec`
monad (using `it` and `shouldBe`).

* First, we parse a piece of code given as `[Text]` into an AST. If that fails, the test stops.
* Otherwise, we continue by calling `analyse` with only the linter we want to test in the allowed
  linter lint. We pass it the `(file path, ast)` tuple. For most linters, the file path won't
  matter, but some (like the logger related ones) may want to exempt some files. This way, you can
  test that the exemption works.
* Finally, we check that the linter gives the diagnostic we expect. We can leave this empty if we
  don't know what the diagnostic will be, yet, or put an empty string in it to make sure it fails
  (TDD: we start with a failing test and make it pass by writing code later).

```hs
    it "detects compound literal initialisers" $ do
        ast <- mustParse
            [ "void f(void) {"
            , "  Foo foo = (Foo){0};"
            , "}"
            ]
        analyse ["compound-init"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: don't use compound literals in initialisations; use simple `Type var = {0};` [-Wcompound-init]"
            ]
```

Typically, we also write a negative test for the pattern we do want to allow. If the developer sees
the above error, they fix it, and then the linter should be silent.

```hs
    it "accepts aggregate initialisers" $ do
        ast <- mustParse
            [ "void f(void) {"
            , "  Foo foo = {0};"
            , "}"
            ]
        analyse ["compound-init"] ("test.c", ast)
            `shouldBe` []
```

### Registering the linter

Now we have our test, which will fail if we run it (using `bazel` below, but you can run it with
`stack` or build and run with `cabal` as well):

```
$ bazel run //hs-tokstyle:testsuite -- --match "/Tokstyle.Linter.CompoundInit/"
...
  hs-tokstyle/test/Tokstyle/Linter/CompoundInitSpec.hs:18:9:
  1) Tokstyle.Linter.CompoundInit detects compound literal initialisers
       expected: ["test.c:2: don't use compound literals in initialisations; use simple `Type var = {0};` [-Wcompound-init]"]
        but got: []
```

It fails because there is no linter for `-Wcompound-init`, so we'll write one:

```hs
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CompoundInit (analyse) where

import           Data.Text       (Text)
import           Language.Cimple (Lexeme (..), Node)

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse _ = []
```

This linter doesn't do anything yet, but we'll register it with the linter list anyway. Open
`src/Tokstyle/Linter.hs`, add a line like

```hs
import qualified Tokstyle.Linter.CompoundInit as CompoundInit
```

to the imports, and then a line like this to the `localLinters` list:

```hs
    , ("compound-init"      , CompoundInit.analyse     )
```

Don't worry about global linters at this point, those are a more advanced (but to be honest not
actually more complicated) feature if you need to do whole program analysis.

Now we've created our linter boilerplate and registered it, we can run the test again, and it will
still fail. The linter exists, but doesn't return any diagnostics. You can try returning one in the
`= []` part, e.g. saying `= ["test.c: oh no!"]`, and now both tests will fail: one has the wrong
diagnostic and the other has a diagnostic when it expects none. Let's write it to actually do
something useful now.

### Traversing the AST

We'll be using the `TraverseAst` framework here, so we'll need some more imports:

```hs
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
```

We also need the AST type constructors to actually perform pattern matching. The Cimple AST uses
fixpoint types, so we'll need `Fix` and the `NodeF` functor constructors (don't worry about what
that means, TL;DR: all the AST node layers have an extra `Fix` in between, useful for recursion
strategies, but we're not using those here).

```hs
import           Data.Fix                    (Fix (..))
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
```

And finally, we will be using the `Diagnostics.warn` helper function which takes care of formatting
diagnostics and adding them to the `State` we'll be using:

```hs
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Language.Cimple.Diagnostics (warn)
```

Next, our actual code, starting with a no-op traversal. The default for `astActions` is to simply
traverse the entire tree and do nothing. `State [Text]` is the monad each of the `do` actions runs
in. This can be anything, and more advanced linters may choose to use a record type to contain more
than just diagnostics, but for now it's just a list of `Text`.

```hs
linter :: AstActions (State [Text]) Text
linter = astActions
```

The `analyse` function now looks like this:

```hs
analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
```

It creates the linter monad using `traverseAst linter` and then runs it with `State.execState` and
the initial state being the empty list `[]` (i.e. no diagnostics). Since `warn` adds diagnostics in
reverse (because prepending to a linked list is `O(1)` while appending is `O(n)`), we need to
reverse the final list when returning it.

Running the test now will still fail, because our linter only does a traversal but doesn't actually
check anything.

### Pattern matching and emitting diagnostics

We need to extend the `astActions` by overriding some of its functions that by default do nothing:

```hs
linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            _ -> act
    }
```

As you can see, the `doNode` function gets the current file path being processed, the current node
being visited, and the recursive action. The default `doNode` ignores `file` and `node` and performs
the recursive action. So, we have just written the default `doNode` implementation. We use `unFix`
here to make the patterns slightly more readable (we still need all the `Fix`es for inner patterns).

Now, let's have a look at what things we might be able to match on by outputting everything this
`doNode` function sees:

```hs
import           Debug.Trace                 (traceShowM)
...
        case unFix node of
            x -> do
                traceShowM x  -- show the node we're visiting
                act           -- still want to recurse
```

Running the test now will, in addition to failing, print a whole bunch of Haskell expressions, each
of which can be copy/pasted as a pattern into the `case unFix node of` we wrote (using `...` here to
skip some uninteresting bits):

```hs
FunctionDefn Global (Fix (FunctionPrototype (Fix ..."void") (L ... "f") ...)) (Fix (CompoundStmt ...))
FunctionPrototype (Fix ..."void") (L ... "f") ...
TyStd (L (AlexPn 0 1 1) KwVoid "void")
CompoundStmt [...]
VarDeclStmt (Fix (VarDecl ..."Foo"... (L ... "foo") [])) (Just (Fix (CompoundLiteral ...)))
VarDecl ..."Foo"...
CompoundLiteral ...
```

Out of all of these, we want the `VarDeclStmt`. Why? Because if we match too deeply, e.g. on a
`VarDecl`, we don't have the initialiser, or if we match the `CompoundLiteral`, we also match them
in expressions outside variable initialisers. So next, we copy/paste the entire Haskell expression
into the `case`:

```hs
        case unFix node of
            VarDeclStmt (Fix (VarDecl ...)) -> do
                traceShowM node
                -- don't recurse further, there's nothing interesting inside, so no "act" here.

            _ -> act  -- recurse for anything not matched
```

Running the test again, we can now see we only output the one node we care about. This means a match
succeeds. You can also check the second test, the negative test which shouldn't match, to make sure
only the first test triggers the `traceShowM` case.

Finally, we want to formulate our diagnostic message using `warn`. We pass the currently processed
file path, the node to use for source locations, and the diagnostic text.

```hs
        case unFix node of
            VarDeclStmt (Fix (VarDecl ...)) -> do
                warn file node "don't use compound literals in initialisations; use simple `Type var = {0};`"

            _ -> act  -- recurse for anything not matched
```

Now run the test, and it should pass. We're done! You have now written your first linter. The sky's
the limit from now on.

### Add files to tokstyle.cabal

The above tutorial mostly assumes `bazel`, which automatically detects new files. To make a pull
request or to run these with `stack` or `cabal`, you'll need to add your new module names to
`tokstyle.cabal` in the `library` and `test-suite` sections.

### Some useful tips

* Have a look at other linters to see how they work with diagnostics. E.g. the
  `Language.Cimple.Pretty` module can be useful for pretty printing nodes as C code.
* As a performance optimisation, stop recursive traversals whenever you've hit your pattern. If you
  know there are large subtrees you will never match in, write a pattern to avoid recursing into
  them (this can save a lot of time if you, for example, only look at top level declarations and
  don't need to inspect `FunctionDefn`s).
* The above is also useful if you have a pattern that's always OK in certain contexts, but not in
  others. You can skip the entire subtree for a node in which it's OK. Example: `LoggerConst.hs`.
* Use `PatternSynonyms` if you need to match the same pattern many times (for an example, look at
  `Booleans.hs`).
* If you need to keep more state during your linter execution, make a `data Linter` with a
  `HasDiagnostics` instance that tells `warn` how to add diagnostics to your data type. See
  `DeclaredOnce.hs` for an example.
* If you write a linter that you don't want to enable by default, add it to the `defaultFlags` list
  in `tools/check-cimple.hs` as `-Wno-my-linter`. Users can still run it explicitly with
 `-Wmy-linter`.
