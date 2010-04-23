<apply template="page">
<div id="haskell-style-guide"
><h1
  >Haskell Style Guide</h1
  ><sub>(adapted from <a
  href="http://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md">Johan
Tibell's style guide</a>.)</sub><p
  >This document describes coding and comment style for the Snap projects. Currently we're more interested in building a working web framework than being code nazis and enforcing this style guide. However, it will be easier on everyone in the long run if contributors follow these guidelines. When something isn't covered by this guide you should stay consistent with the style used in our existing code.</p
  ><div id="table-of-contents"
  ><h2
	>Formatting</h2
    ><div id="line-length"
    ><h3
      >Line Length</h3
      ><p
      >Maximum line length is <em
	>80 characters</em
	>.</p
      ></div
    ><div id="indentation"
    ><h3
      >Indentation</h3
      ><p
      >Tabs are illegal. Use spaces for indenting. Indent your code blocks with <em
	>4 spaces</em
	>. Indent the <code
	>where</code
	> keyword two spaces to set it apart from the rest of the code and indent the definitions in a <code
	>where</code
	> clause 2 spaces. Some examples:</p
      ><pre
      ><code
	>sayHello :: IO ()
sayHello = do
    name &lt;- getLine
    putStrLn $ greeting name
  where
    greeting name = &quot;Hello, &quot; ++ name ++ &quot;!&quot;

filter :: (a -&gt; Bool) -&gt; [a] -&gt; [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
</code
	></pre
      ></div
    ><div id="blank-lines"
    ><h3
      >Blank Lines</h3
      ><p
      >One blank line between top-level definitions. No blank lines between type signatures and function definitions. Add one blank line between functions in a type class instance declaration if the functions bodies are large. Use your judgement.</p
      ></div
    ><div id="whitespace"
    ><h3
      >Whitespace</h3
      ><p
      >Surround binary operators with a single space on either side. Use your better judgment for the insertion of spaces around arithmetic operators but always be consistent about whitespace on either side of a binary operator. Don't insert a space after a lambda.</p
      ></div
    ><div id="data-declarations"
    ><h3
      >Data Declarations</h3
      ><p
      >Align the constructors in a data type definition. Example:</p
      ><pre
      ><code
	>data Tree a = Branch a (Tree a) (Tree a)
            | Leaf
</code
	></pre
      ><p
      >For long type names the following formatting is also acceptable:</p
      ><pre
      ><code
	>data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
</code
	></pre
      ><p
      >Format records as follows:</p
      ><pre
      ><code
	>data Person = Person
    { firstName :: String  -- ^ First name
    , lastName  :: String  -- ^ Last name
    , age       :: Int     -- ^ Age
    } deriving (Eq, Show)
</code
	></pre
      ></div
    ><div id="pragmas"
    ><h3
      >Pragmas</h3
      ><p
      >Put pragmas immediately following the function they apply to. Example:</p
      ><pre
      ><code
	>id :: a -&gt; a
id x = x
{-# INLINE id #-}
</code
	></pre
      ><p
      >In the case of data type definitions you must put the pragma before the type it applies to. Example:</p
      ><pre
      ><code
	>data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
</code
	></pre
      ></div
    ><div id="hanging-lambdas"
    ><h3
      >Hanging Lambdas</h3
      ><p
      >You may or may not indent the code following a &quot;hanging&quot; lambda. Use your judgement. Some examples:</p
      ><pre
      ><code
	>bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -&gt; do
          putStrLn &quot;Here comes a number!&quot;
          print n

foo :: IO ()
foo = alloca 10 $ \a -&gt;
      alloca 20 $ \b -&gt;
      cFunction a b
</code
	></pre
      ></div
    ><div id="export-lists"
    ><h3
      >Export Lists</h3
      ><p
      >Format export lists as follows:</p
      ><pre
      ><code
	>module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
</code
	></pre
      ></div
    ></div
  ><div id="imports"
  ><h2
    >2. Imports</h2
    ><p
    >Imports should be grouped in the following order:</p
    ><ol style="list-style-type: decimal;"
    ><li
      >standard library imports</li
      ><li
      >related third party imports</li
      ><li
      >local application/library specific imports</li
      ></ol
    ><p
    >Put a blank line between each group of imports. The imports in each group should be sorted alphabetically, by module name.</p
    ><p
    >Always use explicit import lists or <code
      >qualified</code
      > imports for standard and third party libraries. This makes the code more robust against changes in these libraries. Exception: The Prelude.</p
    ></div
  ><div id="comments"
  ><h2
    >3. Comments</h2
    ><p
    >You should strive to write self-documenting code. Comments should be more about the <em
      >why</em
      > than the <em
      >what</em
      >. They should make the reader aware of higher-level concerns, ideas, constraints, pitfalls that are not obvious from the code at hand.</p
    ><div id="line-length-1"
    ><h3
      >Line Length</h3
      ><p
      >Maximum line length is <em
	>70 characters</em
	>. This increases readability as the eye has to travel back to the start of the next line.</p
      ></div
    ><div id="punctuation"
    ><h3
      >Punctuation</h3
      ><p
      >Write proper sentences; start with a capital letter and use proper punctuation.</p
      ></div
    ><div id="voice"
    ><h3
      >Voice</h3
      ><p
      >Comments should be written from the perspective of a narrator addressing the user talking about the code. The comment for a function called <em
	>send</em
	> should be &quot;Sends a message...&quot;, not &quot;Send a message...&quot;. It is understood that the subject of the sentence is &quot;The send function&quot;, so those words should be omitted.</p
      ></div
    ><div id="top-level-definitions"
    ><h3
      >Top-Level Definitions</h3
      ><p
      >Comment every top level function (particularly exported functions), and provide a type signature; use Haddock syntax in the comments. Comment every exported data type. Some examples:</p
      ><pre
      ><code
	>-- | Sends a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -&gt; ByteString  -- ^ Data to send
     -&gt; IO Int      -- ^ Bytes sent

-- | Bla bla bla.
data Person = Person
    { age  :: Int     -- ^ Age
    , name :: String  -- ^ First name
    }
</code
	></pre
      ><p
      >For functions the documentation should give enough information to apply the function without looking at the function's definition.</p
      ></div
    ><div id="end-of-line-comments"
    ><h3
      >End-of-Line Comments</h3
      ><p
      >Separate end-of-line comments from the code using 2 spaces. Align comments for data type definitions. Some examples:</p
      ><pre
      ><code
	>data Parser = Parser
    Int         -- Current position
    ByteString  -- Remaining input

foo :: Int -&gt; Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
</code
	></pre
      ></div
    ></div
  ><div id="naming"
  ><h2
    >4. Naming</h2
    ><p
    >Use mixed-case when naming functions and camel-case when naming data types.</p
    ><p
    >For readability reasons, don't capitalize all letters when using an abbreviation. For example, write <code
      >HttpServer</code
      > instead of <code
      >HTTPServer</code
      >. Exception: Two letter abbreviations, e.g. <code
      >IO</code
      >.</p
    ></div
  ><div id="misc"
  ><h2
    >5. Misc</h2
    ><div id="warnings"
    ><h3
      >Warnings</h3
      ><p
      >Code should be compilable with <code
	>-Wall -Werror</code
	>. There should be no warnings.</p
      ></div
    ></div
  ></div
>
</apply>
