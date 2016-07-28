{-# LANGUAGE BangPatterns #-}
{-
   A Philip Wadler style prettyprinter with location annotations for source
   maps.

   Based on the wl-pprint implementation by Daan Leijen
 -}

module GHCJS.Tyr.Doc ( Doc(..) -- fixme, hide?
                     , FormattedDoc
                     , formatCompact
                     , formatIndent
                     -- output to Handle or ByteString
                     , buildDoc
                     , buildDocWithMap
                     , hPutDoc
                     , hPutDocWithMap
                     -- source location annotation
                     , loc
                     -- construction
                     , sep
                     , fillSep
                     , hsep
                     , vsep
                     , cat
                     , hcat
                     , vcat
                     , fold
                     , (<+>),  (<!+>)
                     , (</>),  (<!/>)
                     , (<//>), (<!//>)
                     , (<$>),  (<!$>)
                     , (<$$>), (<!$$>)
                     , softline
                     , softbreak
                     , enclose
                     , fillBreak
                     , fill
                     , width
                     , indent
                     , hang
                     , align
                     , char
                     , string
                     , text
                     , lit
                     , line
                     , linebreak
                     , beside
                     , empty
                     , nest
                     , column
                     , nesting
                     , group
                     , flatten
                     , squotes, dquotes, braces, parens, angles, brackets
                     -- some terminals
                     , lparen, rparen
                     , langle, rangle
                     , lbrace, rbrace
                     , lbracket, rbracket
                     , squote, dquote, semi, colon, comma, space
                     , dot, backslash, equals, slash, question
                     ) where

import           Prelude hiding ((<$>))

import           Control.Exception (evaluate)
import           Control.DeepSeq
import           Control.Monad

import           Data.List (intersperse)
import           Data.Monoid

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL

import qualified Data.ByteString.Builder as BB

import           System.IO

import qualified Panic


import           GHCJS.Tyr.Base (Pos(..), L(..), Span(..))

panic :: String -> a
panic = Panic.panic . ("GHCJS.Tyr.Doc."++)

data Doc = Empty
         | Location !L !Doc   -- location annotation
         | Char !Char             -- invariant: char is not '\n'
         | Text !Int !Builder     -- invariant: text doesn't contain '\n'
         | Line !Bool            -- True <=> when undone by group, do not insert a space
         | Cat !Doc !Doc
         | Nest !Int !Doc
         | Union !Doc !Doc         -- invariant: first lines of first doc longer than the first lines of the second doc
         | Column  (Int -> Doc)
         | Nesting (Int -> Doc)
         | Spaces !Int
         | Optional !Doc         -- add for readability when prettyprinting but leave out when compact printing

instance Show Doc where
  show Empty    = "Empty"
  show (Char c) = "Char '" ++ [c] ++ "'"
  show (Text i t) = "Text " ++ show i ++ " " ++ show t
  show (Line b) = "Line " ++ show b
  show (Cat a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Nest n b) = "<" ++ show n ++ ", " ++ show b ++ ">"
  show (Union a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
  show (Column _)  = "Column"
  show (Nesting _) = "Nesting"
  show (Spaces n) = "Spaces " ++ show n
  show (Optional a) = "{" ++ show a ++ "}"

instance Monoid Doc where
  mappend = beside
  mempty  = Empty

loc :: L -> Doc -> Doc
loc s doc = Location s doc

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right sep ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right)


-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
-- document @p@ except for the last document.
--
-- > someText = map text ["words","in","a","tuple"]
-- > test     = parens (align (cat (punctuate comma someText)))
--
-- This is layed out on a page width of 20 as:
--
-- @
-- (words,in,a,tuple)
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- (words,
--  in,
--  a,
--  tuple)
-- @
--
-- (If you want put the commas in front of their elements instead of
-- at the end, you should use 'tupled' or, in general, 'encloseSep'.)
{-
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds
-}

-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @(\<$\>)@.
--
-- > sep xs  = group (vsep xs)
sep :: [Doc] -> Doc
sep             = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, than
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (\<\/\>) empty xs
fillSep :: [Doc] -> Doc
fillSep         = fold (</>)

hsep :: [Doc] -> Doc
hsep            = fold (<+>)

vsep :: [Doc] -> Doc
vsep            = fold (<$>)

cat :: [Doc] -> Doc
cat             = group . vcat

{-
-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, than inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (\<\/\/\>) empty xs
fillCat :: [Doc] -> Doc
fillCat         = fold (<//>)
-}

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: [Doc] -> Doc
hcat            = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @(\<$$\>)@. If a 'group' undoes the line breaks
-- inserted by @vcat@, all documents are directly concatenated.
vcat :: [Doc] -> Doc
vcat            = fold (<$$>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold _ []       = empty
fold f ds       = foldr1 f ds

-- | suggested space, left out when compact printing
(<+>) :: Doc -> Doc -> Doc
x <+> y         = x <> Optional space <> y

-- | required space
(<!+>) :: Doc -> Doc -> Doc
x <!+> y         = x <> space <> y

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@ with a
-- 'softline' in between. This effectively puts @x@ and @y@ either
-- next to each other (with a @space@ in between) or underneath each
-- other. (infixr 5)
(</>) :: Doc -> Doc -> Doc
x </> y         = x <> Optional softline <> y

(<!/>) :: Doc -> Doc -> Doc
x <!/> y         = x <> softline <> y

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@ with
-- a 'softbreak' in between. This effectively puts @x@ and @y@ either
-- right next to each other or underneath each other. (infixr 5)
(<//>) :: Doc -> Doc -> Doc
x <//> y        = x <> Optional softbreak <> y

(<!//>) :: Doc -> Doc -> Doc
x <!//> y        = x <> softbreak <> y

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
-- fixme rename this
(<$>) :: Doc -> Doc -> Doc
x <$> y         = x <> Optional line <> y

(<!$>) :: Doc -> Doc -> Doc
x <!$> y         = x <> line <> y

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
(<!$$>) :: Doc -> Doc -> Doc
x <!$$> y        = x <> linebreak <> y

(<$$>) :: Doc -> Doc -> Doc
x <$$> y        = x <> Optional linebreak <> y

-- | The document @softline@ behaves like 'space' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: Doc
softline        = group line

-- | The document @softbreak@ behaves like 'empty' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softbreak  = group linebreak
softbreak :: Doc
softbreak       = group linebreak

squotes, dquotes, braces, parens, angles, brackets :: Doc -> Doc
squotes         = enclose squote squote
dquotes         = enclose dquote dquote
braces          = enclose lbrace rbrace
parens          = enclose lparen rparen
angles          = enclose langle rangle
brackets        = enclose lbracket rbracket

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x   = l <> x <> r

lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket :: Doc
lparen          = char '('
rparen          = char ')'
langle          = char '<'
rangle          = char '>'
lbrace          = char '{'
rbrace          = char '}'
lbracket        = char '['
rbracket        = char ']'

squote, dquote, semi, colon, comma, space, dot, backslash, equals, slash, question :: Doc
squote          = char '\''
dquote          = char '"'
semi            = char ';'
colon           = char ':'
comma           = char ','
space           = char ' '
dot             = char '.'
backslash       = char '\\'
equals          = char '='
slash           = char '/'
question        = char '?'

-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
-- than appends @space@s until the width is equal to @i@. If the
-- width of @x@ is already larger than @i@, the nesting level is
-- increased by @i@ and a @line@ is appended. When we redefine @ptype@
-- in the previous example to use @fillBreak@, we get a useful
-- variation of the previous output:
--
-- > ptype (name,tp)
-- >        = fillBreak 6 (text name) <+> text "::" <+> text tp
--
-- The output will now be:
--
-- @
-- let empty  :: Doc
--     nest   :: Int -> Doc -> Doc
--     linebreak
--            :: Doc
-- @
fillBreak :: Int -> Doc -> Doc
fillBreak f x   = width x (\w ->
                  if (w > f) then nest f linebreak
                             else Spaces (f - w))


-- | The document @(fill i x)@ renders document @x@. It than appends
-- @space@s until the width is equal to @i@. If the width of @x@ is
-- already larger, nothing is appended. This combinator is quite
-- useful in practice to output a list of bindings. The following
-- example demonstrates this.
--
-- > types  = [("empty","Doc")
-- >          ,("nest","Int -> Doc -> Doc")
-- >          ,("linebreak","Doc")]
-- >
-- > ptype (name,tp)
-- >        = fill 6 (text name) <+> text "::" <+> text tp
-- >
-- > test   = text "let" <+> align (vcat (map ptype types))
--
-- Which is layed out as:
--
-- @
-- let empty  :: Doc
--     nest   :: Int -> Doc -> Doc
--     linebreak :: Doc
-- @
fill :: Int -> Doc -> Doc
fill f d        = width d (\w ->
                  if (w >= f) then empty
                              else Spaces (f - w))

width :: Doc -> (Int -> Doc) -> Doc
width d f       = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
-- > test  = indent 4 (fillSep (map text
-- >         (words "the indent combinator indents these words !")))
--
-- Which lays out with a page width of 20 as:
--
-- @
--     the indent
--     combinator
--     indents these
--     words !
-- @
indent :: Int -> Doc -> Doc
indent i d      = hang i (Optional (Spaces i) <> d)

-- | The hang combinator implements hanging indentation. The document
-- @(hang i x)@ renders document @x@ with a nesting level set to the
-- current column plus @i@. The following example uses hanging
-- indentation for some text:
--
-- > test  = hang 4 (fillSep (map text
-- >         (words "the hang combinator indents these words !")))
--
-- Which lays out on a page with a width of 20 characters as:
--
-- @
-- the hang combinator
--     indents these
--     words !
-- @
--
-- The @hang@ combinator is implemented as:
--
-- > hang i x  = align (nest i x)
hang :: Int -> Doc -> Doc
hang i d        = align (nest i d)

-- | The document @(align x)@ renders document @x@ with the nesting
-- level set to the current column. It is used for example to
-- implement 'hang'.
--
-- As an example, we will put a document right above another one,
-- regardless of the current nesting level:
--
-- > x $$ y  = align (x <$> y)
--
-- > test    = text "hi" <+> (text "nice" $$ text "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: Doc -> Doc
align d         = column (\k ->
                  nesting (\i -> nest (k - i) d))   --nesting might be negative :-)

data FormattedDoc
  = FNil
  | FChar          !Char          FormattedDoc
  | FText          !Int  !Builder FormattedDoc
  | FLine          !Int           FormattedDoc
  | FSpaces        !Int           FormattedDoc
  | FLocationStart !L             FormattedDoc
  | FLocationEnd                  FormattedDoc
  deriving (Show)

-- | The empty document is, indeed, empty. Although @empty@ has no
-- content, it does have a \'height\' of 1 and behaves exactly like
-- @(text \"\")@ (and is therefore not a unit of @\<$\>@).
empty :: Doc
empty = Empty

char :: Char -> Doc
char '\n'       = line
char c          = Char c

string :: String -> Doc
string = text . T.pack

text :: Text -> Doc
text = mconcat . intersperse line
               . map lit
               . T.lines

-- | literals cannot contain newlines, use text instead for values that
--   might contain them.
lit :: Text -> Doc
lit l | T.null l  = Empty
      | otherwise = Text (T.length l) (B.fromText l)

-- | The @line@ document advances to the next line and indents to the
-- current nesting level. Document @line@ behaves like @(text \" \")@
-- if the line break is undone by 'group'.
line :: Doc
line = Line False

-- | The @linebreak@ document advances to the next line and indents to
-- the current nesting level. Document @linebreak@ behaves like
-- 'empty' if the line break is undone by 'group'.
linebreak :: Doc
linebreak       = Line True

beside :: Doc -> Doc -> Doc
beside x y      = Cat x y

-- | The document @(nest i x)@ renders document @x@ with the current
-- indentation level increased by i (See also 'hang', 'align' and
-- 'indent').
--
-- > nest 2 (text "hello" <$> text "world") <$> text "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @
nest :: Int -> Doc -> Doc
nest i x        = Nest i x

column, nesting :: (Int -> Doc) -> Doc
column f        = Column f
nesting f       = Nesting f

group :: Doc -> Doc
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line break)    = if break then Empty else Spaces 1
flatten (Union x _)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten (Location l e)  = Location l (flatten e)
flatten (Optional d)    = Optional (flatten d)
flatten other           = other                     --Empty,Char,Text,Spaces

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs = Nil | Cons !Int Doc Docs


-- | format with indentation
formatIndent :: Float -> Int -> Doc -> FormattedDoc
formatIndent rfrac w x
    = best 0 0 (\_ _ -> FNil) (Cons 0 x Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best n k t Nil = t n k
      best n k t (Cons i d ds)
        = case d of
            Empty       -> best n k t ds
            Char c      -> let k' = k+1 in k' `seq` FChar c (best n k' t ds)
            Text l s    -> let k' = k+l in k' `seq` FText l s (best n k' t ds)
            Line _      -> FLine i (best i i t ds)
            Cat x y     -> best n k t (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in i' `seq` best n k t (Cons i' x ds)
            Union x y   -> nicest n k (best n k t (Cons i x ds))
                                      (best n k t (Cons i y ds))

            Column f     -> best n k t (Cons i (f k) ds)
            Nesting f    -> best n k t (Cons i (f i) ds)
            Location l s -> -- best n k t (Cons i s ds)
                            FLocationStart l $
                            best n k (\n' k' -> FLocationEnd $ best n' k' t ds) (Cons i s Nil)
            Spaces s     -> let k' = k+s in k' `seq` FSpaces s (best n k' t ds)
            Optional d   -> best n k t (Cons i d ds)

      nicest :: Int -> Int -> FormattedDoc -> FormattedDoc -> FormattedDoc
      nicest n k x y    | fits width x  = x
                        -- | fits' w r width x = x -- fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)

fits' :: Int -> Int -> Int -> FormattedDoc -> Bool
fits' lw lr w _            | w < 0 = False
fits' lw lr _ FNil                 = True
fits' lw lr w (FChar _ x)          = fits' lw lr (w - 1) x
fits' lw lr w (FText l _ x)        = fits' lw lr (w - l) x
fits' lw lr _ (FLine i x)          = True -- fits' lw (lr`div`2) (min (lw-i) lr) x
fits' lw lr w (FSpaces n x)        = fits' lw lr (w - n) x
fits' lw lr w (FLocationStart _ x) = fits' lw lr w x
fits' lw lr w (FLocationEnd x)     = fits' lw lr w x


fits :: Int -> FormattedDoc -> Bool
fits w _            | w < 0 = False
fits _ FNil                 = True
fits w (FChar _ x)          = fits (w - 1) x
fits w (FText l _ x)        = fits (w - l) x
fits _ (FLine{})            = True
fits w (FSpaces n x)        = fits (w - n) x
fits w (FLocationStart _ x) = fits w x
fits w (FLocationEnd x)     = fits w x

{-|
  the compact formatter ignores optional document parts
 -}
formatCompact :: Int -- ^ insert line breaks if line gets longer than this
              -> Doc
              -> FormattedDoc
formatCompact w x = go 0 (const FNil) x []
    where
      scan k t []     = t k
      scan k t (d:ds) = go k t d ds
      go k t d ds = case d of
        Empty           -> scan k t ds
        Char c          -> let k' = k+1
                           in  k' `seq` FChar c (scan k' t ds)
        Text l s        -> let k' = k+l
                           in  k' `seq` FText l s (scan k' t ds)
        Line _  | k > w -> FLine 0 (scan 0 t ds)
        Line False      -> go k t (Spaces 1) ds
        Line _          -> scan k t ds
        Cat x y         -> go k t x (y:ds)
        Nest _ x        -> go k t x ds
        Union _ y       -> go k t y ds
        Column f        -> go k t (f k) ds
        Nesting f       -> go k t (f 0) ds
        Optional _      -> scan k t ds
        Spaces n        -> let k' = k+n
                           in k' `seq` FSpaces n (scan k' t ds)
        Location l s    -> FLocationStart l $
          go k (\k' -> FLocationEnd (scan k' t ds)) s []

buildDoc :: FormattedDoc -> BB.Builder
buildDoc d = go d
  where
    go :: FormattedDoc -> BB.Builder
    go FNil                 = mempty
    go (FChar c d)          = BB.charUtf8 c <> go d
    go (FText _ b d)        = TLE.encodeUtf8Builder (B.toLazyText b) <> go d
    go (FLine i d)          = BB.char7 '\n' <> spaces i <> go d
    go (FSpaces i d)        = spaces i <> go d
    go (FLocationStart _ d) = go d
    go (FLocationEnd d)     = go d

buildDocWithMap :: FormattedDoc -> (BB.Builder, Pos, [(L, Span)])
buildDocWithMap d = go 1 1 [] [] mempty d
  where
    go :: Int -> Int -> [(L,Int,Int)] -> [(L, Span)]
       -> BB.Builder -> FormattedDoc -> (BB.Builder, Pos, [(L, Span)])
    go  l  c [] !ls !b FNil =
      let ls' = reverse ls in rnf ls' `seq` (b, Pos l c, ls')
    go  _  _  _  _   _  FNil =
      panic "buildDocWithMap: unterminated location marker"
    go !l !c !ss !ls !b (FChar ch d) =
      go l c ss ls (b <> BB.charUtf8 ch) d
    go !l !c !ss !ls !b (FText n t d) =
      go l (c+n) ss ls (b <> TLE.encodeUtf8Builder (B.toLazyText t)) d
    go !l !c !ss !ls !b (FLine i d) =
      go (l+1) i ss ls (b <> BB.char7 '\n' <> spaces i) d
    go !l !c !ss !ls !b (FSpaces i d) = go l (c+i) ss ls (b <> spaces i) d
    go !l !c !ss !ls !b (FLocationStart lo d) = go l c ((lo,l,c):ss) ls b d
    go !l !c ((lo,sl,sc):ss) ls b (FLocationEnd d) =
      go l c ss ((lo, Span sl sc l c):ls) b d
    go _  _  _  _  _ (FLocationEnd{}) =
      panic ("hPutDocWithMap: unexpected end of location")

-- is this efficient enough?
spaces :: Int -> BB.Builder
spaces n =
  if n <= 0 then mempty
            else BB.char7 ' ' <> spaces (n-1)

hPutDoc :: Handle -> FormattedDoc -> IO ()
hPutDoc h d = go d
  where
    go :: FormattedDoc -> IO ()
    go FNil                 = return ()
    go (FChar c d)          = hPutChar h c >> go d
    go (FText _ b d)        = TL.hPutStr h (B.toLazyText b) >> go d
    go (FLine i d)          = hPutChar h '\n' >> hPutStr h (replicate i ' ') >> go d
    go (FSpaces i d)        = hPutStr h (replicate i ' ') >> go d
    go (FLocationStart _ d) = go d
    go (FLocationEnd d)     = go d

hPutDocWithMap :: Handle -> FormattedDoc -> IO (Pos, [(L,Span)])
hPutDocWithMap h d = go 1 0 [] [] d
  where
    go :: Int -> Int -> [(L,Int,Int)] -> [(L, Span)] -> FormattedDoc -> IO (Pos, [(L, Span)])
    go !l !c  [] !ls FNil = evaluate $
      let ls' = reverse ls in rnf ls' `seq` (Pos l c, ls')
    go !l !c  _   _  FNil =
      panic "hPutDocWithMap: unterminated location marker"
    go !l !c !ss !ls (FChar ch d)  = hPutChar h ch >> go l (c+1) ss ls d
    go !l !c !ss !ls (FText n b d) = TL.hPutStr h (B.toLazyText b) >> go l (c+n) ss ls d
    go !l  _ !ss !ls (FLine i d)   = hPutChar h '\n' >>
                                     hPutStr h (replicate i ' ') >>
                                     go (l+1) i ss ls d
    go !l !c !ss !ls (FSpaces i d) = hPutStr h (replicate i ' ') >>
                                     go l (c+i) ss ls d
    go !l !c !ss !ls (FLocationStart lo d) = go l c ((lo,l,c):ss) ls d
    go !l !c ((lo,sl,sc):ss) !ls (FLocationEnd d) =
      go l c ss ((lo, Span sl sc l c):ls) d
    go _  _  _   _   (FLocationEnd{}) =
      panic ("hPutDocWithMap: unexpected end of location")

{-
-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------


-- | @(displayS simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class).
--
-- > showWidth :: Int -> Doc -> String
-- > showWidth w x   = displayS (renderPretty 0.4 w x) ""
displayS :: SimpleDoc -> ShowS
displayS SEmpty             = id
displayS (SChar c x)        = showChar c . displayS x
displayS (SText l s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':indentation i) . displayS x


-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file
-- handle @handle@. This function is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc  = displayIO handle (renderPretty 0.4 100 doc)
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO handle simpleDoc
    = display simpleDoc
    where
      display SEmpty        = return ()
      display (SChar c x)   = do{ hPutChar handle c; display x}
      display (SText l s x) = do{ hPutStr handle s; display x}
      display (SLine i x)   = do{ hPutStr handle ('\n':indentation i); display x}

-}
{-
-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show Doc where
  showsPrec d doc       = displayS (renderPretty 0.4 80 doc)

-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc

-- | @(hPutDoc handle doc)@ pretty prints document @doc@ to the file
-- handle @handle@ with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main = do{ handle <- openFile "MyFile" WriteMode
-- >          ; hPutDoc handle (vcat (map text
-- >                            ["vertical","text"]))
-- >          ; hClose handle
-- >          }
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc      = displayIO handle (renderPretty 0.4 80 doc)
-}

{-
-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '
-}
