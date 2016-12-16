{-# LANGUAGE ExtendedDefaultRules, NoMonoPatBinds #-}
module Main(main) where
import Data.Time.Clock.POSIX (getPOSIXTime)

import Data.Char (isDigit, isAlpha, isLower, isUpper)
import Text.Printf (printf)

data Expr
  = Var  Var
  | Con  Tag  [Expr]
  | Lam  Var  Expr
  | App  Expr Expr
  | Let  [Binding] Expr
  | Case Expr [Alt]
  deriving Eq

type Var = String
type Tag = String
type Binding = (Var, Expr)
type Alt = (Pat, Expr)
data Pat = Pat Tag [Var] deriving Eq

con :: Tag -> Expr
con tag = Con tag []

app :: Expr -> [Expr] -> Expr
app expr args = case args of
  [] -> expr
  arg:args' -> app (App expr arg) args'

zero, suc, nil, cons :: Expr
zero = con "Zero"
suc = con "Succ"
nil = con "Nil"
cons = con "Cons"

nat :: Int -> Expr
nat n = if n > 0 then App suc (nat (n - 1)) else zero

-- | Represents how many characters are consumed within the parsed string.
type Chars = Int

-- | The result of parsing. It can be an Error or Done.
-- | Done contains the value, line, column and the tail to be parsed.
data ParserResult a = Error String | Done a Chars String

-- | The Parser type. A parser takes a String, process the appropiate
-- | parser, and returns a ParserResult.
newtype Parser a = Parser { parse :: String -> ParserResult a }

--instance Functor Parser where
fmap' :: (a -> b) -> Parser a -> Parser b
fmap' f parser =
  case parser of
    Parser parse -> Parser (\s ->
      case parse s of
        Error msg -> Error msg
        Done a chars rest -> Done (f a) chars rest
      )

--instance Applicative Parser where
pure' :: a -> Parser a
pure' a = Parser (Done a 0)


(<*>!) :: Parser (a -> b) -> Parser a -> Parser b
(<*>!) op v = case op of
  Parser pf -> case v of
    Parser p -> Parser (\s ->
      case pf s of
        Error msg -> Error msg
        Done f chars rest -> case p rest of
          Error msg' -> Error msg'
          Done a chars' rest' -> Done (f a) (chars+chars') rest'
      )

--instance Monad Parser where
return' :: a -> Parser a
return' = pure'

(>>=!) :: Parser a -> (a -> Parser b) -> Parser b
(>>=!) pr f = case pr of
  Parser p -> Parser (\s ->
    case p s of
      Error msg -> Error msg
      Done a chars rest -> case parse (f a) rest of
        Error msg' -> Error msg'
        Done b chars' rest' -> Done b (chars+chars') rest'
    )

(>>!) :: Parser a -> Parser b -> Parser b
(>>!) p q = p >>=! (\_x -> q)

--instance Alternative Parser where
empty :: Parser a
empty = Parser (const (Error "Empty parser"))

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) pr qr = case pr of
  Parser p -> case qr of
    Parser q -> Parser (\s ->
      case p s of
        Error msg'p -> case q s of
          Error msg'q -> Error (msg'p ++ " or " ++ msg'q)
          done -> done
        done -> done
        )

some :: Parser a -> Parser [a]
some v = some_v
  where
    many_v = some_v <|> pure' []
    some_v = (fmap' (:) v) <*>! many_v

many :: Parser a -> Parser [a]
many v = many_v
  where
    many_v = some_v <|> pure' []
    some_v = (fmap' (:) v) <*>! many_v


-- | Parses a char.
item :: Parser Char
item = Parser (\s -> case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = (>>=!) item (\c ->
  if pred c
    then return' c
    else Parser (const (Error (printf "expecting %s got %c" msg c))))

oneOf :: String -> Parser Char
oneOf s = satisfy s (`elem` s)

char :: Char -> Parser Char
char c = satisfy [c] (c ==)

natural :: Parser Integer
natural = fmap' read (some (satisfy "isDigit" isDigit))

string :: String -> Parser String
--string [] = return []
--string (c:cs) = do { char c; string cs; return (c:cs)}
string s = case s of
  [] -> return' []
  (c:cs) -> char c >>! string cs >>! return' (c:cs)

token :: Parser a -> Parser a
--token p = do { a <- p; spaces ; return a}
token p = p >>=! (\a -> spaces >>! return' a)

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy "isdigit" isDigit

alpha :: Parser Char
alpha = satisfy "isalpha" isAlpha

loweralpha :: Parser Char
loweralpha = satisfy "loweralpha" (\c -> isAlpha c && isLower c)

upperalpha :: Parser Char
upperalpha = satisfy "upperalpha" (\c -> isAlpha c && isUpper c)

dollar :: Parser Char
dollar = satisfy "dollar sign" (== '$')

underscore :: Parser Char
underscore = satisfy "underscore" (== '_')

quote :: Parser Char
quote = satisfy "quote" (== '\'')

number :: Parser Int
--number = do
--  s <- string "-" <|> return []
--  cs <- some digit
--  spaces
--  return (read (s ++ cs))
number =
  (string "-" <|> return' []) >>=! ( \s ->
  some digit >>=! ( \cs ->
  spaces >>!
  return' (read (s ++ cs))
  ))

lowerword :: Parser String
lowerword =
  loweralpha >>=! (\c->
  many alpha >>=! (\cs->
  spaces >>!
  return' (c:cs)
  ))

upperword :: Parser String
upperword =
  upperalpha >>=! (\c->
  many alpha >>=! (\cs->
  spaces >>!
  return' (c:cs)
  ))

sat :: String -> Parser String -> (String -> Bool) -> Parser String
sat msg p pred = (>>=!) p (\s -> if pred s
    then return' s
    else Parser (const (Error (printf "expecting %s got %s" msg s))))

paired:: String -> String -> Parser a -> Parser a
paired o c m = reserved o >>! m >>=! (\n-> reserved c >>! return' n )

parens :: Parser a -> Parser a
parens = paired "(" ")"

braces :: Parser a -> Parser a
braces = paired "{" "}"

brackets :: Parser a -> Parser a
brackets = paired "[" "]"

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>=! \a -> rest a
  where rest a = (op >>=! (\f -> p >>=! \b-> rest (f a b))) <|> return' a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>=! \a -> rest a
  where rest a = (op >>=! \f->  p >>=! \b->  rest b>>=! \b'-> return' (f a b'))
          <|> return' a

exprp :: Parser Expr
exprp = (termp `chainr1` conslistp) `chainl1` return' App

conslistp :: Parser (Expr -> Expr -> Expr)
conslistp = reserved ":" >>! return' (App . App cons)

termp :: Parser Expr
termp = litintp
    <|> letp
    <|> casep
    <|> varp
    <|> conp
    <|> braces lamp
    <|> parens exprp
    <|> brackets listp

litintp :: Parser Expr
litintp = number >>=! \n-> return' (nat n)

letp :: Parser Expr
letp =
  reserved "let" >>!
  bindsp >>=! \binds ->
  --var <- varnamep
  --reserved "="
  --valexpr <- exprp
  reserved "in" >>!
  exprp >>=! \inexpr ->
  return' (Let binds inexpr)

bindsp :: Parser [Binding]
bindsp =
        varnamep >>=! \var->
        reserved "=" >>!
        exprp >>=! \valexpr ->
        (
          reserved ";" >>!
          bindsp >>=! \binds ->
          return' ((var, valexpr):binds) ) <|>
          return' [(var, valexpr)]

varp :: Parser Expr
varp =
  varnamep >>=! \var->
  return' (Var var)

conp :: Parser Expr
conp = upperword >>=! \tag-> return' (con tag)

lamp :: Parser Expr
lamp =
  varnamep >>=! \var->
  reserved "->" >>!
  exprp >>=! \valexpr ->
  return' (Lam var valexpr)

casep :: Parser Expr
casep =
  reserved "case" >>!
  exprp >>=! \scexpr ->
  reserved "of" >>!
  some altp >>=! \alts->
  return' (Case scexpr alts)

altp :: Parser (Pat, Expr)
altp =
  upperword >>=! \tag->
  many varnamep >>=! \vars->
  reserved "->" >>!
  exprp >>=! \res->
  reserved ";" >>!
  return' (Pat tag vars, res)

listp :: Parser Expr
listp = (
      exprp >>=! \item ->
      (
        reserved "," >>!
        listp >>=! \rest->
        return' (app cons [item, rest])) <|>
        return' (app cons [item, nil])
    ) <|> return' nil

varnamep :: Parser Var
varnamep = sat (show keywords) varid (not . (`elem` keywords))
  where keywords = ["let", "in", "case", "of"]

varid :: Parser Var
varid =
  loweralpha <|> dollar <|> underscore >>=! \c->
  many (alpha <|> digit <|> underscore <|> quote) >>=! \cs->
  spaces >>!
  return' (c:cs)

parseWith :: Parser Expr -> String -> Expr
parseWith p s =
  case parse (spaces >>! p) s of
    Done a chars rest -> case rest of
      [] -> a
      _ -> error $ "Parser didn't consume entire stream: <<" ++ rest ++ ">> " ++
        " in <<" ++ s ++ ">> at " ++ show chars ++
                   " with (expr ommited by --show--)"
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s

{-|
  Given a textual representation of Expr, returns the parsed Expr.
-}
parseExpr :: String -> Expr
parseExpr = parseWith exprp

root = parseExpr

tests = []



-- | A class of types that can be fully evaluated.
class NFData a where
    rnf :: a -> ()
    rnf a = a `seq` ()

instance NFData Int 
instance NFData Integer
instance NFData Float
instance NFData Double

instance NFData Char
instance NFData Bool
instance NFData ()

instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x

instance (NFData a, NFData b) => NFData (Either a b) where
    rnf (Left x)  = rnf x
    rnf (Right y) = rnf y

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = rnf x `seq` rnf xs

instance (NFData a, NFData b) => NFData (a,b) where
  rnf (x,y) = rnf x `seq` rnf y

time_ :: IO a -> IO Double
time_ act = do { start <- getTime; act; end <- getTime; return $! (Prelude.-) end start }

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

main = do { t <- time_ (rnf results `seq` return ()); print t }
  where results = map assertEq tests

assertEq :: (Show a, Eq a) => (a, a) -> ()
assertEq (x, y) = if x == y then () else error ("FAIL! " ++ show x ++ ", " ++ show y)

root = let
         wTup2_a729 = \x1 -> \x2 -> (,) x1 x2
         wCons_a454 = \x1 -> \x2 -> (:) x1 x2
         wApp_a313 = \x1 -> \x2 -> App x1 x2
         wCase_a730 = \x1 -> \x2 -> Case x1 x2
         wCon_a731 = \x1 -> \x2 -> Con x1 x2
         wDone_a732 = \x1 -> \x2 -> \x3 -> Done x1 x2 x3
         wError_a733 = \x1 -> Error x1
         wLam_a734 = \x1 -> \x2 -> Lam x1 x2
         wLet_a735 = \x1 -> \x2 -> Let x1 x2
         wParser_a736 = \x1 -> Parser x1
         wPat_a737 = \x1 -> \x2 -> Pat x1 x2
         wVar_a738 = \x1 -> Var x1
         wNil_a455 = []
         w0_a739 = 0 :: Int
         w1_a740 = 1 :: Int
         wz10_a741 = '\n'
         wz13_a742 = '\r'
         wz32_a458 = ' '
         wz36_a743 = '$'
         wz37_a676 = '%'
         w'_a521 = '\''
         wz40_a635 = '('
         wz41_a576 = ')'
         wz44_a744 = ','
         wz45_a577 = '-'
         wz58_a461 = ':'
         wz59_a745 = ';'
         wz60_a453 = '<'
         wz61_a746 = '='
         wz62_a548 = '>'
         wC_a747 = 'C'
         wE_a749 = 'E'
         wF_a750 = 'F'
         wN_a751 = 'N'
         wO_a752 = 'O'
         wP_a545 = 'P'
         wR_a753 = 'R'
         wS_a754 = 'S'
         wZ_a755 = 'Z'
         wz91_a756 = '['
         wz93_a757 = ']'
         w__a758 = '_'
         wz96_a679 = '`'
         wa_a467 = 'a'
         wb_a604 = 'b'
         wc_a514 = 'c'
         wd_a526 = 'd'
         we_a470 = 'e'
         wf_a759 = 'f'
         wg_a760 = 'g'
         wh_a590 = 'h'
         wi_a488 = 'i'
         wl_a761 = 'l'
         wm_a464 = 'm'
         wn_a493 = 'n'
         wo_a511 = 'o'
         wp_a627 = 'p'
         wq_a762 = 'q'
         wr_a473 = 'r'
         ws_a479 = 's'
         wt_a476 = 't'
         wu_a504 = 'u'
         ww_a585 = 'w'
         wx_a630 = 'x'
         wy_a601 = 'y'
         wz123_a763 = '{'
         wz125_a764 = '}'
         wAdd_a765 = \x1 -> \x2 -> (+) x1 x2
         wSubtract_a766 = \x1 -> \x2 -> (-) x1 x2
         wEqual_a767 = \x1 -> \x2 -> (==) x1 x2
         error = \msg -> seq (prelude_error msg) undefined
         con = \tag -> wCon_a731 tag wNil_a455
         app = \expr -> \args -> case args of
                                   [] -> expr
                                   (:) arg args' ->
                                     let aapp1_a881 = wApp_a313 expr arg
                                     in app aapp1_a881 args'
         zzero = let
                   awCons2_a990 = wCons_a454 wo_a511 wNil_a455
                   awCons2_a991 = wCons_a454 wr_a473 awCons2_a990
                   awCons2_a992 = wCons_a454 we_a470 awCons2_a991
                   acon1_a993 = wCons_a454 wZ_a755 awCons2_a992
                 in con acon1_a993
         suc = let
                 awCons2_a994 = wCons_a454 wc_a514 wNil_a455
                 awCons2_a995 = wCons_a454 wc_a514 awCons2_a994
                 awCons2_a996 = wCons_a454 wu_a504 awCons2_a995
                 acon1_a997 = wCons_a454 wS_a754 awCons2_a996
               in con acon1_a997
         nil = let
                 awCons2_a882 = wCons_a454 wl_a761 wNil_a455
                 awCons2_a883 = wCons_a454 wi_a488 awCons2_a882
                 acon1_a884 = wCons_a454 wN_a751 awCons2_a883
               in con acon1_a884
         cons = let
                  awCons2_a801 = wCons_a454 ws_a479 wNil_a455
                  awCons2_a802 = wCons_a454 wn_a493 awCons2_a801
                  awCons2_a803 = wCons_a454 wo_a511 awCons2_a802
                  acon1_a804 = wCons_a454 wC_a747 awCons2_a803
                in con acon1_a804
         nat = \n -> let
                       anat1_a886 = wSubtract_a766 n w1_a740
                       awApp2_a885 = nat anat1_a886
                     in case z62 n w0_a739 of
                          True -> wApp_a313 suc awApp2_a885
                          False -> zzero
         fmap' = \f -> \parser -> case parser of
                                    Parser parse ->
                                      let
                                        awParser1_a887 = \s -> case parse s of
                                                                 Error msg -> wError_a733 msg
                                                                 Done a chars rest ->
                                                                   let awDone1_a888 = f a
                                                                   in wDone_a732 awDone1_a888 chars rest
                                      in wParser_a736 awParser1_a887
         pure' = \a -> let awParser1_a805 = wDone_a732 a w0_a739
                       in wParser_a736 awParser1_a805
         z60z42z62z33 = \op -> \v -> case op of
                                       Parser pf ->
                                         case v of
                                           Parser p ->
                                             let
                                               awParser1_a889 = \s -> case pf s of
                                                                        Error msg -> wError_a733 msg
                                                                        Done f chars rest ->
                                                                          case p rest of
                                                                            Error msg' -> wError_a733 msg'
                                                                            Done a chars' rest' ->
                                                                              let
                                                                                awDone1_a890 = f a
                                                                                awDone2_a891 = wAdd_a765 chars chars'
                                                                              in wDone_a732 awDone1_a890 awDone2_a891 rest'
                                             in wParser_a736 awParser1_a889
         return' = pure'
         z62z62z61z33 = \pr -> \f -> case pr of
                                       Parser p ->
                                         let
                                           awParser1_a806 = \s -> case p s of
                                                                    Error msg -> wError_a733 msg
                                                                    Done a chars rest ->
                                                                      let aparse1_a808 = f a
                                                                      in case parse aparse1_a808 rest of
                                                                           Error msg' -> wError_a733 msg'
                                                                           Done b chars' rest' ->
                                                                             let
                                                                               awDone2_a807 = wAdd_a765 chars chars'
                                                                             in wDone_a732 b awDone2_a807 rest'
                                         in wParser_a736 awParser1_a806
         z62z62z33 = \p -> \q -> let az62z62z61z332_a768 = \_x -> q
                                 in z62z62z61z33 p az62z62z61z332_a768
         z60z124z62 = \pr -> \qr -> case pr of
                                      Parser p ->
                                        case qr of
                                          Parser q ->
                                            let
                                              awParser1_a809 = \s -> case p s of
                                                                       Error msg'p ->
                                                                         case q s of
                                                                           Error msg'q ->
                                                                             let
                                                                               awCons2_a811 = wCons_a454 wz32_a458 wNil_a455
                                                                               awCons2_a812 = wCons_a454 wr_a473 awCons2_a811
                                                                               awCons2_a813 = wCons_a454 wo_a511 awCons2_a812
                                                                               az43z431_a814 = wCons_a454 wz32_a458 awCons2_a813
                                                                               az43z432_a815 = z43z43 az43z431_a814 msg'q
                                                                               awError1_a810 = z43z43 msg'p az43z432_a815
                                                                             in wError_a733 awError1_a810
                                                                           done -> done
                                                                       done -> done
                                            in wParser_a736 awParser1_a809
         some = \v -> let
                        many_v = let az60z124z622_a892 = pure' wNil_a455
                                 in z60z124z62 some_v az60z124z622_a892
                        some_v = let az60z42z62z331_a893 = fmap' wCons_a454 v
                                 in z60z42z62z33 az60z42z62z331_a893 many_v
                      in some_v
         many = \v -> let
                        many_v = let az60z124z622_a816 = pure' wNil_a455
                                 in z60z124z62 some_v az60z124z622_a816
                        some_v = let az60z42z62z331_a817 = fmap' wCons_a454 v
                                 in z60z42z62z33 az60z42z62z331_a817 many_v
                      in many_v
         item = let
                  awParser1_a998 = \s -> case s of
                                           [] ->
                                             let
                                               awCons2_a1000 = wCons_a454 wF_a750 wNil_a455
                                               awCons2_a1001 = wCons_a454 wO_a752 awCons2_a1000
                                               awCons2_a1002 = wCons_a454 wE_a749 awCons2_a1001
                                               awCons2_a1003 = wCons_a454 wz32_a458 awCons2_a1002
                                               awCons2_a1004 = wCons_a454 wd_a526 awCons2_a1003
                                               awCons2_a1005 = wCons_a454 we_a470 awCons2_a1004
                                               awCons2_a1006 = wCons_a454 wh_a590 awCons2_a1005
                                               awCons2_a1007 = wCons_a454 wc_a514 awCons2_a1006
                                               awCons2_a1008 = wCons_a454 wa_a467 awCons2_a1007
                                               awCons2_a1009 = wCons_a454 we_a470 awCons2_a1008
                                               awError1_a999 = wCons_a454 wR_a753 awCons2_a1009
                                             in wError_a733 awError1_a999
                                           (:) c cs -> wDone_a732 c w1_a740 cs
                in wParser_a736 awParser1_a998
         satisfy = \msg -> \pred -> let
                                      az62z62z61z332_a916 = \c -> let
                                                                    awCons2_a895 = wCons_a454 wc_a514 wNil_a455
                                                                    awCons2_a896 = wCons_a454 wz37_a676 awCons2_a895
                                                                    awCons2_a897 = wCons_a454 wz32_a458 awCons2_a896
                                                                    awCons2_a898 = wCons_a454 wt_a476 awCons2_a897
                                                                    awCons2_a899 = wCons_a454 wo_a511 awCons2_a898
                                                                    awCons2_a900 = wCons_a454 wg_a760 awCons2_a899
                                                                    awCons2_a901 = wCons_a454 wz32_a458 awCons2_a900
                                                                    awCons2_a902 = wCons_a454 ws_a479 awCons2_a901
                                                                    awCons2_a903 = wCons_a454 wz37_a676 awCons2_a902
                                                                    awCons2_a904 = wCons_a454 wz32_a458 awCons2_a903
                                                                    awCons2_a905 = wCons_a454 wg_a760 awCons2_a904
                                                                    awCons2_a906 = wCons_a454 wn_a493 awCons2_a905
                                                                    awCons2_a907 = wCons_a454 wi_a488 awCons2_a906
                                                                    awCons2_a908 = wCons_a454 wt_a476 awCons2_a907
                                                                    awCons2_a909 = wCons_a454 wc_a514 awCons2_a908
                                                                    awCons2_a910 = wCons_a454 we_a470 awCons2_a909
                                                                    awCons2_a911 = wCons_a454 wp_a627 awCons2_a910
                                                                    awCons2_a912 = wCons_a454 wx_a630 awCons2_a911
                                                                    aprintf1_a913 = wCons_a454 we_a470 awCons2_a912
                                                                    awError1_a914 = printf aprintf1_a913 msg c
                                                                    aconst1_a915 = wError_a733 awError1_a914
                                                                    awParser1_a894 = const aconst1_a915
                                                                  in case pred c of
                                                                       True -> return' c
                                                                       False -> wParser_a736 awParser1_a894
                                    in z62z62z61z33 item az62z62z61z332_a916
         oneOf = \s -> let asatisfy2_a819 = \rsect_a818 -> elem rsect_a818 s
                       in satisfy s asatisfy2_a819
         char = \c -> let
                        asatisfy1_a1011 = wCons_a454 c wNil_a455
                        asatisfy2_a1010 = wEqual_a767 c
                      in satisfy asatisfy1_a1011 asatisfy2_a1010
         string = \s -> case s of
                          [] -> return' wNil_a455
                          (:) c cs ->
                            let
                              az62z62z331_a920 = char c
                              az62z62z332_a919 = string cs
                              az62z62z331_a918 = z62z62z33 az62z62z331_a920 az62z62z332_a919
                              areturn'1_a921 = wCons_a454 c cs
                              az62z62z332_a917 = return' areturn'1_a921
                            in z62z62z33 az62z62z331_a918 az62z62z332_a917
         token = \p -> let
                         az62z62z61z332_a923 = \a -> let az62z62z332_a922 = return' a
                                                     in z62z62z33 spaces az62z62z332_a922
                       in z62z62z61z33 p az62z62z61z332_a923
         reserved = \s -> let atoken1_a820 = string s
                          in token atoken1_a820
         spaces = let
                    awCons2_a769 = wCons_a454 wz13_a742 wNil_a455
                    awCons2_a770 = wCons_a454 wz10_a741 awCons2_a769
                    aoneOf1_a771 = wCons_a454 wz32_a458 awCons2_a770
                    az362_a772 = oneOf aoneOf1_a771
                  in z36 many az362_a772
         digit = let
                   awCons2_a1012 = wCons_a454 wt_a476 wNil_a455
                   awCons2_a1013 = wCons_a454 wi_a488 awCons2_a1012
                   awCons2_a1014 = wCons_a454 wg_a760 awCons2_a1013
                   awCons2_a1015 = wCons_a454 wi_a488 awCons2_a1014
                   awCons2_a1016 = wCons_a454 wd_a526 awCons2_a1015
                   awCons2_a1017 = wCons_a454 ws_a479 awCons2_a1016
                   asatisfy1_a1018 = wCons_a454 wi_a488 awCons2_a1017
                 in satisfy asatisfy1_a1018 isDigit
         alpha = let
                   awCons2_a1019 = wCons_a454 wa_a467 wNil_a455
                   awCons2_a1020 = wCons_a454 wh_a590 awCons2_a1019
                   awCons2_a1021 = wCons_a454 wp_a627 awCons2_a1020
                   awCons2_a1022 = wCons_a454 wl_a761 awCons2_a1021
                   awCons2_a1023 = wCons_a454 wa_a467 awCons2_a1022
                   awCons2_a1024 = wCons_a454 ws_a479 awCons2_a1023
                   asatisfy1_a1025 = wCons_a454 wi_a488 awCons2_a1024
                 in satisfy asatisfy1_a1025 isAlpha
         loweralpha = let
                        awCons2_a1074 = wCons_a454 wa_a467 wNil_a455
                        awCons2_a1075 = wCons_a454 wh_a590 awCons2_a1074
                        awCons2_a1076 = wCons_a454 wp_a627 awCons2_a1075
                        awCons2_a1077 = wCons_a454 wl_a761 awCons2_a1076
                        awCons2_a1078 = wCons_a454 wa_a467 awCons2_a1077
                        awCons2_a1079 = wCons_a454 wr_a473 awCons2_a1078
                        awCons2_a1080 = wCons_a454 we_a470 awCons2_a1079
                        awCons2_a1081 = wCons_a454 ww_a585 awCons2_a1080
                        awCons2_a1082 = wCons_a454 wo_a511 awCons2_a1081
                        asatisfy1_a1073 = wCons_a454 wl_a761 awCons2_a1082
                        asatisfy2_a1072 = \c -> let
                                                  az38z381_a1084 = isAlpha c
                                                  az38z382_a1083 = isLower c
                                                in z38z38 az38z381_a1084 az38z382_a1083
                      in satisfy asatisfy1_a1073 asatisfy2_a1072
         upperalpha = let
                        awCons2_a1028 = wCons_a454 wa_a467 wNil_a455
                        awCons2_a1029 = wCons_a454 wh_a590 awCons2_a1028
                        awCons2_a1030 = wCons_a454 wp_a627 awCons2_a1029
                        awCons2_a1031 = wCons_a454 wl_a761 awCons2_a1030
                        awCons2_a1032 = wCons_a454 wa_a467 awCons2_a1031
                        awCons2_a1033 = wCons_a454 wr_a473 awCons2_a1032
                        awCons2_a1034 = wCons_a454 we_a470 awCons2_a1033
                        awCons2_a1035 = wCons_a454 wp_a627 awCons2_a1034
                        awCons2_a1036 = wCons_a454 wp_a627 awCons2_a1035
                        asatisfy1_a1027 = wCons_a454 wu_a504 awCons2_a1036
                        asatisfy2_a1026 = \c -> let
                                                  az38z381_a1038 = isAlpha c
                                                  az38z382_a1037 = isUpper c
                                                in z38z38 az38z381_a1038 az38z382_a1037
                      in satisfy asatisfy1_a1027 asatisfy2_a1026
         dollar = let
                    awCons2_a1087 = wCons_a454 wn_a493 wNil_a455
                    awCons2_a1088 = wCons_a454 wg_a760 awCons2_a1087
                    awCons2_a1089 = wCons_a454 wi_a488 awCons2_a1088
                    awCons2_a1090 = wCons_a454 ws_a479 awCons2_a1089
                    awCons2_a1091 = wCons_a454 wz32_a458 awCons2_a1090
                    awCons2_a1092 = wCons_a454 wr_a473 awCons2_a1091
                    awCons2_a1093 = wCons_a454 wa_a467 awCons2_a1092
                    awCons2_a1094 = wCons_a454 wl_a761 awCons2_a1093
                    awCons2_a1095 = wCons_a454 wl_a761 awCons2_a1094
                    awCons2_a1096 = wCons_a454 wo_a511 awCons2_a1095
                    asatisfy1_a1086 = wCons_a454 wd_a526 awCons2_a1096
                    asatisfy2_a1085 = \rsect_a1097 -> wEqual_a767 rsect_a1097 wz36_a743
                  in satisfy asatisfy1_a1086 asatisfy2_a1085
         underscore = let
                        awCons2_a1100 = wCons_a454 we_a470 wNil_a455
                        awCons2_a1101 = wCons_a454 wr_a473 awCons2_a1100
                        awCons2_a1102 = wCons_a454 wo_a511 awCons2_a1101
                        awCons2_a1103 = wCons_a454 wc_a514 awCons2_a1102
                        awCons2_a1104 = wCons_a454 ws_a479 awCons2_a1103
                        awCons2_a1105 = wCons_a454 wr_a473 awCons2_a1104
                        awCons2_a1106 = wCons_a454 we_a470 awCons2_a1105
                        awCons2_a1107 = wCons_a454 wd_a526 awCons2_a1106
                        awCons2_a1108 = wCons_a454 wn_a493 awCons2_a1107
                        asatisfy1_a1099 = wCons_a454 wu_a504 awCons2_a1108
                        asatisfy2_a1098 = \rsect_a1109 -> wEqual_a767 rsect_a1109 w__a758
                      in satisfy asatisfy1_a1099 asatisfy2_a1098
         quote = let
                   awCons2_a1112 = wCons_a454 we_a470 wNil_a455
                   awCons2_a1113 = wCons_a454 wt_a476 awCons2_a1112
                   awCons2_a1114 = wCons_a454 wo_a511 awCons2_a1113
                   awCons2_a1115 = wCons_a454 wu_a504 awCons2_a1114
                   asatisfy1_a1111 = wCons_a454 wq_a762 awCons2_a1115
                   asatisfy2_a1110 = \rsect_a1116 -> wEqual_a767 rsect_a1116 w'_a521
                 in satisfy asatisfy1_a1111 asatisfy2_a1110
         number = let
                    astring1_a926 = wCons_a454 wz45_a577 wNil_a455
                    az60z124z621_a928 = string astring1_a926
                    az60z124z622_a927 = return' wNil_a455
                    az62z62z61z331_a925 = z60z124z62 az60z124z621_a928 az60z124z622_a927
                    az62z62z61z332_a924 = \s -> let
                                                  az62z62z61z331_a930 = some digit
                                                  az62z62z61z332_a929 = \cs -> let
                                                                                 aread1_a932 = z43z43 s cs
                                                                                 areturn'1_a931 = read aread1_a932
                                                                                 az62z62z332_a933 = return' areturn'1_a931
                                                                               in z62z62z33 spaces az62z62z332_a933
                                                in z62z62z61z33 az62z62z61z331_a930 az62z62z61z332_a929
                  in z62z62z61z33 az62z62z61z331_a925 az62z62z61z332_a924
         upperword = let
                       az62z62z61z332_a938 = \c -> let
                                                     az62z62z61z331_a935 = many alpha
                                                     az62z62z61z332_a934 = \cs -> let
                                                                                    areturn'1_a936 = wCons_a454 c cs
                                                                                    az62z62z332_a937 = return' areturn'1_a936
                                                                                  in z62z62z33 spaces az62z62z332_a937
                                                   in z62z62z61z33 az62z62z61z331_a935 az62z62z61z332_a934
                     in z62z62z61z33 upperalpha az62z62z61z332_a938
         sat = \msg -> \p -> \pred -> let
                                        az62z62z61z332_a1061 = \s -> let
                                                                       awCons2_a1040 = wCons_a454 ws_a479 wNil_a455
                                                                       awCons2_a1041 = wCons_a454 wz37_a676 awCons2_a1040
                                                                       awCons2_a1042 = wCons_a454 wz32_a458 awCons2_a1041
                                                                       awCons2_a1043 = wCons_a454 wt_a476 awCons2_a1042
                                                                       awCons2_a1044 = wCons_a454 wo_a511 awCons2_a1043
                                                                       awCons2_a1045 = wCons_a454 wg_a760 awCons2_a1044
                                                                       awCons2_a1046 = wCons_a454 wz32_a458 awCons2_a1045
                                                                       awCons2_a1047 = wCons_a454 ws_a479 awCons2_a1046
                                                                       awCons2_a1048 = wCons_a454 wz37_a676 awCons2_a1047
                                                                       awCons2_a1049 = wCons_a454 wz32_a458 awCons2_a1048
                                                                       awCons2_a1050 = wCons_a454 wg_a760 awCons2_a1049
                                                                       awCons2_a1051 = wCons_a454 wn_a493 awCons2_a1050
                                                                       awCons2_a1052 = wCons_a454 wi_a488 awCons2_a1051
                                                                       awCons2_a1053 = wCons_a454 wt_a476 awCons2_a1052
                                                                       awCons2_a1054 = wCons_a454 wc_a514 awCons2_a1053
                                                                       awCons2_a1055 = wCons_a454 we_a470 awCons2_a1054
                                                                       awCons2_a1056 = wCons_a454 wp_a627 awCons2_a1055
                                                                       awCons2_a1057 = wCons_a454 wx_a630 awCons2_a1056
                                                                       aprintf1_a1058 = wCons_a454 we_a470 awCons2_a1057
                                                                       awError1_a1059 = printf aprintf1_a1058 msg s
                                                                       aconst1_a1060 = wError_a733 awError1_a1059
                                                                       awParser1_a1039 = const aconst1_a1060
                                                                     in case pred s of
                                                                          True -> return' s
                                                                          False ->
                                                                            wParser_a736 awParser1_a1039
                                      in z62z62z61z33 p az62z62z61z332_a1061
         paired = \o -> \c -> \m -> let
                                      az62z62z331_a943 = reserved o
                                      az62z62z61z331_a940 = z62z62z33 az62z62z331_a943 m
                                      az62z62z61z332_a939 = \n -> let
                                                                    az62z62z331_a942 = reserved c
                                                                    az62z62z332_a941 = return' n
                                                                  in z62z62z33 az62z62z331_a942 az62z62z332_a941
                                    in z62z62z61z33 az62z62z61z331_a940 az62z62z61z332_a939
         parens = let
                    apaired1_a822 = wCons_a454 wz40_a635 wNil_a455
                    apaired2_a821 = wCons_a454 wz41_a576 wNil_a455
                  in paired apaired1_a822 apaired2_a821
         braces = let
                    apaired1_a824 = wCons_a454 wz123_a763 wNil_a455
                    apaired2_a823 = wCons_a454 wz125_a764 wNil_a455
                  in paired apaired1_a824 apaired2_a823
         brackets = let
                      apaired1_a826 = wCons_a454 wz91_a756 wNil_a455
                      apaired2_a825 = wCons_a454 wz93_a757 wNil_a455
                    in paired apaired1_a826 apaired2_a825
         chainl1 = \p -> \op -> let
                                  rest = \a -> let
                                                 az62z62z61z332_a777 = \f -> let
                                                                               az62z62z61z332_a776 = \b -> let
                                                                                                             arest1_a775 = f a b
                                                                                                           in rest arest1_a775
                                                                             in z62z62z61z33 p az62z62z61z332_a776
                                                 az60z124z621_a774 = z62z62z61z33 op az62z62z61z332_a777
                                                 az60z124z622_a773 = return' a
                                               in z60z124z62 az60z124z621_a774 az60z124z622_a773
                                  az62z62z61z332_a778 = \a -> rest a
                                in z62z62z61z33 p az62z62z61z332_a778
         chainr1 = \p -> \op -> let
                                  rest = \a -> let
                                                 az62z62z61z332_a785 = \f -> let
                                                                               az62z62z61z332_a784 = \b -> let
                                                                                                             az62z62z61z331_a782 = rest b
                                                                                                             az62z62z61z332_a781 = \b' -> let
                                                                                                                                            areturn'1_a783 = f a b'
                                                                                                                                          in return' areturn'1_a783
                                                                                                           in z62z62z61z33 az62z62z61z331_a782 az62z62z61z332_a781
                                                                             in z62z62z61z33 p az62z62z61z332_a784
                                                 az60z124z621_a780 = z62z62z61z33 op az62z62z61z332_a785
                                                 az60z124z622_a779 = return' a
                                               in z60z124z62 az60z124z621_a780 az60z124z622_a779
                                  az62z62z61z332_a786 = \a -> rest a
                                in z62z62z61z33 p az62z62z61z332_a786
         exprp = let
                   achainl11_a312 = chainr1 termp conslistp
                   achainl12_a311 = return' wApp_a313
                 in chainl1 achainl11_a312 achainl12_a311
         conslistp = let
                       areserved1_a789 = wCons_a454 wz58_a461 wNil_a455
                       az62z62z331_a788 = reserved areserved1_a789
                       az462_a790 = wApp_a313 cons
                       areturn'1_a791 = z46 wApp_a313 az462_a790
                       az62z62z332_a787 = return' areturn'1_a791
                     in z62z62z33 az62z62z331_a788 az62z62z332_a787
         termp = let
                   az60z124z621_a795 = z60z124z62 litintp letp
                   az60z124z621_a794 = z60z124z62 az60z124z621_a795 casep
                   az60z124z621_a796 = z60z124z62 az60z124z621_a794 varp
                   az60z124z621_a798 = z60z124z62 az60z124z621_a796 conp
                   az60z124z622_a797 = braces lamp
                   az60z124z621_a800 = z60z124z62 az60z124z621_a798 az60z124z622_a797
                   az60z124z622_a799 = parens exprp
                   az60z124z621_a793 = z60z124z62 az60z124z621_a800 az60z124z622_a799
                   az60z124z622_a792 = brackets listp
                 in z60z124z62 az60z124z621_a793 az60z124z622_a792
         litintp = let
                     az62z62z61z332_a828 = \n -> let areturn'1_a827 = nat n
                                                 in return' areturn'1_a827
                   in z62z62z61z33 number az62z62z61z332_a828
         letp = let
                  awCons2_a831 = wCons_a454 wt_a476 wNil_a455
                  awCons2_a832 = wCons_a454 we_a470 awCons2_a831
                  areserved1_a833 = wCons_a454 wl_a761 awCons2_a832
                  az62z62z331_a834 = reserved areserved1_a833
                  az62z62z61z331_a830 = z62z62z33 az62z62z331_a834 bindsp
                  az62z62z61z332_a829 = \binds -> let
                                                    awCons2_a837 = wCons_a454 wn_a493 wNil_a455
                                                    areserved1_a838 = wCons_a454 wi_a488 awCons2_a837
                                                    az62z62z331_a839 = reserved areserved1_a838
                                                    az62z62z61z331_a836 = z62z62z33 az62z62z331_a839 exprp
                                                    az62z62z61z332_a835 = \inexpr -> let
                                                                                       areturn'1_a840 = wLet_a735 binds inexpr
                                                                                     in return' areturn'1_a840
                                                  in z62z62z61z33 az62z62z61z331_a836 az62z62z61z332_a835
                in z62z62z61z33 az62z62z61z331_a830 az62z62z61z332_a829
         bindsp = let
                    az62z62z61z332_a958 = \var -> let
                                                    areserved1_a946 = wCons_a454 wz61_a746 wNil_a455
                                                    az62z62z331_a947 = reserved areserved1_a946
                                                    az62z62z61z331_a945 = z62z62z33 az62z62z331_a947 exprp
                                                    az62z62z61z332_a944 = \valexpr -> let
                                                                                        areserved1_a950 = wCons_a454 wz59_a745 wNil_a455
                                                                                        az62z62z331_a951 = reserved areserved1_a950
                                                                                        az62z62z61z331_a955 = z62z62z33 az62z62z331_a951 bindsp
                                                                                        az62z62z61z332_a954 = \binds -> let
                                                                                                                          awCons1_a952 = wTup2_a729 var valexpr
                                                                                                                          areturn'1_a953 = wCons_a454 awCons1_a952 binds
                                                                                                                        in return' areturn'1_a953
                                                                                        az60z124z621_a949 = z62z62z61z33 az62z62z61z331_a955 az62z62z61z332_a954
                                                                                        awCons1_a956 = wTup2_a729 var valexpr
                                                                                        areturn'1_a957 = wCons_a454 awCons1_a956 wNil_a455
                                                                                        az60z124z622_a948 = return' areturn'1_a957
                                                                                      in z60z124z62 az60z124z621_a949 az60z124z622_a948
                                                  in z62z62z61z33 az62z62z61z331_a945 az62z62z61z332_a944
                  in z62z62z61z33 varnamep az62z62z61z332_a958
         varp = let
                  az62z62z61z332_a842 = \var -> let areturn'1_a841 = wVar_a738 var
                                                in return' areturn'1_a841
                in z62z62z61z33 varnamep az62z62z61z332_a842
         conp = let
                  az62z62z61z332_a844 = \tag -> let areturn'1_a843 = con tag
                                                in return' areturn'1_a843
                in z62z62z61z33 upperword az62z62z61z332_a844
         lamp = let
                  az62z62z61z332_a851 = \var -> let
                                                  awCons2_a847 = wCons_a454 wz62_a548 wNil_a455
                                                  areserved1_a848 = wCons_a454 wz45_a577 awCons2_a847
                                                  az62z62z331_a849 = reserved areserved1_a848
                                                  az62z62z61z331_a846 = z62z62z33 az62z62z331_a849 exprp
                                                  az62z62z61z332_a845 = \valexpr -> let
                                                                                      areturn'1_a850 = wLam_a734 var valexpr
                                                                                    in return' areturn'1_a850
                                                in z62z62z61z33 az62z62z61z331_a846 az62z62z61z332_a845
                in z62z62z61z33 varnamep az62z62z61z332_a851
         casep = let
                   awCons2_a854 = wCons_a454 we_a470 wNil_a455
                   awCons2_a855 = wCons_a454 ws_a479 awCons2_a854
                   awCons2_a856 = wCons_a454 wa_a467 awCons2_a855
                   areserved1_a857 = wCons_a454 wc_a514 awCons2_a856
                   az62z62z331_a858 = reserved areserved1_a857
                   az62z62z61z331_a853 = z62z62z33 az62z62z331_a858 exprp
                   az62z62z61z332_a852 = \scexpr -> let
                                                      awCons2_a861 = wCons_a454 wf_a759 wNil_a455
                                                      areserved1_a862 = wCons_a454 wo_a511 awCons2_a861
                                                      az62z62z331_a864 = reserved areserved1_a862
                                                      az62z62z332_a863 = some altp
                                                      az62z62z61z331_a860 = z62z62z33 az62z62z331_a864 az62z62z332_a863
                                                      az62z62z61z332_a859 = \alts -> let
                                                                                       areturn'1_a865 = wCase_a730 scexpr alts
                                                                                     in return' areturn'1_a865
                                                    in z62z62z61z33 az62z62z61z331_a860 az62z62z61z332_a859
                 in z62z62z61z33 az62z62z61z331_a853 az62z62z61z332_a852
         altp = let
                  az62z62z61z332_a971 = \tag -> let
                                                  az62z62z61z331_a960 = many varnamep
                                                  az62z62z61z332_a959 = \vars -> let
                                                                                   awCons2_a963 = wCons_a454 wz62_a548 wNil_a455
                                                                                   areserved1_a964 = wCons_a454 wz45_a577 awCons2_a963
                                                                                   az62z62z331_a965 = reserved areserved1_a964
                                                                                   az62z62z61z331_a962 = z62z62z33 az62z62z331_a965 exprp
                                                                                   az62z62z61z332_a961 = \res -> let
                                                                                                                   areserved1_a968 = wCons_a454 wz59_a745 wNil_a455
                                                                                                                   az62z62z331_a967 = reserved areserved1_a968
                                                                                                                   awTup21_a969 = wPat_a737 tag vars
                                                                                                                   areturn'1_a970 = wTup2_a729 awTup21_a969 res
                                                                                                                   az62z62z332_a966 = return' areturn'1_a970
                                                                                                                 in z62z62z33 az62z62z331_a967 az62z62z332_a966
                                                                                 in z62z62z61z33 az62z62z61z331_a962 az62z62z61z332_a961
                                                in z62z62z61z33 az62z62z61z331_a960 az62z62z61z332_a959
                in z62z62z61z33 upperword az62z62z61z332_a971
         listp = let
                   az62z62z61z332_a880 = \item -> let
                                                    areserved1_a870 = wCons_a454 wz44_a744 wNil_a455
                                                    az62z62z331_a871 = reserved areserved1_a870
                                                    az62z62z61z331_a876 = z62z62z33 az62z62z331_a871 listp
                                                    az62z62z61z332_a875 = \rest -> let
                                                                                     awCons2_a872 = wCons_a454 rest wNil_a455
                                                                                     aapp2_a873 = wCons_a454 item awCons2_a872
                                                                                     areturn'1_a874 = app cons aapp2_a873
                                                                                   in return' areturn'1_a874
                                                    az60z124z621_a869 = z62z62z61z33 az62z62z61z331_a876 az62z62z61z332_a875
                                                    awCons2_a877 = wCons_a454 nil wNil_a455
                                                    aapp2_a878 = wCons_a454 item awCons2_a877
                                                    areturn'1_a879 = app cons aapp2_a878
                                                    az60z124z622_a868 = return' areturn'1_a879
                                                  in z60z124z62 az60z124z621_a869 az60z124z622_a868
                   az60z124z621_a867 = z62z62z61z33 exprp az62z62z61z332_a880
                   az60z124z622_a866 = return' nil
                 in z60z124z62 az60z124z621_a867 az60z124z622_a866
         varnamep = let
                      keywords = let
                                   awCons2_a976 = wCons_a454 wt_a476 wNil_a455
                                   awCons2_a985 = wCons_a454 we_a470 awCons2_a976
                                   awCons2_a982 = wCons_a454 wn_a493 wNil_a455
                                   awCons2_a977 = wCons_a454 we_a470 wNil_a455
                                   awCons2_a978 = wCons_a454 ws_a479 awCons2_a977
                                   awCons2_a980 = wCons_a454 wa_a467 awCons2_a978
                                   awCons2_a979 = wCons_a454 wf_a759 wNil_a455
                                   awCons1_a981 = wCons_a454 wo_a511 awCons2_a979
                                   awCons1_a983 = wCons_a454 wc_a514 awCons2_a980
                                   awCons2_a984 = wCons_a454 awCons1_a981 wNil_a455
                                   awCons1_a986 = wCons_a454 wi_a488 awCons2_a982
                                   awCons2_a987 = wCons_a454 awCons1_a983 awCons2_a984
                                   awCons1_a974 = wCons_a454 wl_a761 awCons2_a985
                                   awCons2_a975 = wCons_a454 awCons1_a986 awCons2_a987
                                 in wCons_a454 awCons1_a974 awCons2_a975
                      asat1_a973 = show keywords
                      az462_a989 = \rsect_a988 -> elem rsect_a988 keywords
                      asat3_a972 = z46 not az462_a989
                    in sat asat1_a973 varid asat3_a972
         varid = let
                   az60z124z621_a1071 = z60z124z62 loweralpha dollar
                   az62z62z61z331_a1063 = z60z124z62 az60z124z621_a1071 underscore
                   az62z62z61z332_a1062 = \c -> let
                                                  az60z124z621_a1067 = z60z124z62 alpha digit
                                                  az60z124z621_a1066 = z60z124z62 az60z124z621_a1067 underscore
                                                  amany1_a1068 = z60z124z62 az60z124z621_a1066 quote
                                                  az62z62z61z331_a1065 = many amany1_a1068
                                                  az62z62z61z332_a1064 = \cs -> let
                                                                                  areturn'1_a1069 = wCons_a454 c cs
                                                                                  az62z62z332_a1070 = return' areturn'1_a1069
                                                                                in z62z62z33 spaces az62z62z332_a1070
                                                in z62z62z61z33 az62z62z61z331_a1065 az62z62z61z332_a1064
                 in z62z62z61z33 az62z62z61z331_a1063 az62z62z61z332_a1062
         parseWith = \p -> \s -> let aparse1_a728 = z62z62z33 spaces p
                                 in case parse aparse1_a728 s of
                                      Done a chars rest ->
                                        case rest of
                                          [] -> a
                                          _ ->
                                            let
                                              awCons2_a457 = wCons_a454 wz60_a453 wNil_a455
                                              awCons2_a460 = wCons_a454 wz60_a453 awCons2_a457
                                              awCons2_a463 = wCons_a454 wz32_a458 awCons2_a460
                                              awCons2_a466 = wCons_a454 wz58_a461 awCons2_a463
                                              awCons2_a469 = wCons_a454 wm_a464 awCons2_a466
                                              awCons2_a472 = wCons_a454 wa_a467 awCons2_a469
                                              awCons2_a475 = wCons_a454 we_a470 awCons2_a472
                                              awCons2_a478 = wCons_a454 wr_a473 awCons2_a475
                                              awCons2_a481 = wCons_a454 wt_a476 awCons2_a478
                                              awCons2_a483 = wCons_a454 ws_a479 awCons2_a481
                                              awCons2_a485 = wCons_a454 wz32_a458 awCons2_a483
                                              awCons2_a487 = wCons_a454 we_a470 awCons2_a485
                                              awCons2_a490 = wCons_a454 wr_a473 awCons2_a487
                                              awCons2_a492 = wCons_a454 wi_a488 awCons2_a490
                                              awCons2_a495 = wCons_a454 wt_a476 awCons2_a492
                                              awCons2_a497 = wCons_a454 wn_a493 awCons2_a495
                                              awCons2_a499 = wCons_a454 we_a470 awCons2_a497
                                              awCons2_a501 = wCons_a454 wz32_a458 awCons2_a499
                                              awCons2_a503 = wCons_a454 we_a470 awCons2_a501
                                              awCons2_a506 = wCons_a454 wm_a464 awCons2_a503
                                              awCons2_a508 = wCons_a454 wu_a504 awCons2_a506
                                              awCons2_a510 = wCons_a454 ws_a479 awCons2_a508
                                              awCons2_a513 = wCons_a454 wn_a493 awCons2_a510
                                              awCons2_a516 = wCons_a454 wo_a511 awCons2_a513
                                              awCons2_a518 = wCons_a454 wc_a514 awCons2_a516
                                              awCons2_a520 = wCons_a454 wz32_a458 awCons2_a518
                                              awCons2_a523 = wCons_a454 wt_a476 awCons2_a520
                                              awCons2_a525 = wCons_a454 w'_a521 awCons2_a523
                                              awCons2_a528 = wCons_a454 wn_a493 awCons2_a525
                                              awCons2_a530 = wCons_a454 wd_a526 awCons2_a528
                                              awCons2_a532 = wCons_a454 wi_a488 awCons2_a530
                                              awCons2_a534 = wCons_a454 wd_a526 awCons2_a532
                                              awCons2_a536 = wCons_a454 wz32_a458 awCons2_a534
                                              awCons2_a538 = wCons_a454 wr_a473 awCons2_a536
                                              awCons2_a540 = wCons_a454 we_a470 awCons2_a538
                                              awCons2_a542 = wCons_a454 ws_a479 awCons2_a540
                                              awCons2_a544 = wCons_a454 wr_a473 awCons2_a542
                                              awCons2_a547 = wCons_a454 wa_a467 awCons2_a544
                                              az43z431_a668 = wCons_a454 wP_a545 awCons2_a547
                                              awCons2_a551 = wCons_a454 wz32_a458 wNil_a455
                                              awCons2_a553 = wCons_a454 wz62_a548 awCons2_a551
                                              az43z431_a663 = wCons_a454 wz62_a548 awCons2_a553
                                              awCons2_a556 = wCons_a454 wz60_a453 wNil_a455
                                              awCons2_a558 = wCons_a454 wz60_a453 awCons2_a556
                                              awCons2_a560 = wCons_a454 wz32_a458 awCons2_a558
                                              awCons2_a562 = wCons_a454 wn_a493 awCons2_a560
                                              awCons2_a564 = wCons_a454 wi_a488 awCons2_a562
                                              az43z431_a660 = wCons_a454 wz32_a458 awCons2_a564
                                              awCons2_a567 = wCons_a454 wz32_a458 wNil_a455
                                              awCons2_a569 = wCons_a454 wt_a476 awCons2_a567
                                              awCons2_a571 = wCons_a454 wa_a467 awCons2_a569
                                              awCons2_a573 = wCons_a454 wz32_a458 awCons2_a571
                                              awCons2_a575 = wCons_a454 wz62_a548 awCons2_a573
                                              az43z431_a655 = wCons_a454 wz62_a548 awCons2_a575
                                              az43z431_a652 = show chars
                                              awCons2_a582 = wCons_a454 wz41_a576 wNil_a455
                                              awCons2_a584 = wCons_a454 wz45_a577 awCons2_a582
                                              awCons2_a587 = wCons_a454 wz45_a577 awCons2_a584
                                              awCons2_a589 = wCons_a454 ww_a585 awCons2_a587
                                              awCons2_a592 = wCons_a454 wo_a511 awCons2_a589
                                              awCons2_a594 = wCons_a454 wh_a590 awCons2_a592
                                              awCons2_a596 = wCons_a454 ws_a479 awCons2_a594
                                              awCons2_a598 = wCons_a454 wz45_a577 awCons2_a596
                                              awCons2_a600 = wCons_a454 wz45_a577 awCons2_a598
                                              awCons2_a603 = wCons_a454 wz32_a458 awCons2_a600
                                              awCons2_a606 = wCons_a454 wy_a601 awCons2_a603
                                              awCons2_a608 = wCons_a454 wb_a604 awCons2_a606
                                              awCons2_a610 = wCons_a454 wz32_a458 awCons2_a608
                                              awCons2_a612 = wCons_a454 wd_a526 awCons2_a610
                                              awCons2_a614 = wCons_a454 we_a470 awCons2_a612
                                              awCons2_a616 = wCons_a454 wt_a476 awCons2_a614
                                              awCons2_a618 = wCons_a454 wi_a488 awCons2_a616
                                              awCons2_a620 = wCons_a454 wm_a464 awCons2_a618
                                              awCons2_a622 = wCons_a454 wm_a464 awCons2_a620
                                              awCons2_a624 = wCons_a454 wo_a511 awCons2_a622
                                              awCons2_a626 = wCons_a454 wz32_a458 awCons2_a624
                                              awCons2_a629 = wCons_a454 wr_a473 awCons2_a626
                                              awCons2_a632 = wCons_a454 wp_a627 awCons2_a629
                                              awCons2_a634 = wCons_a454 wx_a630 awCons2_a632
                                              awCons2_a637 = wCons_a454 we_a470 awCons2_a634
                                              awCons2_a639 = wCons_a454 wz40_a635 awCons2_a637
                                              awCons2_a641 = wCons_a454 wz32_a458 awCons2_a639
                                              awCons2_a643 = wCons_a454 wh_a590 awCons2_a641
                                              awCons2_a645 = wCons_a454 wt_a476 awCons2_a643
                                              awCons2_a647 = wCons_a454 wi_a488 awCons2_a645
                                              awCons2_a649 = wCons_a454 ww_a585 awCons2_a647
                                              az43z432_a651 = wCons_a454 wz32_a458 awCons2_a649
                                              az43z432_a654 = z43z43 az43z431_a652 az43z432_a651
                                              az43z432_a657 = z43z43 az43z431_a655 az43z432_a654
                                              az43z432_a659 = z43z43 s az43z432_a657
                                              az43z432_a662 = z43z43 az43z431_a660 az43z432_a659
                                              az43z432_a665 = z43z43 az43z431_a663 az43z432_a662
                                              az43z432_a667 = z43z43 rest az43z432_a665
                                              az362_a670 = z43z43 az43z431_a668 az43z432_a667
                                            in z36 error az362_a670
                                      Error msg ->
                                        let
                                          awCons2_a673 = wCons_a454 w'_a521 wNil_a455
                                          awCons2_a675 = wCons_a454 w'_a521 awCons2_a673
                                          awCons2_a678 = wCons_a454 ws_a479 awCons2_a675
                                          awCons2_a681 = wCons_a454 wz37_a676 awCons2_a678
                                          awCons2_a683 = wCons_a454 wz96_a679 awCons2_a681
                                          awCons2_a685 = wCons_a454 wz96_a679 awCons2_a683
                                          awCons2_a687 = wCons_a454 wz32_a458 awCons2_a685
                                          awCons2_a689 = wCons_a454 wn_a493 awCons2_a687
                                          awCons2_a691 = wCons_a454 wi_a488 awCons2_a689
                                          awCons2_a693 = wCons_a454 wz32_a458 awCons2_a691
                                          awCons2_a695 = wCons_a454 ws_a479 awCons2_a693
                                          awCons2_a697 = wCons_a454 wz37_a676 awCons2_a695
                                          awCons2_a699 = wCons_a454 wz32_a458 awCons2_a697
                                          awCons2_a701 = wCons_a454 wz58_a461 awCons2_a699
                                          awCons2_a703 = wCons_a454 wr_a473 awCons2_a701
                                          awCons2_a705 = wCons_a454 wo_a511 awCons2_a703
                                          awCons2_a707 = wCons_a454 wr_a473 awCons2_a705
                                          awCons2_a709 = wCons_a454 wr_a473 awCons2_a707
                                          awCons2_a711 = wCons_a454 we_a470 awCons2_a709
                                          awCons2_a713 = wCons_a454 wz32_a458 awCons2_a711
                                          awCons2_a715 = wCons_a454 wr_a473 awCons2_a713
                                          awCons2_a717 = wCons_a454 we_a470 awCons2_a715
                                          awCons2_a719 = wCons_a454 ws_a479 awCons2_a717
                                          awCons2_a721 = wCons_a454 wr_a473 awCons2_a719
                                          awCons2_a723 = wCons_a454 wa_a467 awCons2_a721
                                          aprintf1_a725 = wCons_a454 wP_a545 awCons2_a723
                                          az362_a727 = printf aprintf1_a725 msg s
                                        in z36 error az362_a727
         parseExpr = parseWith exprp
         root = parseExpr
       in root
tests = let
          wNil_a455 = []
          tests = wNil_a455
        in tests
