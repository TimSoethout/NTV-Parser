
module Parser where

--data Expr = Lambda Axiom Term
--            deriving Show
            
data Term = Prop Axiom
          | Lambda Axiom Term
          | Term Term Term
            deriving Eq
--            deriving (Eq , Show)

type Axiom = [Char]

instance Show Term where
  show (Prop a) = a
  show (Lambda a b) = '\\' : ((a) ++ ".(" ++ (show b) ++ ")")
  show (Term a b) = '(' : (show a) ++ ' ' : (show b) ++ ")"           
            

showTermArray :: (Term -> String) -> [Term] -> String  
showTermArray showType t = unlines $ map showType t   

printTermArray = putStrLn.(showTermArray show)

termToLatex :: Term -> String
termToLatex (Prop a) = a
termToLatex (Lambda a b) = "\\lambda " ++ ((a) ++ ".(" ++ (termToLatex b) ++ ")")
termToLatex (Term a b) = '(' : (termToLatex a) ++ "\\ " ++ (termToLatex b) ++ ")"      

printTermArrayLatex = putStrLn.(showTermArray termToLatex)     
--data Axiom = Letter Char
--             deriving Show


testTerm = Term (Lambda "a" (Prop "a")) (Lambda "b" (Term (Prop "c") (Prop "b") ))
testTerm2 = (Lambda "b" (Term (Prop "c") (Prop "b") ))
testTerm3 = Term (Lambda "b" (Term (Prop "b") (Prop "c") )) 
                 (Term (Lambda "a" (Prop "a")) 
                       (Lambda "b" (Term (Prop "c")
                                         (Prop "b") 
                                   )
                       )
                 )
testTerm4 = Term (Lambda "a" (Term (Prop "a") (Prop "a")))
                 (Prop "b")
  
-- Terms     
someone, everyone, loves, mary, thinks, left :: Term                 
someone = Lambda "ks" (Term (Prop "E")
                            (Lambda "xs" (Term (Prop "ks")
                                              (Prop "xs") )) )      
everyone = Lambda "ke" (Term (Prop "A")
                            (Lambda "xe" (Term (Prop "ke")
                                              (Prop "xe") )) )         
loves = Lambda "kl" (Term (Prop "kl")
                          (Prop "LOVES" ))      

mary =  Lambda "km" (Term (Prop "km")
                          (Prop "MARY" ))    
                              
left =  Lambda "kl2" (Term (Prop "kl2")
                          (Prop "LEFT" ))                       
                              
thinks =  Lambda "kt" (Term (Prop "kt")
                          (Prop "THINKS" ))                            
                 
--els = Term2 (Prop2 (Prop "everyone")) (Term2 (Prop2 (Prop "loves")) (Prop2 (Prop "someone")) )
-- (everyone > (loves < someone))<
elsn = cbn '>' (Term2 (Prop2 everyone) (Prop2 lsn) )
lsn = cbn2 '<' (Term2 (Prop2 loves) (Prop2 someone) )

-- (Mary > (thinks (someone > left)))<

sln = cbn '>' (Term2 (Prop2 someone) (Prop2 left) )
tsln = cbn2 '<' (Term2 (Prop2 sln) (Prop2 thinks) )
mtsln = cbn3 '>' (Term2 (Prop2 mary) (Prop2 tsln) )

-- *Parser> writeFile "test.txt" $ showTermArray termToLatex $ take 20 $ iterate beta2 (mtsln)
-- *Parser> printTermArray $ take 20 $ iterate beta2 (mtsln)

--Lambda "k2" (Term (Lambda "ks" 
--                          (Term (Prop "E") 
--                                (Lambda "xs" 
--                                        (Term (Prop "ks")
--                                        (Prop "xs")))
--                  ))
--                  (Lambda "n2" (Term (Lambda "kl" 
--                                             (Term (Prop "kl") 
--                                                   (Prop "LOVES"))
--                                     ) 
--                                     (Lambda "m2" (Term (Prop "k2")
--                                                        (Term (Prop "m2")
--                                                              (Prop "n2"))))
--                  ))
--            )
            
testTerm6 = Lambda "k2" (Term (Lambda "ks" 
                          (Prop "ks")
                  )
                  (Lambda "n2"
                          (Prop "TWEE")
                  )
            )


data Term2 = Prop2 Term
           | Term2 Term2 Term2
             deriving Show
           

--cbn
cbn :: Char -> Term2 -> Term    
cbn '>' a = cbn '<' a              
cbn '<' (Term2 m n) = Lambda "k1"
                     (Term (cbn '<' n)
                           (Lambda "n1" 
                                         (Term (cbn '<' m)
                                               (Lambda "m1" 
                                                       (Term (Prop "k1") 
                                                             (Term (Prop "m1")
                                                                   (Prop "n1")
                                                             )
                                                       )
                                               )                                            
                                         )
                                  )
                           )
cbn _ (Prop2 a) = a

cbn2 :: Char -> Term2 -> Term    
cbn2 '>' a = cbn2 '<' a              
cbn2 '<' (Term2 m n) = Lambda "k2"
                     (Term (cbn2 '<' n)
                           (Lambda "n2" 
                                         (Term (cbn2 '<' m)
                                               (Lambda "m2" 
                                                       (Term (Prop "k2") 
                                                             (Term (Prop "m2")
                                                                   (Prop "n2")
                                                             )
                                                       )
                                               )                                            
                                         )
                                  )
                           )
cbn2 _ (Prop2 a) = a

cbn3 :: Char -> Term2 -> Term    
cbn3 '>' a = cbn3 '<' a              
cbn3 '<' (Term2 m n) = Lambda "k3"
                     (Term (cbn3 '<' n)
                           (Lambda "n3" 
                                         (Term (cbn3 '<' m)
                                               (Lambda "m3" 
                                                       (Term (Prop "k3") 
                                                             (Term (Prop "m3")
                                                                   (Prop "n3")
                                                             )
                                                       )
                                               )                                            
                                         )
                                  )
                           )
cbn3 _ (Prop2 a) = a

-- (everyone > (loves < someone))>
elsv = cbv '>' (Term2 (Prop2 everyone) (Prop2 lsv) )
lsv = cbv2 '<' (Term2 (Prop2 loves) (Prop2 someone) )

--cbv
cbv :: Char -> Term2 -> Term  
cbv a (Term2 m n) = cbn a (Term2 n m) 

cbv2 a (Term2 m n) = cbn2 a (Term2 n m) 

cbv3 a (Term2 m n) = cbn3 a (Term2 n m) 
                      
            
-- initiates lambda application
betaBegin :: Term -> Maybe Term
betaBegin (Term a@(Lambda _ _) b) = Just (beta a b)     
betaBegin _ = Nothing       

-- continues reduction
--applicate :: Term -> Term
--applicate a = until ((==Nothing)) (betaBegin.takeJust) (Just a)



{--
showTermFilter :: String -> String
showTermFilter b = tail $ dropWhile (not.(=='(')) b
showTermFilter (a:b) =  
--}

mapTerm :: (Axiom -> Axiom) -> Term -> Term
mapTerm f (Term a b)  = Term (mapTerm f a) (mapTerm f b)
mapTerm f (Lambda a b) = (Lambda a (mapTerm f b)) 
mapTerm f (Prop a)  = Prop (f a)

-- not working
alpha :: Term -> Term
alpha (Lambda a b) = Lambda a (alphaHulp a (a ++ "1") b)
alpha a = a 

--erm -> String -> Term
alphaHulp :: Axiom -> Axiom -> Term -> Term
alphaHulp oldAxiom newAxiom term = mapTerm (replace oldAxiom newAxiom) term

replace :: Axiom -> Axiom -> Axiom -> Axiom
replace old replace toReplace | old == toReplace = replace
                              | otherwise = old


takeJust :: Maybe a -> a
takeJust (Just a) = a
  
reduce :: Term -> [Term]
reduce = iterate beta2
  
beta2 :: Term -> Term
beta2 (Term (Lambda l t) b) = betaHulp l t b
beta2 (Term a b) = Term (beta2 a) (beta2 b)
beta2 (Lambda a b) = Lambda a (beta2 b)
beta2 a = a  
           
beta :: Term -> Term -> Term
beta (Term a b) _ = beta a b
beta (Lambda a b) newTerm = betaHulp a b newTerm
beta a _ = a
--beta (Term a b) newTerm = Term (beta a newTerm) (beta b newTerm)
--beta (Prop _) newTerm = newTerm
--beta (Lambda a (Prop b)) (Prop replace) = 
--beta (Lambda a (Prop b)) (Prop replace) | a == b = Prop replace
--                                        | otherwise = Prop 'o'   
                         
-- to Replace
-- oldTerm
-- replace              
-- \x    .M    [a     -> y   ]
-- \Axiom.Term [Axiom -> Term]                          
betaHulp :: Axiom -> Term -> Term -> Term
betaHulp a (Term t1 t2) newTerm = Term (betaHulp a t1 newTerm) (betaHulp a t2 newTerm)
betaHulp a t@(Prop b) replace | a == b = replace
                              | otherwise = t
betaHulp a (Lambda l t) replace = (Lambda l (betaHulp a t replace))