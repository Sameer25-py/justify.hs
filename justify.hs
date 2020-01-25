-----------------------------------------------------------------------------------
import Data.Char
import Data.List
-----------------------------------------------------------------------------------
data Token = Word String| Blank | Hypword String deriving (Show,Eq) 

--Q1
type Line=[]Token

str2line :: String -> Line
str2line= \text -> map (Word)(words text)

--Q2
--helper 
stringer :: Line->[String]
stringer tokens=case tokens of
                  []->[]
                  (Word x : xs)->[x] ++ stringer xs
                  (Hypword x : xs)-> [(x ++ "-")] ++ stringer xs 

line2str:: Line->String
line2str tokens = unwords (stringer tokens)

--Q3
toklen :: Token-> Int
toklen x=case x of
          Blank->1
          Word v -> length v
          Hypword v -> length v


--Q4
linelen:: Line -> Int
linelen tokens= case tokens of
                  [x]->toklen x
                  (x:xs)-> toklen x + 1 + linelen xs


--Q5
--helper
first::Int -> Line ->Line
first x tokens = case tokens of 
              []->[]
              (v:vs)|toklen(v) <= x->[v] ++ first (x -toklen(v)) vs
              _->[]

second :: Line -> Line-> Line
second tokens original= let len=length tokens
                        in drop len original

breakline :: Int->Line -> (Line,Line)
breakline x tokens= let f=first x tokens
                  in (f,second f tokens)


--Q6
mergers :: [String]->[(String,String)]
mergers a = case a of   
          [x]->[]
          [x,y]->[(x,y)]
          (v:vs)|length vs >=1->[(v,concat vs)] ++ mergers first
                where first = [v++head(vs)]++ tail vs


--Q7
enHyp=[("controls",["co","nt","ro","ls"]),("future",["fu","tu","re"]),("present",["pre","se","nt"])]
--helper
givesecond:: [(String,[String])] -> String -> [String]
givesecond hyp v= case hyp of
                  []->[""]
                  (x : xs)| (fst(x) == v)->snd(x)
                  _:xs-> givesecond xs v
adder :: [String] -> String ->[String]
adder ls pun = let 
                end = last ls
                in (init ls) ++ [end ++ pun]

mapper :: [(String,String)] -> [(Token,Token)]
mapper ls = case ls of 
        [] -> []
        (x : xs ) -> 
                  let (y,z) = x
                  in [(Hypword y,Word z )] ++ mapper xs

hyphenate:: [(String,[String])] -> Token -> [(Token,Token)]
hyphenate hyp token = case token of
                       
                       Word x | z /= "" && (find (==y)(map (fst) hyp))==Just y ->mapper( mergers (adder (givesecond hyp y) z ))
                              where (y,z)=span (isAlpha) x
                       Word x -> mapper (mergers (givesecond hyp x ) )     


--Q8
--helper
tupler :: Line->Line->Line->[(Line,Line)]
tupler ls1 ls2 tokens = case ls1 of 
                          [] -> []
                          x:xs->
                            let y:ys=ls2
                            in [(tokens ++ [x],[y])] ++ tupler xs ys tokens                       

filter':: (Line,Line) -> Int
filter' x = linelen (fst x) 

linebreaks :: [(String,[String])] -> Int -> Line -> [(Line,Line)]
linebreaks hyp x tokens = case tokens of 
                              tokens | z==[]->[(y,z)]
                              tokens | z/=[]-> filter (\v-> (filter' v) <= x) ([(y,z)] ++ tupler ls1 ls2 y) 

                              where (y,z)=breakline x tokens
                                    ls1=map (fst) (hyphenate hyp (head z))
                                    ls2=map (snd) (hyphenate hyp (head z))                                


--Q9
--helper
inserter:: [a] -> a ->Int -> Int -> [[a]]
inserter a b c d = case c of 
                    c | c <= d -> [(take c a) ++ [b] ++  (drop c a)]++ inserter a b (c+1) d
                    c->[] 

insertions ::  a-> [a] -> [[a]]
insertions a b = inserter b a 0 (length b)             


--Q10
--helper 
inserter':: [Line] -> [Line]
inserter' x = case x of 
                [] -> []
                x:xs-> (insertions Blank x) ++ inserter' xs

inserter'' :: [Line]->Int->[Line]
inserter'' x y = case y of
                  y | y >0 -> nub ((inserter' x) ++ (inserter''( inserter' x) (y-1)))
                  y->[]                

filter'' :: [Line]->[Line]
filter'' ls = case ls of 
            [] -> []
            (x:xs)|(head(x)==Blank || last(x)==Blank)->filter'' xs
            (x:xs)-> [x] ++ filter'' xs  
               
insertblanks :: Int -> Line -> [Line]
insertblanks x y = case x of
                    x| (x<=0)->[]
                    x| (x==1) -> filter'' (insertions Blank y)
                    x-> filter'' (inserter'' (insertions Blank y) (x-1))


--Q11
blankdistances ::(Fractional a)=> Line -> [a]
blankdistances x = case x of 
                      []->[]
                      x|Blank `elemIndex` x == Nothing -> [fromIntegral(length x)]
                      x|Blank `elemIndex` x /= Nothing-> [fromIntegral a] ++ blankdistances b
                        where Just a =Blank `elemIndex` x 
                              b=drop (a+1) x


--Q12
avg x = sum x / fromIntegral (length x)
var x = let av= avg x
        in avg (map (\v-> (v-av)**2) x )


--Q13
---------------------------------------------------------------------------------------------------------
data Cost = Cost Double Double Double Double deriving (Eq,Show)
---------------------------------------------------------------------------------------------------------
defaultcost = Cost 0 0 0 0

blanks:: Line -> Int
blanks x = case x of 
           [] -> 0
           (v:vs)| v == Blank -> 1 + blanks vs 
           _:vs-> blanks vs

hyps::Line -> Int 
hyps x = case x of 
           [] -> 0
           (v:vs) | (v == Hypword z) -> 1 + hyps vs
                  where  Hypword z=v
           (_:vs)-> hyps vs     
                  
        

proxim::(Fractional a)=> Line ->  a
proxim x = case x of 
            [] -> 0
            x|(blanks x) /=0-> fromIntegral(length x)-avg (blankdistances x)    
            x -> 0

linebadness:: Cost -> Line -> Double
linebadness x y = let Cost a b c d = x
                      v1=a+fromIntegral(blanks y)
                      v2=b + proxim y
                      v3=c + var(blankdistances y)
                      v4=d + fromIntegral(hyps y)
                  
                  in  v1+v2+v3+v4

