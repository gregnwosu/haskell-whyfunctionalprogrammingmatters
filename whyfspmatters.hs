module Why where

import Data.List (sortBy)

mysum2:: (Num a) => [a] -> a 
mysum2 = foldr (+) 0




myappend::[a] -> [a] -> [a]
myappend a b = foldr (:) b a

data Treeof a =  Node a [Treeof a]
-- the initial fold is the samce type as the complement to the node value (subtree),
-- these need to be the same type
-- in essence we want to know 
-- a) how to reduce a node with its composite/initial value to create an output
-- b) how to take a output ( a reduced node) and combine it with the remaining composite
foldtree  ::
-- combining  the node value with composite part output ([]/Nil replacment)  (Replacing Node v comp) 
   (nodevalue_t -> composite_t -> output_t) -> 
-- combining  output with second part of node
 (output_t -> composite_t -> composite_t) -> 
-- nil replacement
   composite_t ->
-- composite to map over 
   Treeof nodevalue_t -> 
-- folded output type
   output_t

foldtree f g a (Node label subtrees) = f label (foldsubtrees f g a subtrees)

foldsubtrees :: (nodevalue_t -> composite_t -> output_t) ->
               (output_t -> composite_t -> composite_t) ->
               composite_t ->
               [Treeof nodevalue_t] ->
               composite_t
foldsubtrees _ _ a [] = a
foldsubtrees f g a (subtree:rest) = g (foldtree f g a subtree) (foldsubtrees f g a rest)
sumtree :: (Num label_t) => Treeof label_t -> label_t
sumtree = foldtree (+) (+) 0

labels :: Treeof value_t -> [value_t]
labels = foldtree (:) (++) []

maptree :: (nodevalue_t -> output_t) ->
          Treeof nodevalue_t ->
          Treeof output_t
maptree f = foldtree (Node . f) (:) [] 

exampletree :: (Num a) => Treeof a 
exampletree = Node 1 [Node 2 [], Node 3[Node 4[]]]


multtree = maptree (*)

rept ::    (a -> a) ->
                    a ->
                    [a] 
rept f a = a : rept f (f a)

next :: (Fractional a) =>
                   a ->
                   a ->
                   a
next n x = (x + (n/x))/2

within :: (Num b, Ord b) => b -> [b] -> b
within eps (a:b:rest)
  | (a - b ) <= eps = b
  | otherwise = within eps (b:rest)

relative :: (Fractional b, Ord b) => b -> [b] -> b
relative eps (a:b:rest) 
  | ((a/b) -1) <= eps = b
  | otherwise        = relative eps (b:rest) 

mysqrt :: (Fractional a, Ord a) => a -> a  ->a -> a
mysqrt a0 eps n = within eps (rept (next n) a0)

relsqrt :: (Fractional a, Ord a) => a -> a  ->a -> a
relsqrt a0 eps n = relative eps (rept (next n) a0)

easydiff :: (Fractional x) => (x->x)->x->x->x
easydiff f x h = (f (x + h) - f x)/h

differentiate ::  (Fractional x ) =>  x -> (x -> x)->x -> [x]
differentiate h0 f x = map (easydiff f x ) (rept halve h0)

halve :: (Fractional x)=> x -> x
halve h0 = h0 /2

elimerror :: Fractional x => Integer -> [x] ->[x]
elimerror n (a:b:rest) = (b * (2^n) - a)/ ((2^n)-1):elimerror n ( b:rest)    

order :: (RealFrac x, Floating x) => [x]-> Integer
order (a:b:c:_) = round (logBase 2 ((a-c)/(b-c)-1))
improve :: (RealFrac x, Floating x) => [x] -> [x]
improve s 
  | theorder > 0 = elimerror theorder s
  | otherwise = elimerror  1 s
 where theorder = order s

second :: (RealFrac x, Floating x) => [x] -> x
second (a:_:_) = a

super ::(RealFrac x, Floating x, Fractional x) => [x] -> [x]
super s = map second (rept improve s)

easyintegrate :: (RealFrac a)=>(a -> a) -> a -> a -> a
easyintegrate f a b = (f a + f b) * (b-a)  / 2

integrate :: (RealFrac a) => (a -> a) -> a -> a ->[a]
integrate f a b = easyintegrate f a b : map addpair (zip (integrate f a mid) (integrate f mid b))
                where mid=(a+b)/2
                     
addpair:: ( RealFrac a)=>(a,a) -> a
addpair (a,b) = a + b

integrate'::  (Double -> Double) -> Double -> Double->  [Double]
integrate' f a b  = integ f a b (f a) (f b)
integ ::  (RealFrac a, Fractional a ) => (a -> a) -> a ->a ->a->a -> [a]
integ f a b fa fb = ((fa+fb)*(b-a)/2):map addpair (zip (integ f a m fa fm) (integ f m b fm fb ))
                    where fm = f m
                          m = (a+b)/2
                          




w tolerance initialguess function pointOnCurve = within tolerance  (super (differentiate initialguess function pointOnCurve))

   

data BoardPositionState = Cross | Nought | Empty deriving (Eq, Show)
type Board =  [BoardPositionState]



--conventionis crosses move first
iscrossmove :: Board -> Bool
iscrossmove   = even . length . filter (==Cross)


--to do: need to figure out a way of working outwhose move it is
getmoveszipper :: Board -> Board -> [Board]
getmoveszipper  _ [] = []
getmoveszipper  y z@(Empty:xs)  = ns : getmoveszipper (y++[Empty])  xs
                                where 
                                      nextplayer = if iscrossmove z then Cross else Nought
                                      ns = y ++ (nextplayer:xs)
                                      
getmoveszipper  y (x:xs) = getmoveszipper (y++[x])  xs

moves :: Board ->[Board]
moves = getmoveszipper [] 

reptree :: (a -> [a]) -> a -> Treeof a
reptree f a = Node a (map (reptree f) (f a))

gametree :: Board -> Treeof Board
gametree = reptree moves 



mapmin :: (Ord x) => [[x]] ->[x] 
mapmin (nums:rest) = minnums: (omit minnums rest)
                     where minnums = minimum nums
mapmax :: (Ord x) => [[x]] ->[x] 
mapmax (nums:rest) = maxnums : (omit' maxnums rest)
                     where maxnums = maximum nums
minleq' [] pot = False
minleq' (n:rest) pot 
         | n > pot = True
         | otherwise = minleq' rest pot

minleq [] pot = False
minleq (n:rest) pot 
         | n < pot = True
         | otherwise = minleq rest pot


omit pot []= []
omit pot (nums:rest) 
      | minleq nums pot = omit pot rest 
      | otherwise = (minimum nums) : (omit (minimum nums) rest)


omit' pot []= []
omit' pot (nums:rest) 
      | minleq' nums pot = omit pot rest 
      | otherwise = (minimum nums) : (omit (minimum nums) rest)


minimize':: (Ord n) => Treeof n -> [n]
minimize' (Node n []) = [n]
minimize' (Node _ sub) = mapmax (map maximize' sub)
          
maximize':: (Ord n) => Treeof n -> [n]
maximize' (Node n []) = [n]
maximize' (Node _ sub) = mapmin (map minimize' sub)


minimize:: Treeof Int -> Int
minimize = minimum . minimize'


maximize ::  Treeof Int -> Int
maximize = maximum . maximize'

iswin:: BoardPositionState -> Board -> Bool
iswin Empty _ = undefined
iswin p b = all (p==) b


checkwin :: BoardPositionState -> Board -> Bool
checkwin p b = any (iswin p) $ map (map (b!!))[[0,1,2], [3,4,5], [6,5,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]]

otherPlayer:: BoardPositionState -> BoardPositionState
otherPlayer Empty = undefined
otherPlayer Nought = Cross
otherPlayer Cross = Nought

static :: BoardPositionState -> Board -> Int
static p board
       | checkwin p board = 1
       | checkwin (otherPlayer p) board  = -1
       | otherwise = 0

prune :: Int -> Treeof a -> Treeof a
prune 0 (Node a _) = Node a []
prune n (Node a subtree) = Node a $ map (prune (n-1)) subtree

evaluate :: Board -> Int
evaluate  = maximum . maximize' .  maptree (static Nought). prune 5 . gametree




invert :: Ordering -> Ordering
invert a = compare EQ a

lower:: Ord a => Treeof a -> Treeof a -> Ordering
lower a b =  invert (higher a b)

lowfirst::Ord a => Treeof a -> Treeof a
lowfirst (Node n sub) = Node n (sortBy lower (map highfirst sub))

highfirst:: Ord a => Treeof a -> Treeof a
highfirst (Node n sub) = Node n (sortBy higher     (map lowfirst sub) ) 

higher:: Ord a => Treeof a -> Treeof a -> Ordering
higher (Node a _) (Node b _) = compare a b



