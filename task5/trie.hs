import qualified Data.Map as DM

data Trie = Trie Int (DM.Map Char Trie) deriving (Show)

empty :: Trie
empty = Trie 0 DM.empty

insert :: String -> Trie -> Trie
insert [] (Trie freq nodes) = Trie (freq + 1) nodes
insert (x:xs) (Trie freq nodes) = case DM.lookup x nodes of
        Just node -> Trie freq (DM.insert x (insert xs node) nodes)
        Nothing -> Trie freq (DM.insert x (insert xs empty) nodes)

makeTrie :: [String] -> Trie -> Trie
makeTrie [] trie = trie
makeTrie (x:xs) trie = makeTrie xs $ insert x trie

trieBuild :: String -> Trie
trieBuild xs = makeTrie (words xs) empty

trieGet :: Trie -> String -> Int
trieGet (Trie freq _) [] = freq
trieGet (Trie _ nodes) (x:xs) = case DM.lookup x nodes of
        Just node -> trieGet node xs      
        Nothing -> 0