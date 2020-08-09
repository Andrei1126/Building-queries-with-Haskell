
{-
  Micut Andrei-Ion
  Grupa 321CB
-}

module Query where

import Movie
import Rating
import UserInfo
import Data.List

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry] --deriving Show

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f (x : xs)
  | f x = splitOn f xs
  | otherwise = let (h, t) = break f (x : xs) in h : (splitOn f t)

-- Am facut split pe linii in functie de '-' pentru a avea o lista de linii

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col_sep ln_sep string=Table (head(map(splitOn (==col_sep)) (splitOn (==ln_sep) string))) 
                                  (tail (map (splitOn ( == col_sep)) (splitOn (== ln_sep) string)))


--TODO 2
instance Show Table where
   show (Table header entries)=(replicate((sum(maxi'(Table header entries)))+length header+1) '-')
                               ++ "\n|" ++ (showHeader header (maxi' (Table header entries))) ++ 
                               (replicate((sum(maxi'(Table header entries)))+length header+1) '-')
                               ++ "\n" ++ (showEntries entries (maxi' (Table header entries))) 
                               ++(replicate((sum(maxi'(Table header entries)))+length header+1)'-')
                               ++"\n"

-- imi va afisa header-ul tabelei
showHeader :: TableSchema -> [Int] -> String
showHeader [] _ = "\n"
showHeader columns list=(head columns) ++ replicate((head list) - length (head columns)) ' ' ++ "|" 
                        ++ (showHeader (tail columns) (tail list)) 

-- imi va afisa entry-urile tabelei
showEntries :: [Entry] -> [Int] -> String
showEntries [] _ = ""
showEntries entries list="|" ++ showHeader (head entries) list ++ (showEntries (tail entries) list)

-- pentru a testa mai usor
movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str
user_info = read_table '|' '\n' user_info_str

--pentru fiecare linie intoarce maximul
maxim :: [Entry] -> [Int]
maxim ([] : _) = []
maxim entries = (max' (map head entries) 0) : maxim (map tail entries)

--intoarce maximul
max' :: Entry -> Int -> Int
max' [] maxi = maxi
max' entry maxi = if(length (head entry) > maxi)
                        then max' (tail entry) (length (head entry))
                        else max' (tail entry) maxi

--returneaza o lista de maxim pentru fiecare coloana 
maxi' :: Table -> [Int]
maxi' (Table header entries) = maxim (header : entries)

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
--getFilter (Lt field integer) [] = func [] 0
getFilter (Lt field integer) header = if(field /= (head header))
                                            then getFilter (Lt field integer) (tail header)
                                             else (\header -> func (tail header) integer)


getFilter (Eq field string) header=(\entry -> head (drop (nr_column header field 0) entry)==string)

getFilter (In field string) header = if(field /= (head header))
                                            then getFilter (In field string) (tail header)
                                            else (\header -> func2 (tail header))
getFilter (Not filter) header = not.getFilter filter header 

-- functie pentru Lt
func :: Entry -> Integer -> Bool
func [] integer = False
func entry integer = if((read_Int (head entry)) > fromInteger integer)
                          then func (tail entry) integer
                          else True


func1 :: Entry -> String -> Bool
func1 [] string = False
func1 entry string = if(read_Int (head entry) == read_Int (string)) 
                          then True 
                          else func1 (tail entry) string

-- functie pentru In
func2 :: Entry -> Bool
func2 entry = undefined


-- imi va transforma un string intr-un numar pe care il folosesc in functiile func
read_Int :: String-> Integer
read_Int string = read string :: Integer

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

-- nr_column = functia care imi spune  pe a cata coloana e ce caut eu
nr_column :: TableSchema -> Field -> Integer -> Int
nr_column [] _ _ = 0
nr_column header field iterator = if(field == head header)
                                    then (fromInteger iterator)
                                    else nr_column (tail header) field (1 + iterator)

-- imi va intoarce header-ul table-ului
func_head :: Table -> TableSchema
func_head (Table header entries) = header

-- imi va intoarce entry-urile table-ului
func_entries :: Table -> [Entry]
func_entries (Table header entries) = entries

-- caut o anumita coloana in header
find_column :: Column -> Table -> Table
find_column column (Table [] _) = Table [] []
find_column column(Table header entries)=if (head header == column) 
                                      then Table (column:[]) (map (\x ->((head x):[])) entries) 
                                      else find_column column$Table(tail header)(map tail entries)

-- concatenez 2 tabele
append_tables :: Table -> Table -> Table
append_tables (Table [] _) (Table [] _) = Table [] []
append_tables (Table header1 entry1) (Table header2 entry2) = Table (header1 ++ header2) 
                                                            (zipWith (++) entry1 entry2)

-- caut elementul din entry de pe pozitia x
find_e :: Int -> Entry -> Field
find_e x entry = entry!!(x)


-- caut pozitia pe care se afla coloana data ca param
-- ma duc in tabel pe pozitia nr coloanei pentru a verifica daca 
-- coloana din entry este egala cu field ul meu

func_table_eq :: Table -> Field -> String -> [Entry]
func_table_eq (Table header entries) field string=filter(\x->(find_e(nr_column header field 0)x) 
                                                              == string) entries   

-- ma va ajuta sa filtrez entry-urile in functie de acele 
-- field-uri care au valorile < decat integer-ul meu
func_table_lt :: Table -> Field -> Integer -> [Entry]
func_table_lt (Table header entries) field integer = filter(\x -> (read_Int (find_e (nr_column 
                                                      header field 0) x)) < integer) entries


-- ma va ajuta sa filtrez entry-urile in functie de string-urile din lista
func_table_in :: Table -> Field -> [String] -> [Entry]
func_table_in (Table header []) field list = []
func_table_in (Table header entries) field list = filter (\x -> elem (find_e (nr_column 
                                                  header field 0) x) list) entries

-- neg conditia folosita pentru In
not_func_table_in :: Table -> Field -> [String] -> [Entry]
not_func_table_in (Table header []) field list = []
not_func_table_in (Table header entries) field list = filter (\x -> not (elem (find_e (nr_column 
                                                      header field 0) x) list)) entries

-- neg conditia pentru Lt
not_func_table_lt :: Table -> Field -> Integer -> [Entry]
not_func_table_lt (Table header entries) field integer = filter(\x->not((read_Int(find_e(nr_column 
                                                          header field 0) x)) < integer)) entries

-- neg conditia pentru Eq
not_func_table_eq :: Table -> Field -> String -> [Entry]
not_func_table_eq (Table header entries) field string = filter (\x -> not ((find_e (nr_column 
                                                        header field 0) x) == string)) entries   


eval :: Query -> Table


eval (Atom (Table header entries)) = Table header entries 

eval (Select [x] query) = Table [x] (func_entries (find_column x (eval query)))
eval (Select columns query) = append_tables (find_column (head columns) (eval query)) 
                                  (eval (Select (tail columns) query))

eval (SelectLimit columns integer query) = Table (func_head (eval(Select columns query))) 
                                          (take (fromInteger integer) 
                                          (func_entries (eval(Select columns query))))

eval (Filter (Lt field integer) query) = Table (func_head (eval query)) (func_table_lt 
                                          (eval query) field integer)

eval (Filter (Eq column field) query) = Table (func_head (eval query)) 
                                        (func_table_eq (eval query) column field)

eval (Filter (In field string) query) = Table (func_head (eval query)) (func_table_in 
                                          (eval query) field string)

eval (Filter (Not (Lt field integer)) query) = Table (func_head (eval query)) 
                                              (not_func_table_lt (eval query) 
                                                field integer)
eval (Filter (Not (Eq column field)) query) = Table (func_head (eval query)) 
                                              (not_func_table_eq (eval query) column field)
eval (Filter (Not (In field string)) query) = Table (func_head (eval query)) 
                                              (not_func_table_in (eval query) field string)

--realizez concatenarea entry-urilor, deoarece header-ul va ramane acelasi

eval (table :|| table1) = Table (func_head (eval table)) ((func_entries (eval table)) 
                          ++ (func_entries (eval table1)))

eval (Cosine query) = undefined

-- TODO 5

-- imi creez o functie auxiliara pentru a extrage zona in care se afla user-ul

zone_func :: String -> String
zone_func user_id = head $ head $ func_entries $ eval $ Select ["zone"] 
                    $ Filter (Eq "user_id" user_id) $ Atom user_info

-- exclud entry-ul user_id-ului primit ca parametru

same_zone :: String -> Query
same_zone user_id = Select ["user_id", "occupation"] $ Filter (Eq "zone" (zone_func user_id)) 
                    $ Filter (Not (Eq "user_id" user_id)) $ Atom user_info


male_within_age :: Integer -> Integer -> Query
male_within_age y x = Select ["occupation", "zone"] $ Filter (Eq "sex" "M") 
                      $ Filter (Not (Lt "age" (y + 1))) $ Filter (Lt "age" x) $ Atom user_info


mixed :: [String] -> [String] -> Integer -> Query
mixed list1 list2 x = Select ["user_id"] $ Filter (In "zone" list1) 
                      $ Filter (In "occupation" list2) $ Filter (Lt "age" x) $  Atom user_info
