{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Crud (Transaction(..), Date(..)) where

import Test.Hspec
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Foldable (Foldable, foldr, foldl, sum)

data Date = Date {year :: Int, month :: Int, dayOfMonth :: Int } deriving (Show,Generic)

data Transaction = Transaction { textoIdentificador :: Text, valor :: Float, descricao :: Text, numeroDOC :: Text,
                                 classificada :: Bool, data_ :: Date, tipos :: [String] } deriving (Show,Generic)
                 
instance FromJSON Date 
instance ToJSON Date
instance FromJSON Transaction
instance ToJSON Transaction

getId t = (textoIdentificador t)

flux :: [Transaction] -> [(Int, Float)]
flux dado = Prelude.map (\x -> (x, dailyBalance dado x)) (daysInMonth dado)

daysInMonth :: [Transaction] -> [Int]
daysInMonth dado = quickSort (Data.Foldable.foldl (\acc x -> if not (elem ( dayOfMonth (data_ x))  acc) then acc ++ [( dayOfMonth (data_ x)) ] else acc) [ ( dayOfMonth (data_ (Prelude.head dado)))] dado)

dailyBalance :: [Transaction] -> Int -> Float
dailyBalance dado dia = sum [(valor x) | x <- [x | x <- dado, ( dayOfMonth (data_ x)) == dia]] 

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
    where smaller = Prelude.filter (<=x) xs
          larger  = Prelude.filter (> x) xs
filterByMonthYear :: Int -> Int -> [Transaction] -> [Transaction]
filterByMonthYear m y dado = [x | x <- dado, ( year (data_ x)) == y, ( month (data_ x)) == m ]

cleanData :: [Transaction] -> [Transaction]
cleanData dado = [x | x <- dado, not ("SALDO_CORRENTE" `elem` (tipos x)), not ("APLICACAO" `elem` (tipos x)), not ("VALOR_APLICACAO" `elem` (tipos x))  ]

getMin :: [Transaction] -> Transaction
getMin dado = getMin' (Prelude.head dado) (Prelude.tail dado)

getMin' :: Transaction -> [Transaction] -> Transaction
getMin' minimum [] = minimum
getMin' minimum (x:xs)
  | (valor minimum) < (valor x) = getMin' x xs
  | otherwise = getMin' minimum xs

getMax :: [Transaction] -> Transaction
getMax dado = getMax' (Prelude.head dado) (Prelude.tail dado)

getMax' :: Transaction -> [Transaction] -> Transaction
getMax' maximum [] = maximum
getMax' maximum (x:xs)
  | (valor maximum) <= (valor x) = getMax' x xs
  | otherwise = getMax' maximum xs

getAllIncome :: Int -> Int -> [Transaction] -> [Float]
getAllIncome m y dado = [(valor x) | x <- dado, ( year (data_ x)) == y, ( month (data_ x)) == m, ( valor x) > 0 ]

getAllExpense :: Int -> Int -> [Transaction] -> [Float]
getAllExpense m y dado = [(valor x) | x <- dado, ( year (data_ x)) == y, ( month (data_ x)) == m, ( valor x) < 0 ]

getAllIncomeByYear y dado = [(valor x) | x <- dado, ( year (data_ x)) == y, ( valor x) > 0 ]
getAllExpenseByYear y dado =  [(valor x) | x <- dado, ( year (data_ x)) == y, ( valor x) < 0 ]

getJSON :: IO B.ByteString
getJSON = B.readFile "data.json"

filterByYear :: Int -> IO()
filterByYear y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (filterByYear_ d y)

filterByYear_ d y = [x | x <- d, (year (data_ x)) == y ]

filterByYearMonth :: Int -> Int -> IO()
filterByYearMonth y m= do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (filterByMonthYear m y d)


getIncome  :: Int -> Int -> IO()
getIncome m y  = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (sum (getAllIncome m y (cleanData d)))

getExpense  :: Int -> Int -> IO()
getExpense m y  = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (sum (getAllExpense m y (cleanData d)))

getLeftOver :: Int -> Int -> IO()
getLeftOver m y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (getLeftOver_ d m y)

getLeftOver_ d m y = sum (getAllIncome m y (cleanData d)) - sum (getAllExpense m y (cleanData d)) 
  

getMeanLeftOver :: Int -> IO()
getMeanLeftOver y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print ( getMeanLeftOver_ d y )

getMeanLeftOver_ d y = (sum (getAllIncomeByYear y (cleanData d)) - sum (getAllExpenseByYear y (cleanData d)) ) / (fromIntegral (Prelude.length  (getAllExpenseByYear y (cleanData d))))

getMaxBalance :: Int -> Int -> IO ()
getMaxBalance m y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (getMax (filterByMonthYear m y (cleanData d)))

getMinBalance :: Int -> Int -> IO ()
getMinBalance m y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (getMin (filterByMonthYear m y (cleanData d)))

getMeanIncomeByYear :: Int -> IO ()
getMeanIncomeByYear y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (getMeanIncomeByYear_ d y)

getMeanIncomeByYear_ d y = (sum  (getAllIncomeByYear y (cleanData d))) / (fromIntegral (Prelude.length  (getAllIncomeByYear y (cleanData d))))

getMeanExpenseByYear :: Int -> IO ()
getMeanExpenseByYear y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (getMeanExpenseByYear_ d y)

getMeanExpenseByYear_ d y = (sum  (getAllExpenseByYear y (cleanData d))) / (fromIntegral (Prelude.length  (getAllExpenseByYear y (cleanData d))))

getFlux :: Int -> Int -> IO ()
getFlux m y = do
  json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case json of
    Left err -> putStrLn err
    Right d -> print (flux (filterByMonthYear m y (cleanData d)))

main :: IO ()
main = hspec $ do

  context "Projeto Funcional" $ do

  it "transacoes em 2017 = 1336" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
        Left err -> putStrLn err
        Right d -> Prelude.length (filterByYear_ d 2017) `shouldBe` 1336
  it "transacoes em 2018 = 1146" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
        Left err -> putStrLn err
        Right d -> Prelude.length (filterByYear_ d 2018)  `shouldBe` 1146
  it "transacoes em 2019 = 1207" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
        Left err -> putStrLn err
        Right d -> Prelude.length (filterByYear_ d 2019)  `shouldBe` 1207

  it "transacoes em março de 2018 = 96" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> Prelude.length (filterByMonthYear 3 2018 d) `shouldBe` 96
      
  it "transacoes em janeiro de 2019 = 84" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> Prelude.length (filterByMonthYear 1 2019 d) `shouldBe` 84

  it "caso de erro: transacoes no mes 12 (inexistente) = 0" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> Prelude.length (filterByMonthYear 12 2018 d) `shouldBe` 0
  
  it "caso de erro: receitas no mes 12 (inexistente) = 0" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (sum (getAllIncome 12 2018 (cleanData d))) `shouldBe` 0.0

  it "receitas em janeiro de 2019 = 112735.66" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (sum (getAllIncome 1 2019 (cleanData d))) `shouldBe` 112735.66

  it "caso de erro: depesas no mes 12 (inexistente) = 0" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (sum (getAllExpense 12 2018 (cleanData d))) `shouldBe` 0.0

  it "despesas em janeiro de 2019 = -101597.95" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (sum (getAllExpense 1 2019 (cleanData d))) `shouldBe` -101597.95

  it "caso de erro: sobra no mes 12 (inexistente) = 0" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getLeftOver_ d 12 2018) `shouldBe` 0.0

  it "sobras em janeiro de 2019 = 214333.61" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getLeftOver_ d 1 2019) `shouldBe` 214333.61
  
  it "transacao com saldo maximo em janeiro de 2019 = -101597.95" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getId (getMax (filterByMonthYear 1 2019 (cleanData d)))) `shouldBe` "CR\201D.LIQ.COBRAN\199A"
    
  it "transacao com saldo minimo em janeiro de 2019 = -101597.95" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getId (getMin (filterByMonthYear 1 2019 (cleanData d)))) `shouldBe` "CR\201D.LIQ.COBRAN\199A"
  
  it "média das receitas em 2019 = 6655.4834" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getMeanIncomeByYear_ d 2019) `shouldBe` 6655.4834
    
  it "média das despesas em 2019 = -1183.2368" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (getMeanExpenseByYear_ d 2019) `shouldBe` -1183.2368
    
  it "média das sobras em 2019 = 2484.6636" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> ((sum (getAllIncomeByYear 2019 (cleanData d)) - sum (getAllExpenseByYear 2019 (cleanData d)) ) / (fromIntegral (Prelude.length  (getAllExpenseByYear 2019 (cleanData d))))) `shouldBe` 2484.6636
    
  it "fluxo de caixa em janeiro de 2019" $ do
    json <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
    case json of
      Left err -> putStrLn err
      Right d -> (flux (filterByMonthYear 1 2019 (cleanData d))) `shouldBe` [(1,-21178.709),(4,1321.61),(5,3167.62),(6,-27167.23),(7,-12537.34),(8,-2172.18),(11,-6449.2207),(13,398.0),(14,1404.72),(15,667.8699),(18,7543.6),(19,9770.52),(20,62162.03),(21,-559.15),(22,-43.599976),(25,-1549.31),(26,443.84),(27,-3232.3901),(28,-853.0)]
    
    



    

