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

getJSON :: IO B.ByteString
getJSON = B.readFile "data.json"

filterByYear :: Int -> IO()
filterByYear y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print [x | x <- ps, (year (data_ x)) == y ]

filterByYearMonth :: Int -> Int -> IO()
filterByYearMonth y m= do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (filterByMonthYear m y ps)


getIncome  :: Int -> Int -> IO()
getIncome m y  = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (sum (getAllIncome m y (cleanData ps)))

getExpense  :: Int -> Int -> IO()
getExpense m y  = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (sum (getAllExpense m y (cleanData ps)))

getLeftOver :: Int -> Int -> IO()
getLeftOver m y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (sum (getAllIncome m y (cleanData ps)) - sum (getAllExpense m y (cleanData ps)) )
  

getMeanLeftOver :: Int -> IO()
getMeanLeftOver y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((sum (getAllIncomeByYear y (cleanData ps)) - sum (getAllExpenseByYear y (cleanData ps)) ) / (fromIntegral (Prelude.length  (getAllExpenseByYear y (cleanData ps)))))

getMaxBalance :: Int -> Int -> IO ()
getMaxBalance m y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (getMax (filterByMonthYear m y (cleanData ps)))

getMinBalance :: Int -> Int -> IO ()
getMinBalance m y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (getMin (filterByMonthYear m y (cleanData ps)))

getMeanIncomeByYear :: Int -> IO ()
getMeanIncomeByYear y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((sum  (getAllIncomeByYear y (cleanData ps))) / (fromIntegral (Prelude.length  (getAllIncomeByYear y (cleanData ps)))))

getMeanExpenseByYear :: Int -> IO ()
getMeanExpenseByYear y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print ((sum  (getAllExpenseByYear y (cleanData ps))) / (fromIntegral (Prelude.length  (getAllExpenseByYear y (cleanData ps)))))

getFlux :: Int -> Int -> IO ()
getFlux m y = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Transaction])
  case d of
    Left err -> putStrLn err
    Right ps -> print (flux (filterByMonthYear m y (cleanData ps)))

daysInMonth :: [Transaction] -> [Int]
daysInMonth dado = quickSort (Data.Foldable.foldl (\acc x -> if not (elem ( dayOfMonth (data_ x))  acc) then acc ++ [( dayOfMonth (data_ x)) ] else acc) [ ( dayOfMonth (data_ (Prelude.head dado)))] dado)

dailyBalance :: [Transaction] -> Int -> Float
dailyBalance dado dia = sum [(valor x) | x <- [x | x <- dado, ( dayOfMonth (data_ x)) == dia]] 

flux :: [Transaction] -> [(Int, Float)]
flux dado = Prelude.map (\x -> (x, dailyBalance dado x)) (daysInMonth dado)

filterByMonthYear :: Int -> Int -> [Transaction] -> [Transaction]
filterByMonthYear m y dado = [x | x <- dado, ( year (data_ x)) == y, ( month (data_ x)) == m ]

cleanData :: [Transaction] -> [Transaction]
cleanData dado = [x | x <- dado, not ("SALDO_CORRENTE" `elem` (tipos x)), not ("APLICACAO" `elem` (tipos x)), not ("VALOR_APLICACAO" `elem` (tipos x))  ]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
    where smaller = Prelude.filter (<=x) xs
          larger  = Prelude.filter (> x) xs

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

main :: IO ()
main = hspec $ do

  context "Projeto Funcional" $ do
  describe "filterByYear" $ do
  it "quantidade de transacoes no ano de 2017" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
        Left err -> putStrLn err
        Right ps -> Prelude.length [x | x <- ps, (year (data_ x)) == 2017 ]  `shouldBe` 1336
  it "quantidade de transacoes no ano de 2018" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
        Left err -> putStrLn err
        Right ps -> Prelude.length [x | x <- ps, (year (data_ x)) == 2018 ]  `shouldBe` 1146
  it "quantidade de transacoes no ano de 2019" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
        Left err -> putStrLn err
        Right ps -> Prelude.length [x | x <- ps, (year (data_ x)) == 2019 ]  `shouldBe` 1207

  describe "filterByYearMonth" $ do
  it "quantidade de transacoes no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> Prelude.length (filterByMonthYear 3 2018 ps) `shouldBe` 96
      
  it "quantidade de transacoes no ano de 2019 no mes 1" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> Prelude.length (filterByMonthYear 1 2019 ps) `shouldBe` 84

  it "quantidade de transacoes no ano de 2018 no mes 12" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> Prelude.length (filterByMonthYear 12 2018 ps) `shouldBe` 0
  
  describe "getIncome" $ do        
  it "Calcular o valor das receitas (créditos) no ano de 2018 no mes 12" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllIncome 12 2018 (cleanData ps))) `shouldBe` 0.0
  it "Calcular o valor das receitas (créditos) no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllIncome 3 2018 (cleanData ps))) `shouldBe` 107375.22

  describe "getExpense" $ do        
  it "Calcular o valor das despesas (débitos) ano de 2018 no mes 12" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllExpense 12 2018 (cleanData ps))) `shouldBe` 0.0
  it "Calcular o valor das despesas (débitos) ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllExpense 3 2018 (cleanData ps))) `shouldBe` -107956.09

  describe "calculaValorDasSobras" $ do        
  it "Calcular a sobra (receitas - despesas) no ano de 2018 no mes 12" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllIncome 12 2018 (cleanData ps)) - sum (getAllExpense 12 2018 (cleanData ps)) ) `shouldBe` 0.0

  it "Calcular a sobra (receitas - despesas) no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (sum (getAllIncome 3 2018 (cleanData ps)) - sum (getAllExpense 3 2018 (cleanData ps)) ) `shouldBe` 215331.31
    
  describe "calculaSaldo" $ do        
  it "Calcular o saldo final" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    pending
  
  describe "calculaSaldoMaximo" $ do        
  it "Calcular o saldo máximo atingido no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (getId (getMax (filterByMonthYear 3 2018 (cleanData ps)))) `shouldBe` "CR\201D.LIQ.COBRAN\199A"
    
  describe "calculaSaldoMinimo" $ do        
  it "Calcular o saldo minimo atingido no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (getId (getMin (filterByMonthYear 3 2018 (cleanData ps)))) `shouldBe` "CR\201D.LIQ.COBRAN\199A"
  
  describe "calculaMediaReceitas" $ do        
  it "Calcular a média das receitas no ano de 2018" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> ((sum  (getAllIncomeByYear 2018 (cleanData ps))) / (fromIntegral (Prelude.length  (getAllIncomeByYear 2018 (cleanData ps))))) `shouldBe` 5415.0576
    
  describe "calculaMediaDespesas" $ do        
  it "Calcular a média das despesas no ano de 2018" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> ((sum  (getAllExpenseByYear 2018 (cleanData ps))) / (fromIntegral (Prelude.length  (getAllExpenseByYear 2018 (cleanData ps))))) `shouldBe` -1428.7981
    
  describe "calculaMediaSobras" $ do        
  it "Calcular a média das sobras no ano de 2018" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> ((sum (getAllIncomeByYear 2018 (cleanData ps)) - sum (getAllExpenseByYear 2018 (cleanData ps)) ) / (fromIntegral (Prelude.length  (getAllExpenseByYear 2018 (cleanData ps))))) `shouldBe` 2957.3914
    
  describe "retornaFluxoDeCaixa" $ do        
  it "Retornar o flux de caixa de  no ano de 2018 no mes 3" $ do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Crud.Transaction])
    case d of
      Left err -> putStrLn err
      Right ps -> (flux (filterByMonthYear 3 2018 (cleanData ps))) `shouldBe` [(2,8.350048),(3,5962.22),(4,-789.92),(5,-23551.18),(6,-15569.9),(9,-1154.0),(10,-13446.461),(11,3908.8801),(12,1360.1399),(13,2607.11),(16,-6976.09),(17,2136.5999),(18,3174.2002),(19,-3577.6802),(20,55066.8),(23,1644.8301),(24,1905.19),(25,88.51997),(26,-14158.721),(27,475.88),(30,304.35)]
    
    



    

