{-# LANGUAGE GADTs #-}

module Chapter0709 where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Operational
import Data.List

-- 7.9.1

type Price = Int

type Amount = Int

type Product = String

type Report = [(Product, Amount)]

type ProductList = [(Product, Price)]

data SalesBase a where
  GetProductList :: SalesBase ProductList
  GetReport :: SalesBase Report
  Sell :: (Product, Amount) -> SalesBase ()

type SalesT m a = ProgramT SalesBase m a

type Sales a = Program SalesBase a

getProductList :: SalesT m ProductList
getProductList = singleton GetProductList

getReport :: SalesT m Report
getReport = singleton GetReport

sell :: (Product, Amount) -> SalesT m ()
sell priceAndAmount = singleton $ Sell priceAndAmount

sellFruits :: Sales ()
sellFruits = do
  sell ("Apple", 5)
  sell ("Grape", 8)
  sell ("Pineapple", 2)

-- 7.9.2

runSalesT :: Monad m => ProductList -> Report -> SalesT m a -> m a
runSalesT productList report =
  viewT >=> eval
  where
    eval (Return x) = pure x
    eval (GetProductList :>>= salesBase) = runSalesT productList report (salesBase productList)
    eval (GetReport :>>= salesBase) = runSalesT productList report (salesBase report)
    eval (Sell productAndAmount :>>= salesBase) = runSalesT productList (productAndAmount : report) (salesBase ())

runSales :: ProductList -> Report -> Sales a -> a
runSales productList report = runIdentity . runSalesT productList report

findAmount :: Product -> ProductList -> Maybe Price
findAmount product = fmap snd . find ((product ==) . fst)

summary :: Sales [(Product, Amount, Maybe Price)]
summary =
  getProductList >>= \productList -> map (sumRecord productList) <$> getReport
  where
    sumRecord productList (product, amount) = (product, amount, (* amount) <$> findAmount product productList)

productList :: ProductList
productList =
  [ ("Apple", 98),
    ("Grape", 398),
    ("Pineapple", 498)
  ]

-- >>> runSales productList [] (sellFruits >> summary)
-- [("Pineapple",2,Just 996),("Grape",8,Just 3184),("Apple",5,Just 490)]
