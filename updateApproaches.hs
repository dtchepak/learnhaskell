import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.State

-- experimenting based on http://blog.jessitron.com/2014/04/left-to-right-top-to-bottom.html
type Customer = String
type Item = String
data Sale = Sale { customer :: Customer, items :: [Item], saleNum :: Maybe Int }
    deriving (Show, Eq)

empty :: Sale
empty = Sale "" [] Nothing

-- Compose functions Sale -> Sale
addCustomer :: Customer -> Sale -> Sale
addCustomer c s = s { customer = c }
addItems :: [Item] -> Sale -> Sale
addItems i s = s { items = i }
complete :: Int -> Sale -> Sale
complete i s = s { saleNum = Just i }

doSale :: Customer -> [Item] -> Int -> Sale -> Sale
doSale c i n = complete n . addItems i . addCustomer c
doSale' :: Customer -> [Item] -> Int -> Sale -> Sale
doSale' c i n = addCustomer c >>> addItems i >>> complete n
{-
ghci> (addCustomer "Clive" >>> addItems ["Tea"] >>> complete 123) empty
Sale {customer = "Clive", items = ["Tea"], saleNum = Just 123}
-}

doSale''' :: Customer -> [Item] -> Int -> Sale -> Sale
doSale''' c i n = 
    foldr (>>>) id [addCustomer c, addItems i, complete n]
-- Or use Endo: http://davesquared.net/2012/07/composition-via-scary-sounding-maths-terms.html

-- Keep track of state of Sale; thread to each
-- State = \s -> (s,a)
doSale'' :: Customer -> [Item] -> Int -> Sale -> Sale
doSale'' c i n = 
    execState $ do
      modify $ addCustomer c
      modify $ addItems i
      modify $ complete n

-- Lens and state
-- (can generate lenses for each field automatically if we like)
customerL = lens customer (\x c -> x { customer = c })
itemsL = lens items (\x i -> x { items = i })
saleNumL = sets (\u s -> s { saleNum = Just (u s) })

doSaleL :: Customer -> [Item] -> Int -> Sale -> Sale
doSaleL c i n =
    execState $ do
        customerL .= c
        itemsL .= i
        saleNumL .= n

