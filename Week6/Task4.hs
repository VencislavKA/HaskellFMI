main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\(_,_,city) -> city == "Burgas") == 24.95 -- 24.950000000000003
    print $ getAverageBalance (accounts1, people1) (\(_,(n:_),_) -> n == 'P') == 18.85
    print $ getAverageBalance (accounts1, people1) (\ (n,_,_) -> n == 2) == 26.8

    -- print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    -- print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    -- print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62
    -- print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25
    -- print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25

--             id, id owner, balance 
type Account = (Int,Int,Double)

--              id, name, city 
type Person = (Int,String,String)

people1 :: [Person]
people1 = [(1, "Ivan", "Sofia"),(2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"),(4, "Petya", "Burgas")]

accounts1 :: [Account]
accounts1 = [(1, 1, 12.5),(2, 1, 123.2),(3, 2, 13.0),(4, 2, 50.2),(5, 2, 17.2),(6, 3, 18.3),(7, 4, 19.4)]

getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance database p = sum filteredBalance / (fromIntegral $ length filteredBalance)
 where
    accounts = fst database
    persons = snd database
    filteredPersonsId = map (\(id,_,_) -> id) $ filter p $ persons
    filteredBalance = map (\(_,_,balance) -> balance) $ filter (\(_,ownerId,_) -> elem ownerId filteredPersonsId) accounts