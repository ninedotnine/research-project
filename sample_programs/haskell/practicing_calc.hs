-- takes up to 7 integer arguments, computes practice time
import System.Environment (getArgs)

weekdays = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

makeDays :: [(String, Int)] -> String
makeDays [] = ""
makeDays (tup:xs) = day ++ ": " ++ hours ++ " hours\n" ++ makeDays xs
    where day = fst tup 
          hours = show $ convertToHour (snd tup)

-- convert minutes to hours 
convertToHour :: Int -> Double
convertToHour = (/ 60) . fromIntegral

readInt :: String -> Int
readInt = read

main :: IO ()
main = do 
    args <- getArgs
    
    let minutes = map readInt args
    -- let hours = map convertToHour minutes
    let total = foldl (+) 0 minutes
    let totalHours = convertToHour total
    let average = div total 7
    let averageHours = totalHours / 7

    putStrLn $ makeDays $ zip weekdays minutes

    putStrLn $ "total over 7 days: " ++ (show total) ++ " minutes"
    putStrLn $ "which is:          " ++ (show totalHours) ++ " hours"
    putStrLn $ "7-day average:     " ++ (show average) ++ " minutes"
    putStrLn $ "which is:          " ++ (show averageHours) ++ " hours"
