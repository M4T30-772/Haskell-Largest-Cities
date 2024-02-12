{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Data.List (filter, nub)
import Data.Foldable (null)
import Data.Text (Text, pack, unpack, strip)
import Text.HTML.Scalpel

import System.Exit (exitSuccess)

import Data.List (sortOn)

data City = City
  { name       :: Text
  , country    :: Text
  , population :: Text
  , area       :: Text
  } deriving (Show, Eq)

allCities :: IO (Maybe [City])
allCities = scrapeURL "https://en.wikipedia.org/wiki/List_of_largest_cities" table
  where
    table :: Scraper Text [City]
    table = chroot ("table" @: [hasClass "static-row-numbers"]) cities

    cities :: Scraper Text [City]
    cities = chroots "tr" city

    city :: Scraper Text City
    city = do
      name <- text "th"
      rows <- texts "td"
      let country = getCountry rows
      let population = getPopulation rows
      let area = getArea rows 
      return $ City (strip name) country population area

    getCountry (x:_) = strip x
    getCountry _     = "Not available"

    getPopulation (x : y : _) = strip y
    getPopulation _          = "Not available"

    getArea (_ : _ : z : _) = strip z
    getArea _              = "Not available"

readPopulation :: String -> Int
readPopulation str = case reads (filter (/= ',') str) of
  [(x, "")] -> x
  _         -> 0

displayMenu :: IO ()
displayMenu = do

  putStrLn "Choose an option:"

  putStrLn "1. Display the list of cities"
  putStrLn "2. Display city and its population for a specific country"

  putStrLn "3. Display total number of cities and total population for a specific country"
 
  putStrLn "4. Display total amount of cities and countries that the program scraped"
 
  putStrLn "5. Save the scraped data"
  
  putStrLn "6. Sort the cities from lowest to highest or limit the size of cities you are looking for"
 
  putStrLn "7. Exit"

main :: IO ()

main = do
  displayMenu
  loop

loop :: IO ()

loop = do
  option <- getLine
  case option of
    "1" -> do
      result <- allCities
      case result of
        Just x  -> print x
        Nothing -> putStrLn "Didn't find the necessary items."
      displayMenu
      loop

    "2" -> do
      putStrLn "Enter the name of the country:"
      countryName <- getLine
      result <- allCities
      case result of
        Just cities -> do
          let citiesForCountry = filter (\city -> country city == pack countryName) cities
          if null citiesForCountry
            then putStrLn $ "No information found for " ++ countryName
            else mapM_ (\city -> putStrLn $ "City: " ++ unpack (name city) ++ ", Population: " ++ unpack (population city) ++ ", Area: " ++ unpack (area city)) citiesForCountry
          displayMenu
          loop
        Nothing -> putStrLn "Didn't find the necessary items."

    "3" -> do
      putStrLn "Enter the name of the country:"
      countryName <- getLine
      result <- allCities
      case result of
        Just cities -> do
          let citiesForCountry = filter (\city -> country city == pack countryName) cities
          if null citiesForCountry
            then putStrLn $ "No information found for " ++ countryName
            else do
              let totalCities = length citiesForCountry
                  totalPopulation = sum $ map (\city -> readPopulation (unpack (population city))) citiesForCountry
              putStrLn $ "Total number of cities for " ++ countryName ++ ": " ++ show totalCities
              putStrLn $ "Total population for " ++ countryName ++ ": " ++ show totalPopulation
              displayMenu
              loop
        Nothing -> putStrLn "Didn't find the necessary items."

    "4" -> do
      result <- allCities
      case result of
        Just cities -> do
          let totalCities = length cities
              totalCountries = length $ nub $ map country cities
          putStrLn $ "Total numbr of cities: " ++ show totalCities
          putStrLn $ "Total number of countries: " ++ show totalCountries
          displayMenu
          loop
        Nothing -> putStrLn "Didn't find the necessary items."

    "5" -> do
      result <- allCities
      case result of
        Just cities -> do
          writeFile "cities.txt" (unlines (map show cities))
          putStrLn "Cities data saved to 'cities.txt'"
        Nothing -> putStrLn "Didn't find the necessary items."
      displayMenu
      loop

    "6" -> do
      putStrLn "Choose sorting/filtering option:"
      putStrLn "a. Sort by population (lowest to highest )"
      putStrLn "b. Filter by population above a certain threshold"
      subOption <- getLine
      case subOption of
        "a" -> do
          result <- allCities
          case result of
            Just cities -> do
              let sortedCities = sortOn (\city -> readPopulation (unpack (population city))) cities
              mapM_ print sortedCities
            Nothing -> putStrLn "Didn't find the necessary items."
        "b" -> do
          putStrLn "Enter population threshold:"
          threshold <- getLine
          result <- allCities
          case result of
            Just cities -> do
              let filteredCities = filter (\city -> readPopulation (unpack (population city)) > read threshold) cities
              mapM_ print filteredCities
            Nothing -> putStrLn "Didn't find the necessary items."
        _ -> putStrLn "Invalid option. Please choose a valid option."
      displayMenu
      loop

    "7" -> do
      putStrLn "Exiting..."
      exitSuccess

    _   -> do
      putStrLn "Invalid option. Please choose a valid option."
      displayMenu
      loop
