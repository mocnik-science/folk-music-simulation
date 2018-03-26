{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chorale.Common
import Chorale.Geo.Coordinates
import Control.Monad
import Data.Array
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Safe
import System.Random
import System.Random.Shuffle

repeatEndo :: Monad m => Int -> (a -> m a) -> a -> m a
repeatEndo 0 _ a = return a
repeatEndo n f a = repeatEndo (n - 1) f =<< f a

-- --== Setting

data State = State {
    years :: Double,
    musicians :: [Musician],
    _randomSeed :: Int,
    _simulationYears :: Int,
    _numberOfMusicians :: Int,
    _maximalCoordinateX :: Double,
    _maximalCoordinateY :: Double,
    _maximalCreativity :: Double,
    _maximalSocialness :: Double,
    _maximalOrganizer :: Double,
    _maximalRadiusOfEvent :: Double,
    _numberOfTunesAtEvent :: Int,
    _lengthOfTune :: Int,
    _maximalReuseForInventedTune :: Int,
    _maximalNumberOfErrorsWhenLearning :: Int,
    _thresholdTuneSimilarity :: Int,
    _thresholdLikeSimilarity :: Maybe Double,
    _percentageLikeSimilarTune :: Double,
    _percentageLikeDissimilarTune :: Double
}

instance Show State where
    show = unlines . map show . musicians

data Musician = Musician {
    idMusician :: Int,
    age :: Double,
    location :: CoordinatesCartesian,
    creativity :: Double,
    socialness :: Double,
    organizer :: Double,
    radiusOfEvent :: Double,
    knowsTunes :: [Tune],
    likesTunes :: [Tune],
    neighbouringMusicianIDs :: [Int]
} deriving (Eq, Ord)

instance Show Musician where
    show m = unlines ["====== Musician", show . idMusician $ m, show . age $ m, show . location $ m, show . creativity $ m, show . socialness $ m, show . organizer $ m, show . radiusOfEvent $ m, "", unlines . map show . knowsTunes $ m, unlines . map show . likesTunes $ m]

data Tune = Tune {
    music :: String,
    createdAtYear :: Double
} deriving (Show, Eq, Ord)

-- --== Create

createMusicians :: State -> IO [Musician]
createMusicians s = do
    ms <- mapM (createMusician s) [0.._numberOfMusicians s]
    return . map (\m -> m {neighbouringMusicianIDs = map idMusician . filter ((<= radiusOfEvent m) . distance (location m) . location) $ ms}) $ ms

createMusician :: State -> Int -> IO Musician
createMusician s i = do
    x <- randomRIO (0, _maximalCoordinateX s)
    y <- randomRIO (0, _maximalCoordinateY s)
    c <- randomRIO (0, _maximalCreativity s)
    so <- randomRIO (0, _maximalSocialness s)
    o <- randomRIO (0, _maximalOrganizer s)
    roe <- randomRIO (0, _maximalRadiusOfEvent s)
    return Musician {
        idMusician = i,
        age = 0,
        location = CoordinatesCartesian (x, y),
        creativity = c,
        socialness = so,
        organizer = o,
        radiusOfEvent = roe,
        knowsTunes = [],
        likesTunes = []
    }

-- --== Helping functions

mapMusicians :: (Musician -> Musician) -> State -> State
mapMusicians f s = s {musicians = map f . musicians $ s}

-- --== Social and musical behaviour

inventTune :: State -> Musician -> IO Musician
inventTune s m = do
    mu <- if null . likesTunes $ m
        then randomWord . _lengthOfTune $ s
        else do
            t' <- shuffleM . music =<< (chooseIO . likesTunes $ m)
            r <- randomRIO (0, _maximalReuseForInventedTune s)
            (take r t' ++) . drop r <$> (randomWord . _lengthOfTune $ s)
    let t = Tune {
        music = mu,
        createdAtYear = years s
    }
    return m {knowsTunes = knowsTunes m ++ [t], likesTunes = likesTunes m ++ [t]}

tuneIsSimilar :: State -> Tune -> Tune -> Bool
tuneIsSimilar s = (<= _thresholdTuneSimilarity s) .* tuneSimilarity

tuneSimilarity :: Tune -> Tune -> Int
tuneSimilarity = curry $ uncurry levenshtein . map12 music

likesTune :: State -> Musician -> Tune -> IO Bool
likesTune s m t =
    if isJust . _thresholdLikeSimilarity $ s
    then
        if uncurry (||) . map21 (null, (<= (fromJust . _thresholdLikeSimilarity) s) . fromIntegral . minimum . map (tuneSimilarity t)) . likesTunes $ m
        then (<= _percentageLikeSimilarTune s) <$> randomIO
        else (<= _percentageLikeDissimilarTune s) <$> randomIO
    else return True

learnTune :: State -> Tune -> Musician -> IO Musician
learnTune s t m = do
    e <- randomRIO (0, _maximalNumberOfErrorsWhenLearning s)
    mu <- randomizeCharacters e . music $ t
    let t' = t {music = mu}
    if any (tuneIsSimilar s t') . knowsTunes $ m
    then return m
    else do
        l <- likesTune s m t'
        return m {knowsTunes = nubOrd $ knowsTunes m ++ [t'], likesTunes = nubOrd $ likesTunes m ++ [t' | l]}

-- --== Simulation

simulate :: State -> IO State
simulate s = do
    setStdGen . mkStdGen . _randomSeed $ s
    ms <- createMusicians s
    repeatEndo (_simulationYears s) evolve $ s {musicians = ms}

state :: State
state = State {years = 0}

evolve :: State -> IO State
evolve s = do
    s <- inventNewTunes s
    s <- organizeEvents s
    return . mapMusicians (\m -> m {age = age m + 1}) $ s {years = years s + 1}

inventNewTunes :: State -> IO State
inventNewTunes s = do
    ms <- mapM inventTunesForMusician . musicians $ s
    return s {musicians = ms}
    where
        inventTunesForMusician m = do
            n <- round <$> randomRIO (0, creativity m)
            repeatEndo n (inventTune s) m

organizeEvents :: State -> IO State
organizeEvents s = do
    ms <- fmap fst . (flip $ repeatEndo (length . musicians $ s)) (musicians s, 0) $ \(ms, i) -> do
        o <- (<= organizer (ms!!i)) <$> randomIO
        if o
        then do
            r <- randomRIO (0, radiusOfEvent (ms!!i))
            ss <- mapM (\m -> (<= socialness m) <$> randomIO) ms
            let participation = filter (\j -> (ss!!j) || i == j) . map fst . filter ((<= r) . distance (location $ ms!!i) . location . snd) . filter ((`elem` neighbouringMusicianIDs (ms!!i)) . idMusician . snd) . zip [0..] $ ms
            if length participation <= 1
            then return (ms, i + 1)
            else do
                tunesPlayed <- fmap (take . _numberOfTunesAtEvent $ s) . shuffleM . concatMap (likesTunes . (ms!!)) $ participation
                let learnTunes ts (j, m) = if (j `elem` participation) || j == i
                    then foldM (flip $ learnTune s) m tunesPlayed
                    else return m
                ms <- mapM (learnTunes tunesPlayed) . zip [0..] $ ms
                return (ms, i + 1)
        else return (ms, i + 1)
    return s {musicians = ms}

-- --== Common Helping Functions

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein xs ys = levMemo ! (n, m) where
    n = length xs
    m = length ys
    levMemo = array ((0, 0), (n, m)) [((i, j), lev i j) | i <- [0..n], j <- [0..m]]
    lev 0 v = v
    lev u 0 = u
    lev u v
        | listArray (1, n) xs ! u == listArray (1, m) ys ! v = levMemo ! (u - 1, v - 1)
        | otherwise = 1 + minimum [levMemo ! (u, v - 1), levMemo ! (u - 1, v), levMemo ! (u - 1, v - 1)]

randomWord :: Int -> IO String
randomWord n = replicateM n randomChar

randomChar :: IO Char
randomChar = randomRIO ('a', 'z')

randomizeCharacters :: Int -> String -> IO String
randomizeCharacters n s = foldM (flip . const $ randomizeCharacter) s [0..n-1]

randomizeCharacter :: String -> IO String
randomizeCharacter "" = return ""
randomizeCharacter s = do
    c <- randomChar
    i <- randomRIO (0, length s - 1)
    return . intercalate [c] . tupleToList2 . mapSnd (drop 1) . splitAt i $ s

chooseIO :: [a] -> IO a
chooseIO [] = error "nothing to choose"
chooseIO as = do
    i <- randomRIO (0, length as - 1)
    return $ as !! i

-- --== Analysis

analysisExampleTuneSimilarity :: (Musician -> [Tune]) -> (Tune -> Bool) -> State -> IO [(Int, Double, Double, Maybe Int)]
analysisExampleTuneSimilarity m2t f s = do
    t <- chooseIO . filter f . concatMap m2t . musicians $ s
    return . map (\m -> (idMusician m, fst . toTuple . location $ m, snd . toTuple . location $ m, minimumMay . map (tuneSimilarity t) . m2t $ m)) . musicians $ s

analysisExampleTuneSimilarityCSV :: String -> (Musician -> [Tune]) -> (Tune -> Bool) -> State -> IO ()
analysisExampleTuneSimilarityCSV filename m2t f s = B.writeFile filename . B.append "id,x,y,similarity\n" . B.filter (/= BS.c2w '\r') . encode . sortBy (comparing fth4) . map (map44 (id, id, id, fromMaybe . _lengthOfTune $ s)) =<< analysisExampleTuneSimilarity m2t f s

-- --== Run the simulation

main :: IO ()
main = do
    let s' = state {
        _randomSeed = 0,
        _numberOfMusicians = 40,
        _maximalCoordinateX = 10,
        _maximalCoordinateY = 10,
        _maximalCreativity = 1.05,
        _maximalSocialness = 1.2,
        _maximalOrganizer = 0.1,
        _maximalRadiusOfEvent = 6,
        _numberOfTunesAtEvent = 5,
        _lengthOfTune = 16,
        _maximalReuseForInventedTune = 8,
        _maximalNumberOfErrorsWhenLearning = 2,
        _thresholdTuneSimilarity = 3,
        _percentageLikeSimilarTune = 0.9,
        _percentageLikeDissimilarTune = 0.1
    }
    
    s <- simulate $ s' {
        _simulationYears = 2,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-2-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 6,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-6-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 10,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-10-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 25,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-25-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 50,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-50-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 75,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-75-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 100,
        _thresholdLikeSimilarity = Nothing
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years-1.csv" knowsTunes ((== "ukqgoqlvtohskefw") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years-2.csv" knowsTunes ((== "zdomzgajdryxihcc") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years-3.csv" knowsTunes ((== "dvffctgjgdszcigf") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years-4.csv" knowsTunes ((== "ammokdfkyhlbdyal") . music) s
    
    s <- simulate $ s' {
        _simulationYears = 2,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-2-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 6,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-6-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 10,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-10-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 25,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-25-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 50,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-50-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 75,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-75-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    s <- simulate $ s' {
        _simulationYears = 100,
        _thresholdLikeSimilarity = Just 4
    }
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years4-0.csv" knowsTunes ((== "iaohjujjrxgauhyi") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years4-1.csv" knowsTunes ((== "ukqgoqlvtohskefw") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years4-2.csv" knowsTunes ((== "zdomzgajdryxihcc") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years4-3.csv" knowsTunes ((== "dvffctgjgdszcigf") . music) s
    analysisExampleTuneSimilarityCSV "../paper/data/simulation-100-years4-4.csv" knowsTunes ((== "ammokdfkyhlbdyal") . music) s
