--
-- Created to practise generating pseudo-random numbers in my head
-- according to the Marsaglia algorithm[1].
--
-- [1]: https://groups.google.com/forum/#!msg/sci.math/6BIYd0cafQo/Ucipn_5T_TMJ
--
-- Copyright (c) 2016, Chris
-- 
-- Permission to use, copy, modify, and/or distribute this software
-- for any purpose with or without fee is hereby granted, provided
-- that the above copyright notice and this permission notice appear
-- in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
-- WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED 
-- WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
-- AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
-- OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
-- TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
-- PERFORMANCE OF THIS SOFTWARE.
--


{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
import Protolude
import Prelude (getLine, String)
import System.Random (randomRIO)
import System.IO (hFlush, stdout)


main = do
    (tens, ones) <- (,) <$> randomRIO (1,9) <*> randomRIO (1,9)
    printf "The seed is %d. Let's go!\n" (tens*10 + ones)
    printf "Keep entering numbers from the sequence:\n"
    takeGuess tens ones


takeGuess :: Int -> Int -> IO ()
takeGuess tens ones = do
    guess <- readMaybe <$> getLine
    if guess == Just ones then
        let next = tens + 6 * ones in
            takeGuess (div next 10) (mod next 10)
    else
        printf "---- FAIL ----\n"


{-
 -
 - SIMPLE TIMES TABLE PLUS ADD TRAINER BELOW
 -
 -
data QA = QA { question :: String, answer :: Int }

questions (factor:term:rest) = QA
    { question = printf "6×%d + %d = " factor term
    , answer = 6*factor + term
    } : questions rest


main = do
    numbers <- randomRs (1,9) <$> getStdGen
    forM_ (questions numbers) $ \qa -> do
        putStr (question qa)
        guess <- readMaybe <$> getLine
        when (guess /= Just (answer qa)) $
            putStrLn ("---- FAIL ----" :: Text)
 -
 -
 -
 -}
