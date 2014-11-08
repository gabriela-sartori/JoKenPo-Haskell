module Main where

import System.Random(randomRIO)
import Data.Char(digitToInt)

data Jogada = Pedra | Papel | Tesoura | Exit deriving (Eq, Ord, Enum, Read, Show, Bounded)
data Status = Win | Lose | Draw deriving (Eq, Ord, Enum, Read, Show, Bounded)
type Vitorias = Int
type Perdas   = Int
type Empates  = Int
data Info     = Info {vitorias :: Vitorias, perdas :: Perdas, empates :: Empates} deriving (Eq, Ord, Show)

{- Verifica se é uma jogada válida, caso contrário retorna Exit, pra terminar o programa -}
isJogadaValid :: Jogada -> Bool
isJogadaValid jogada = jogada /= Exit

{- Recebe um id do menu e retorna um tipo -}
getJogadaById :: Int -> Jogada
getJogadaById jogada = case jogada of
                           1 -> Pedra
                           2 -> Papel
                           3 -> Tesoura
                           _ -> Exit

{- Define Vitória/Derrota/Empate nas combinações entre Pedra/Papel/Tesoura -}
getRoundStatus :: Jogada -> Jogada -> Status
getRoundStatus Pedra     Papel = Lose
getRoundStatus Papel   Tesoura = Lose
getRoundStatus Tesoura   Pedra = Lose
getRoundStatus Pedra   Tesoura = Win
getRoundStatus Tesoura   Papel = Win
getRoundStatus Papel     Pedra = Win
getRoundStatus _ _ = Draw

{- Verifica se o primeiro caractere de uma string é um dígito, e o retorna caso seja -}
getValidInput :: String -> Maybe Int
getValidInput    [] = Nothing
getValidInput (s:_) = if s `elem` ['0'..'9'] then Just (digitToInt s) else Nothing

{- Cada jogo -}
mainWithInfo :: Info -> IO ()
mainWithInfo info = do
    putStrLn $ "+----------------+\n"
            ++ "|    JokenPo     |\n"
            ++ "+----------------+\n"
            ++ "| V: " ++ show (vitorias info) ++ " L: " ++ show (perdas info) ++ " E: " ++ show (empates info) ++ " |\n"
            ++ "+----------------+\n"
            ++ "| 1: Pedra       |\n"
            ++ "| 2: Papel       |\n"
            ++ "| 3: Tesoura     |\n"
            ++ "| _: Exit        |\n"
            ++ "+----------------+\n"
    raw_escolha <- getLine
    let escolha = read raw_escolha :: Int
    let jogada  = getJogadaById escolha
    if not $ isJogadaValid jogada then
        putStrLn "Até mais! vlw flw\nFim."
    else do
        random <- randomRIO (1, 3)
        let jogada_inimigo = getJogadaById random
        putStrLn $ "Você escolheu: " ++ show jogada
        putStrLn $ "O inimigo escolheu:  " ++ show jogada_inimigo
        case getRoundStatus jogada jogada_inimigo of 
            Win -> do
                putStrLn "Você venceu, aeHOOOOOO"
                mainWithInfo (Info (vitorias info + 1) (perdas info) (empates info))
            Lose -> do
                putStrLn "Você perdeu, perdedor hahahhahaha"
                mainWithInfo (Info (vitorias info) (perdas info + 1) (empates info))
            Draw -> do
                putStrLn "Empatou, que droga! rss"
                mainWithInfo (Info (vitorias info) (perdas info) (empates info + 1))

{- Main, programa começa aqui -}
main :: IO ()
main = mainWithInfo (Info 0 0 0)
