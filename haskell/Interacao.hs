module Interacao where
import Util
import Data.Array
import Data.Char
{--import Data.Map (Map, (!))--}
import qualified Data.Map as Map


{-Comando para dar scape no terminal-}

cleanScreen :: IO()
cleanScreen = putStr "\ESC[2J"

{-Imprime as strings de uma lista de string-}

showLines :: [String] -> IO()
showLines [] = return ()
showLines s = do 
	putStrLn (head s)
	showLines (tail s)

{-Exibe mensagem inicial do jogo-}

tela_principal :: IO ()
tela_principal = do
	cleanScreen
	cont <- readFile ".msg"
	showLines (take 23 (lines cont))

{-Mensagem de derrota-}

showMenu :: IO()
showMenu = do
	cont <- readFile ".msg"
	showLines (take 9 (drop 14 (lines cont)))


showDerrota :: IO()
showDerrota = do
	cont <- readFile ".msg"
	showLines (take 14 (drop 23 (lines cont)))
	
{-Mensagem de Vitoria-}
showVitoria :: IO()
showVitoria = do
	cont <- readFile ".msg"
	showLines (take 8 (drop 39 (lines cont)))


{-Retorna a string com a visão do usuário do
  tabuleiro-}
showTabuleiro :: Array (Int,Int) Int -> Int -> Int -> String
showTabuleiro m i j |(j == 1)&&(i < 10) = show(i) ++ " |" ++ (showPosicao (m ! (i,j))) ++ showTabuleiro m i (j+1)
					|(j == 1)&&(i >= 10)&&(i <= 12) = show(i)++"|" ++ (showPosicao (m ! (i,j))) ++ showTabuleiro m i (j+1)
					|(j > 1)&&(j < 12)&&(i <= 12) = "|" ++ (showPosicao (m ! (i,j))) ++ showTabuleiro m i (j+1) 
					|(j == 12)&&(i < 12) = "|" ++ (showPosicao (m ! (i,j))) ++"|" ++ "\n" ++ showTabuleiro m (i+1) 1
					| (j== 12)&&(i == 12) = "|"++(showPosicao (m ! (i,j)))++"|"

{--Retorna a string com a visão final do tabuleiro--}
showTabuleiroFinal :: Array (Int,Int) Int -> Int -> Int -> String
showTabuleiroFinal m i j |(j == 1)&&(i < 10) = show(i) ++ " |" ++ (showPosicaoFinal (m ! (i,j))) ++ showTabuleiroFinal m i (j+1)
					|(j == 1)&&(i >= 10)&&(i <= 12) = show(i)++"|" ++ (showPosicaoFinal (m ! (i,j))) ++ showTabuleiroFinal m i (j+1)
					|(j > 1)&&(j < 12)&&(i <= 12) = "|" ++ (showPosicaoFinal (m ! (i,j))) ++ showTabuleiroFinal m i (j+1) 
					|(j == 12)&&(i < 12) = "|" ++ (showPosicaoFinal (m ! (i,j))) ++"|" ++ "\n" ++ showTabuleiroFinal m (i+1) 1
					| (j== 12)&&(i == 12) = "|"++(showPosicaoFinal (m ! (i,j)))++"|"



numeroLinhaAux :: String -> Int
numeroLinhaAux linha | (linha == "1") = 1
				  | (linha == "2") = 2
				  | (linha == "3") = 3
				  | (linha == "4") = 4
				  | (linha == "5") = 5
				  | (linha == "6") = 6
				  | (linha == "7") = 7
				  | (linha == "8") = 8
				  | (linha == "9") = 9
				  | (linha == "10") = 10
				  | (linha == "11") = 11
				  | (linha == "12") = 12
				  | otherwise = -1


lowerString str = [ toLower loweredString | loweredString <- str]
numeroLinha :: String -> Int
numeroLinha linha = numeroLinhaAux (lowerString linha)
numeroColunaAux :: String -> Int
numeroColunaAux coluna | (coluna == "a") = 1
				  		| (coluna == "b") = 2
				  		| (coluna == "c") = 3
				  		| (coluna == "d") = 4
				  		| (coluna == "e") = 5
				  		| (coluna == "f") = 6
				  		| (coluna == "g") = 7
				  		| (coluna == "h") = 8
				  		| (coluna == "i") = 9
				  		| (coluna == "j") = 10
				  		| (coluna == "k") = 11
				  		| (coluna == "l") = 12
				  		| otherwise = -1

numeroColuna :: String -> Int
numeroColuna coluna = numeroColunaAux (lowerString coluna)

