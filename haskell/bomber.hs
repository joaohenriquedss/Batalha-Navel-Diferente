{--import Data.Map (Map, (!))

import qualified Data.Map as Map--}

import Data.Array

import System.Random

import Util

import Interacao

{- --------------------------------------------

   Funções de interação com terminal

   ---------------------------------------------}

{- Representa um Jogador com o seu tabuleiro -}

data Jogador = Jogador {tabuleiro :: Array (Int,Int) Int, bombas :: Int, pontuacao :: Int} deriving(Show)





{-Metodo que cria a matriz base de inteiro com valor zero-}

criarMatrizZero :: Int -> Int -> Array (Int,Int) Int

criarMatrizZero x n = array ((1,1),(x,n)) [((i,j), 0)|i<-[1..x],j<-[1..n]]



{--verifica se existe base na vertical: receber a matriz m (12X12), a linha de alocacao i, o tamanho da base ext e a linha novamente para inserir --}

verificarVertical :: Array (Int,Int) Int -> Int -> Int -> Int -> Int -> Bool

verificarVertical m i j ext k | (j == 1)&&(i < (k+ext))&&(not((m ! (i,j)) == 0) || not((m ! (i,(j+1))) == 0)) = False

							  | (j > 1)&&(j < 12)&&(i < (k+ext))&&(not((m ! (i,j)) == 0) || not((m ! (i,(j+1))) == 0) || not((m ! (i,(j-1))) == 0)) = False

							  | (j == 12)&&(i < (k+ext))&&(not((m ! (i,j)) == 0) || not((m ! (i,(j-1))) == 0)) = False

							  | (i >= (k + ext)) = True 

							  | otherwise = True&&(verificarVertical m (i+1) j ext k)

{--verifica se existe base na horizontal: receber a matriz m (12X12), a linha de alocacao i, o tamanho da base ext e a coluna novamente para inserir --}

verificarHorizontal :: Array (Int,Int) Int -> Int -> Int -> Int -> Int -> Bool

verificarHorizontal m i j ext k | (i == 1)&&(j < (k + ext))&&(not((m ! (i,j)) == 0) || not((m ! (i+1,j)) == 0)) = False

								| (i > 1)&&(i < 12)&&(j < (k + ext))&&(not((m ! (i,j)) == 0) || not((m ! (i+1,j)) == 0) || not((m ! (i-1,j)) == 0)) = False

								| (i == 12)&&(i < (k + ext))&&(not((m ! (i,j)) == 0) || not((m ! (i-1,j)) == 0)) = False

								| (j >= (k + ext)) = True 

								| otherwise = True&&(verificarHorizontal m i (j+1) ext k)



{--Aloca CT com uma posicao--}

alocarCT :: Jogador -> IO Jogador

alocarCT j = do

	linha <- randomRIO (1,12::Int)

	coluna <- randomRIO (1,12::Int)

	if not(verificarVertical (tabuleiro j) linha coluna 1 linha) then

		alocarCT j

	else

		return (Jogador (atualizarTabuleiro (tabuleiro j)  linha coluna 1) (bombas j) (pontuacao j))





alocarIAPA :: Jogador -> IO Jogador

alocarIAPA j = do

	direcao <- randomRIO (1,2::Int)

	if (direcao == 1) then do

		linha <- randomRIO (1,11::Int)

		coluna <- randomRIO (1,12::Int)

		if (verificarVertical (tabuleiro j) linha coluna 2 linha) then do

			return (Jogador (alocarBaseVertical (tabuleiro j) linha coluna 2 2 0) (bombas j) (pontuacao j))

		else

			alocarIAPA j

	else do

		linha <- randomRIO (1,12::Int)

		coluna <- randomRIO (1,11::Int)

		if (verificarHorizontal (tabuleiro j) linha coluna 2 coluna) then do

			return (Jogador (alocarBaseHorizontal (tabuleiro j) linha coluna 2 2 0) (bombas j) (pontuacao j))

		else

			alocarIAPA j



alocarBMT :: Jogador -> IO Jogador

alocarBMT j = do

	direcao <- randomRIO (1,2::Int)

	if (direcao == 1) then do

		linha <- randomRIO (1,10::Int)

		coluna <- randomRIO (1,12::Int)

		if (verificarVertical (tabuleiro j) linha coluna 3 linha) then do

			return (Jogador (alocarBaseVertical (tabuleiro j) linha coluna 3 3 0) (bombas j) (pontuacao j))

		else

			alocarBMT j

	else do

		linha <- randomRIO (1,12::Int)

		coluna <- randomRIO (1,10::Int)

		if (verificarHorizontal (tabuleiro j) linha coluna 3 coluna) then do

			return (Jogador (alocarBaseHorizontal (tabuleiro j) linha coluna 3 3 0) (bombas j) (pontuacao j))

		else

			alocarBMT j



alocarBPC :: Jogador -> IO Jogador

alocarBPC j = do

	direcao <- randomRIO (1,2::Int)

	if (direcao == 1) then do

		linha <- randomRIO (1,9::Int)

		coluna <- randomRIO (1,12::Int)

		if (verificarVertical (tabuleiro j) linha coluna 4 linha) then do

			return (Jogador (alocarBaseVertical (tabuleiro j) linha coluna 4 4 0) (bombas j) (pontuacao j))

		else

			alocarBPC j

	else do

		linha <- randomRIO (1,12::Int)

		coluna <- randomRIO (1,9::Int)

		if (verificarHorizontal (tabuleiro j) linha coluna 3 coluna) then do

			return (Jogador (alocarBaseHorizontal (tabuleiro j) linha coluna 4 4 0) (bombas j) (pontuacao j))

		else

			alocarBPC j



{--Metodo responsavel por alocar todas as Bases e returna o objeto jogador com todas as bases alocadas--}

alocarBases :: Jogador -> IO Jogador

alocarBases j = do
	ct1 <- alocarCT j
	ct2 <- alocarCT ct1
	ct3 <- alocarCT ct2
	ct4 <- alocarCT ct3
	ct5 <- alocarBPC ct4
	ct6 <- alocarBMT ct5
	ct7 <- alocarBMT ct6
	ct8 <- alocarIAPA ct7
	ct9 <- alocarIAPA ct8
	return (ct9)



{-Metodo que cria a objeto jogador com tabuleiro 12X12-}

criarJogador :: Int -> Int -> Jogador
criarJogador nBombas pont = Jogador (criarMatrizZero 12 12) nBombas pont




{-Variavel que representar o jogado com tabuleiro vazio (apenas zero) e com 45 bombas para se jogar-}

jogadorBase = criarJogador 45 0



{--alocar base na vertical de tamanho maior que 1--}

alocarBaseVertical ::  Array (Int,Int) Int -> Int -> Int -> Int -> Int -> Int -> Array (Int,Int) Int

alocarBaseVertical m linha coluna valor tamanho count | (not(tamanho == count)) = alocarBaseVertical (atualizarTabuleiro m linha coluna valor) (linha + 1) coluna valor tamanho (count+1)

													  | otherwise = m



{--alocar base na horizontal de tamanho maior que 1--}

alocarBaseHorizontal :: Array (Int,Int) Int -> Int -> Int -> Int -> Int -> Int -> Array (Int,Int) Int

alocarBaseHorizontal m linha coluna valor tamanho count | (not(tamanho == count)) = alocarBaseHorizontal (atualizarTabuleiro m linha coluna valor) linha (coluna + 1) valor tamanho (count+1)

													    | otherwise = m



{-atualiza o valor no tabuleiro do jogador (atualizacao feita a partir de uma soma), informando o numero da linha e coluna-}

atualizarTabuleiro :: Array (Int,Int) Int -> Int -> Int -> Int -> Array (Int,Int) Int

atualizarTabuleiro m linha coluna valor = accum (+) m [((linha,coluna), valor)]



{-Gera um numero Random mas a saida é um Inteiro IO-}

numeroRan :: Int -> Int -> IO Int

numeroRan v1 v2 = do

	nR <- randomRIO(v1, v2::Int)

	return (nR)





jogador1 = criarJogador 12 12



{--metodo que printa o tabuleiro final--}

mostrarFinal :: Jogador -> IO()

mostrarFinal j = do



	putStrLn (showTabuleiroFinal (tabuleiro j) 1 1)



{--metodo que printa o tabuleiro ao jogador--}

mostrarJogo :: Jogador -> IO()

mostrarJogo j = do

	putStrLn (showTabuleiro (tabuleiro j) 1 1)	





	

{- Main -}    

main :: IO()


main = do

    tela_principal

    putStrLn "Bem Vindo ao ano começo do fim , a gasolina está acabando e o mundo também só você poderá salva-lo e deter o virus mortal"

    putStrLn "Baseado em uma historia real. Se voce nao lembra foi por que o mundo ja foi salvo e sua linha do tempo mudou"

    putStrLn "[1] Pronto para a simulação :"

    

    a <- getLine
   
    if(a == "1") then do
    	cleanScreen
    	jogadorTabela <- alocarBases (criarJogador 45 0)
    	interacaoJogo (criarJogador 45 0) jogadorTabela
    	
        else do 
        	cleanScreen
        	showDerrota
    

interacaoJogo :: Jogador -> Jogador -> IO()
interacaoJogo jaux jtabuleiro = do
	
	if(((pontuacao jaux) < 18) && ((bombas jaux) > 0 )) then do

		
		putStrLn "   A B C D E F G H I J K L"
		mostrarJogo jaux
		showMenu

		putStrLn ("Numeros de Bombas:"++show(bombas jaux))
		putStrLn ("pontuacao do Jogador:"++show(pontuacao jaux))
		
		putStrLn "Digite o Numero da Linha:"
		linhaString <- getLine
		putStrLn "Digite a Letra da Coluna:"
		colunaString <- getLine
	
	
		let linhaInt = numeroLinha linhaString
		let colunaInt = numeroColuna colunaString
	
		if ((not(linhaInt == -1))&&(not(colunaInt == -1))) then

			if(not(((tabuleiro jtabuleiro) ! (linhaInt, colunaInt)) == 0)) then do

				if((((tabuleiro jaux) ! (linhaInt, colunaInt)) == 0)) then do

					let valor = ((tabuleiro jtabuleiro) ! (linhaInt, colunaInt))
					let jogadorAux = Jogador (atualizarTabuleiro (tabuleiro jaux) linhaInt colunaInt valor) ((bombas jaux)-1) ((pontuacao jaux)+1)
					cleanScreen
					interacaoJogo jogadorAux jtabuleiro

				else do
					cleanScreen
					interacaoJogo jaux jtabuleiro
			else do 
				if((((tabuleiro jaux) ! (linhaInt, colunaInt)) == 0)) then do
					cleanScreen
					interacaoJogo (Jogador (atualizarTabuleiro (tabuleiro jaux) linhaInt colunaInt (-1)) ((bombas jaux)-1) (pontuacao jaux)) jtabuleiro
				else do
					cleanScreen
					interacaoJogo (Jogador (tabuleiro jaux) (bombas jaux) (pontuacao jaux)) jtabuleiro

		else do
			cleanScreen
			putStrLn "Linha ou coluna Invalida:"
			interacaoJogo jaux jtabuleiro
	else do 
		if((pontuacao jaux) >= 18) then do
			cleanScreen
			mostrarFinal jtabuleiro
			showVitoria
		else do
			cleanScreen
			mostrarFinal jtabuleiro
			showDerrota


   

    
