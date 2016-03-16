{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

module EABestatica where

-- Se importa el modulo de entrada y salida asi como los analizadores lexico y sintactico
import System.IO
import LexerEAB
import ParserEAB

-- Un tipo de datos para los tipos de EAB
data Tipo = TBol | TNat deriving (Show,Eq) 

-- Contextos como sinonimo de una lista de identificadores
type Ctx = [(Ident,Tipo)]


--Implementacion de la semantica estatica (Juicios para tipos)
vt :: Ctx -> Asa -> Tipo 
vt [] (Var x) = error ("La variable "++ (show x) ++ " no esta declarada en el contexto")

vt ((x,tx):rg) (Var y) = if x==y then tx else vt rg (Var y)

vt _ (VNum _) = TNat
vt _ (VBol _) = TBol
vt t (Suma a b) 
	| vt t a == TNat = if ((vt t b) == TNat)
						then TNat
						else error "Solo se pueden sumar numeros"
	| vt t a == TBol = error "Solo se pueden sumar numeros"

vt t (Prod a b) 
	| vt t a == TNat = if ((vt t b) == TNat)
						then TNat
						else error "Solo se pueden multiplicar numeros"
	| vt t a == TBol = error "Solo se pueden multiplicar numeros"

vt t (Let (Var x) val exp) = 
	let 
		tVar = vt ((x, vt t val):t) (Var x)
		tExp = vt ((x, vt t val):t) exp
	in
		if (tVar == tExp)
			then tExp
			else error "Los tipos no coinciden"

vt t (Pred elem)
	| vt t elem == TNat = TNat
	| otherwise = error "La funcion Pred es solo para numeros"

vt t (Suc elem)
	| vt t elem == TNat = TNat
	| otherwise = error "La funcion Suc es solo para numeros"

vt t (Iszero elem)
	| vt t elem == TNat = TBol
	| otherwise = error "La funcion Iszero es solo para numeros"

vt t (Ifte ev th el)
	| vt t ev == TBol = vt t th
	| otherwise = error "La funcion Ifte tiene que evaluar algo de tipo TBol"