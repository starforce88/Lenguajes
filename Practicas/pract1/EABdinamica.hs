{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}


module EABdinamica where

-- Se importa el módulo de entrada y salida así como los analizadores léxico y sintáctico.
-- Tambien se incorpora la semántica estática para evaluar expresiones bien tipadas.
import System.IO
import LexerEAB
import ParserEAB
import EABestatica


--   Sustitución
-- subst e x r  debe devolver e[x:=r].
sust :: Asa -> Ident -> Asa -> Asa
sust e x r = case e of
        (VNum _) -> e
        (VBol _) -> e
        (Var y) -> 
          if(x == y)
            then r
            else e
        (Suma li ld) -> Suma (sust li x r) (sust ld x r)
        (Prod li ld) -> Prod (sust li x r) (sust ld x r)
        (Suc elem) -> Suc (sust elem x r)
        (Pred elem) -> Pred (sust elem x r)
        (Iszero elem) -> Iszero (sust elem x r)
        (Ifte co th el) -> Ifte (sust co x r) (sust th x r) (sust el x r)
        (Let e1 e2 e3) -> Let e1 (sust e2 x r) (sust e3 x r)


--   Valores 
-- Función que nos dice cuándo una expresión es un valor.
esvalor :: Asa -> Bool
esvalor x = case x of 
            (VNum _) -> True
            (VBol _) -> True
            otherwise -> False


-- Evaluación de expresiones
-- Evalúa las expresiones que están bien tipadas.
eval :: Asa -> Asa
eval t 
  | (vt [] t) == TNat = evalaux t
  | (vt [] t) == TBol = evalaux t
  | otherwise = error ((show (vt [] t)) ++ " Expresion mal tipada") 
--eval = error "Te toca"

-- Funcion que nos dice si una expresion se ha bloqueado y es una forma normal.
esbloq :: Asa -> Bool
esbloq (Var _) = True
esbloq _ = False
--esbloq (VNum _) = False
--esbloq (VBol _) = False
--esbloq (Suma a b) = False
--esbloq (Prod a b) = False

-- evalaux hace transiciones mientras no se llegue a un estado final.
evalaux :: Asa -> Asa
evalaux t 
  | esvalor t = t -- es un valor y no se debe evaluar
  | esbloq t = error ((show t) ++ " Expresion bloqueada") -- es una expresion bloqueada y no se puede seguir evaluando
  | otherwise = eval (eval1p t)
--evalaux = error "Te toca"


-- Reglas de transición
-- eval1p hace una transición mientras se pueda aplicar una regla de transición.
eval1p :: Asa -> Asa
eval1p (VNum x) = (VNum x)
eval1p (VBol x) = (VBol x)
eval1p (Suc (VNum x)) = VNum (x+1)
eval1p (Pred (VNum x)) 
          | x==0 = VNum 0
          | otherwise = VNum (x-1)
eval1p (Iszero (VNum x)) 
          | x==0 = VBol True
          | otherwise = VBol False
eval1p (Suma (VNum x) (VNum y)) = VNum (x+y)
eval1p (Prod (VNum x) (VNum y)) = VNum (x*y)
eval1p (Suma a b) = eval1p (Suma (eval1p a) (eval1p b))
eval1p (Prod a b) = eval1p (Prod (eval1p a) (eval1p b))
eval1p (Iszero a) = eval1p (Iszero (eval1p a))
eval1p (Suc a) = eval1p (Suc (eval1p a))
eval1p (Pred a) = eval1p (Pred (eval1p a))
eval1p (Ifte a b c) 
          | eval1p a==VBol True = eval1p b
          | eval1p a==VBol False = eval1p c
eval1p (Let (Var x) val exp) = (sust exp x val)
--eval1p = error "Te toca"


-- 5 Pretty printer
-- Función que transforma un ASA a una expresión en sintaxis concreta. 
concreta :: Asa -> String
concreta (VNum n) = show n
concreta (VBol b) = show b
concreta (Var x) = x
concreta (Suma (VNum n) (VNum m)) = show n ++ " + " ++ show m 
concreta (Suma t (VNum m)) = "("++concreta t ++ ") + " ++ show m 
concreta (Suma (VNum n) t) = show n ++ " + (" ++ concreta t ++ ")"
concreta (Suma t1 t2) =  "("++concreta t1 ++ ") + (" ++ concreta t2 ++ ")"
concreta (Prod (VNum n) (VNum m)) = show n ++ " * " ++ show m 
concreta (Prod t (VNum m)) = "("++concreta t ++ ") * " ++ show m 
concreta (Prod (VNum n) t) = show n ++ " * (" ++ concreta t ++ ")"
concreta (Prod t1 t2) =  "("++concreta t1 ++ ") * (" ++ concreta t2 ++ ")"
concreta (Let (Var x) e1 e2) = "let " ++ x ++ ":=("++ concreta e1 ++ ") in ("++ concreta e2 ++ ")"
concreta (Ifte t1 t2 t3) = "if ("++ concreta t1 ++") then ("++ concreta t2 ++") else ("++ concreta t3 ++")" 
concreta (Suc t) =  "suc"++ "("++ concreta t ++")"
concreta (Pred t) = "pred"++ "("++ concreta t ++")"
concreta (Iszero t) = "isZero("++ concreta t ++ ")"


-- Pruebas
-- Función que recibe el nombre de un archivo que contiene una expresión en sintaxis concreta y la evalúa mostrando el proceso paso a paso.
correPrueba file = do cont <- readFile (file++".eab");       
                      putStr "\n\n";
                      putStr "<<< CONTENIDO DEL ARCHIVO >>>\n";
                      putStr cont;
                      putStr "\n\n";
                      putStr "<<< TOKENS GENERADOS POR EL LEXER >>>\n";
                      let lex = (lexer cont);
                      putStr (show lex);
                      putStr "\n\n";
                      putStr "<<< ASA  GENERADO POR EL PARSER >>>\n";
                      let par = (parse lex);
                      putStr (show par);
                      putStr "\n\n";
                      let tipo = (vt [] par);
                      putStr "<<< TIPO ASA >>>\n";
                      putStr (show tipo);
                      putStr "\n\n";
                      putStr "<<< ASA EVALUADO >>>\n";
                      let eva = (eval par);
                      putStr (show eva);
                      putStr "\n\n";
                      putStr "<<< RESULTADO >>>\n";
                      putStr (concreta (eval (parse (lexer cont))));
                      putStr "\n\n";                         
      
