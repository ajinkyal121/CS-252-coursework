import Language.Haskell.TH (valD)
This File is _literate Haskell_.
That means that (in some sense) code and comments are reversed.
By default, everything that I type is actually a comment.

To write code, I preface it with a 'greater than' symbol.
Here we define the expressions in our language:

> data Exp = ETrue
>          | EFalse
>          | Eval Int
>          | Eif Exp Exp Exp
>          | ESucc Exp
>          | EPred Exp
>   deriving (Show,Eq)

When an expression is evaluated, it returns a value.

> data Val = VTrue
>          | VFalse
>          | VVal Int
>          | VZero
>   deriving (Show,Eq)

The evaluate function takes an expression and returns a value
The VTrue case has been done for you.
You must complete the other cases.

> evaluate :: Exp -> Val
> evaluate ETrue = VTrue
> evaluate EFalse = VFalse
> evaluate (Eval x) 
>   | 0 <- x          = VZero 
>   | otherwise       = VVal x
> evaluate (Eif cond expTrue expFalse) 
>   | evaluate cond == VTrue = evaluate expTrue 
>   | otherwise              = evaluate expFalse 
> evaluate (ESucc val)
>   | val == Eval 0            = VVal 1
>   | VVal val <- evaluate val = VVal (val + 1)
>   | otherwise                = error "stuck"
> evaluate (EPred val)
>   | val == Eval 0            = VZero
>   | VVal val <- evaluate val = VVal (val - 1)
>   | otherwise                = error "stuck"


And here we have a couple of programs to test.
prog1 should evaluate to VTrue and prog2 should evaluate to VFalse

> prog1 = Eif ETrue ETrue EFalse
> prog2 = Eif (Eif ETrue EFalse ETrue) ETrue (Eif ETrue EFalse ETrue)
> prog3 = Eval 4
> prog4 = ESucc (Eval 4)
> prog5 = EPred (Eval 4)
> prog6 = EPred (Eval 0)
> prog7 = ESucc (Eval 0)



The following lines evaluate the test expressions and display the results.
Note the type of main.  'IO ()' indicates that the function performs IO and returns nothing.
The word 'do' begins a block of code, were you can effectively do sequential statements.
(This is a crude generalization, but we'll talk more about what is going on in this function
when we deal with the great and terrible subject of _monads_.)

> main :: IO ()
> main = do
>   putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
>   putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)
>   putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
>   putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)
>   putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate prog5)
>   putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ evaluate prog6)
>   putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ evaluate prog7)


Once you have the evaluate function working
you add in support the expressions 'succ e', 'pred e', and 'zero'.
With this change, it is possible for evaluate to get 'stuck',
e.g. pred true.
For a first pass, simply use the error function in these cases.


