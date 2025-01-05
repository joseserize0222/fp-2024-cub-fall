module Expr where

data Expr
  = Num Double          -- A number
  | Add Expr Expr       -- Addition
  | Mul Expr Expr       -- Multiplication
  | Diff Expr Expr      -- Subtraction
  | Pow Expr Expr       -- Power (x^y)
  | Div Expr Expr       -- Division
  | Sqrt Expr           -- Square root
  | Cbrt Expr           -- Cube root (cbrt)
  | Reciprocal Expr     -- 1/x
  | Square Expr         -- x^2
  | Cube Expr           -- x^3
  | Ln Expr             -- Natural logarithm
  | Log10 Expr          -- Base 10 logarithm
  | LogBase Expr Expr   -- Logarithm with base y (logy)
  | Exp Expr            -- Exponential (e^x)
  | Rad Expr            -- Expression in radians
  | Deg Expr            -- Expression in degrees (optional)
  | Sin Expr            -- Sine
  | Cos Expr            -- Cosine
  | Tan Expr            -- Tangent
  | ASin Expr           -- Arcsine (sin^-1)
  | ACos Expr           -- Arccosine (cos^-1)
  | ATan Expr           -- Arctangent (tan^-1)
  | Sinh Expr           -- Hyperbolic sine
  | Cosh Expr           -- Hyperbolic cosine
  | Tanh Expr           -- Hyperbolic tangent
  | ASinh Expr          -- Hyperbolic arcsine
  | ACosh Expr          -- Hyperbolic arccosine
  | ATanh Expr          -- Hyperbolic arctangent
  | Factorial Expr      -- Factorial
  | Pi                  -- Constant Pi
  | E                   -- Constant e
  | Exp10 Expr Expr     -- Scientific notation (EE)
  deriving (Eq)

instance Show Expr where
  show (Num x)         = if x < 0 then "(" ++ show x ++ ")" else show x
  show (Add x y)       = "(" ++ show x ++ ")" ++ " + " ++ "(" ++ show y ++ ")"
  show (Mul x y)       = "(" ++ show x ++ ")" ++ " * " ++ "(" ++ show y ++ ")"
  show (Diff x y)      = "(" ++ show x ++ ")" ++ " - " ++ "(" ++ show y ++ ")"
  show (Pow x y)       = "(" ++ show x ++ ")" ++ " ^ " ++ "(" ++ show y ++ ")"
  show (Div x y)       = "(" ++ show x ++ ")" ++ " / " ++ "(" ++ show y ++ ")"
  show (LogBase b x)   = "(" ++ show b ++ ")" ++ " Lny " ++ "(" ++ show x ++ ")"
  show (Exp10 b x)     = "(" ++ show b ++ ")" ++ " EE " ++ "(" ++ show x ++ ")"
  show (Sqrt x)        = "sqrt(" ++ show x ++ ")"
  show (Cbrt x)        = "cbrt(" ++ show x ++ ")"
  show (Reciprocal x)  = "1/x(" ++ show x ++ ")"
  show (Square x)      = "x^2(" ++ show x ++ ")"
  show (Cube x)        = "x^3(" ++ show x ++ ")"
  show (Ln x)          = "ln(" ++ show x ++ ")"
  show (Log10 x)       = "log10(" ++ show x ++ ")"
  show (Exp x)         = "exp(" ++ show x ++ ")"
  show (Rad x)         = "rad(" ++ show x ++ ")"
  show (Deg x)         = "deg(" ++ show x ++ ")"
  show (Sin x)         = "sin(" ++ show x ++ ")"
  show (Cos x)         = "cos(" ++ show x ++ ")"
  show (Tan x)         = "tan(" ++ show x ++ ")"
  show (ASin x)        = "asin(" ++ show x ++ ")"
  show (ACos x)        = "acos(" ++ show x ++ ")"
  show (ATan x)        = "atan(" ++ show x ++ ")"
  show (Sinh x)        = "sinh(" ++ show x ++ ")"
  show (Cosh x)        = "cosh(" ++ show x ++ ")"
  show (Tanh x)        = "tanh(" ++ show x ++ ")"
  show (ASinh x)       = "asinh(" ++ show x ++ ")"
  show (ACosh x)       = "acosh(" ++ show x ++ ")"
  show (ATanh x)       = "atanh(" ++ show x ++ ")"
  show (Factorial x)   = "(" ++ show x ++ ")" ++ "!"
  show Pi              = "π"
  show E               = "e"

