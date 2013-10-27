-- While Haskell syntax might look complicated, it is really a lot
-- simpler than it looks at first glance. Excerpt from A History
-- of Haskell:
--
-- | Haskell has 21 reserved keywords that cannot be used as names
-- | for values or types. This is a relatively low number (Erlang
-- | has 28, OCaml has 48, Java has 50, C++ has 63---and Miranda has
-- | only 10)
--
-- This is an indication of the Haskell syntax really being fairly
-- small. The reason the people think Haskell has complex syntax is
-- that when they read other peoples Haskell programs, those people
-- have defined (or used other peoples definitions of) many very,
-- very clever functions which do amazing things with minimal code.
--
-- Defining such functions is not possible in all languages, so
-- people coming from less powerful languages are used to functions
-- doing pretty ordinary stuff, and leaving the amazing things to
-- the built-in syntax. In Haskell, the amazing things often have
-- nothing at all to do with the built-in syntax, and if you are
-- clever enough, you can write those things yourself as a function.
--
-- Yes, these clever functions take time to master, but they are
-- not vital parts of Haskell, and most of all -- they are not
-- syntax! The syntax of Haskell is rather minimal and can be
-- learnt quickly.
-- 
-- This is meant as a sort of informal summary of the basic
-- syntactic elements you need to get started with writing Haskell
-- programs. Many (if not most) of the amazing things people do can
-- be expressed with this minimal subset of Haskell syntax.
--
-- Because Lisp people are the ones commonly complaining about the
-- complicated syntax of Haskell, this is somewhat aimed at Lisp
-- people, but I think others might benefit from it too.
--
-- ~kqr, 2012-12-24
--
--
--
-- Expressions
--
-- There are two "kinds" of expressions: function calls and operator
-- applications. Function calls are written with the function name
-- followed by the arguments, separated by spaces.
--
-- arg1 and arg2 can of course be other expressions!
--
-- Function calls look like this, when wrapped with parentheses. Does
-- it look familiar? Normally, not all expressions are written with
-- parentheses, because there does exist some rules of precendence,
-- but you can of course throw them out of the window and just go
-- by parentheses!
--
-- Function evaluation:

(functionName arg1 arg2 ...)

-- Operators can only have two arguments, one on either side of
-- the operator. Here, too, can arg1 and arg2 of course be
-- expressions themselves.
--
-- Operators are really the same thing as functions, only with
-- name consisting of symbols and written infix.
--
-- Operator application:

(arg1 * arg2)


-- Function definition
--
-- arg1 and arg2 are names of parameters that can be used in the
-- function body, which in turn consists of a single expression.
--
-- arg1 and arg2 can, for the purpose of simplicity, *not* be
-- expressions here, but have to be names of parameters.

functionName arg1 arg2 ... = expression


-- Variable assignment
--
-- var1 and var2 are variables that will evaluate to the value
-- of their respective expressions. Only a single expression per
-- variable is allowed, which makes sense. The whole assignment
-- block (from let to expression) is considered a single expression
-- itself.

(let var1 = expression1
     var2 = expression2
     ...
 in  expression)


-- If expressions
--
-- The whole if expression is considered a single expression.
-- condition is an expression evaluating to either True or
-- False. If condition is True, the if expression will
-- evaluate to the same thing as expression1, otherwise
-- to the same thing as expression2.
--
-- Once again, these parentheses are not always required, but
-- neither are they prohibited.

(if condition
    then expression1
    else expression2)


-- Common operators
--
-- Multiplication
(3 * 4)

-- Addition
(2 + 3)

-- Subtraction
(9 - 5)

-- Create a list from a first element and a tail (create a cons cell, to the lispers)
-- (This example will evaluate to [3, 4, 5, 6])
(3 : [4, 5, 6])

-- Equality check
(3 == 3)

-- Inequality check
(3 /= 4)

-- Larger than/less than/larger than or equal to/less than or equal to:
(4 > 3)
(6 < 7)
(5 >= 5)
(6 <= 12)


-- Common functions
--
-- Checks if a list is empty
-- (Note: This is accomplishes the same thing as
-- (list == []) but looks more lispy.)
(null list)

-- Returns the first element of a list
(first list)

-- Returns the rest of a list
(tail list)

-- Returns the length of a list
(length list)


-- Common conversions between types
--
-- Integer to float
(fromIntegral 4)

-- Float to integer
(floor 3.75)

-- Integer to char (This requires importing Data.Char)
(chr 43)

-- Char to integer (This requires importing Data.Char)
(ord 'z')