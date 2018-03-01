This article has been given [a better home on my web
page](https://two-wrongs.com/a-gentle-introduction-to-monad-transformers).
This will hopefully give it a more lasting presence, and I personally
also think it looks better and is easier to read over there!


A Gentle Introduction to Monad Transformers
===========================================

or, Values as Exceptions
------------------------


### Table of Contents

[Either Left or Right](#either)  
[Introducing Side-Effects](#side-effects)  
[We Can Make Our Own Monads](#either-io)  
[Implementing Instances for Common Typeclasses](#type-classes)  
[Using `EitherIO`](#using)  
[Do You Even Lift?](#lifting)  
[Signalling Errors](#throwing)  
[`throwE`? What Is This, Java?](#java)  
[`ExceptIO`](#except-io)  
[Gotta Catch 'Em All](#catch)  
[Going General](#transformer)  
[Appendix A: Complete Program](#code)



<a name="either"/>

### Either Left or Right

Before we break into the mysterious world of monad transformers, I want to
start with reviewing a much simpler concept, namely the `Either` data type.
If you aren't familiar with the `Either` type, you should probably not jump
straight into monad transformers – they do require you to be somewhat
comfortable with the most common monads.

With that out of the way:

Pretend we have a function that extracts the domain from an email address.
Actually [checking this properly is a rather complex topic][1] which I will
avoid, and instead I will assume an email address can only contain one `@`
symbol, and everything after it is the domain.

I'm going to work with `Text` values rather than `String`s. This
means if you don't have the `text` library, you can either work with
`String`s instead, or `cabal install text`. If you have the Haskell platform,
you have the `text` library.

We need to import `Data.Text` *and* set the `OverloadedStrings` pragma. The
latter lets us write string literals (such as `"Hello, world!"`) and have
them become `Text` values automatically.

```
λ> :module +Data.Text
λ> :set -XOverloadedStrings
```

Now, figuring out how many `@` symbols there are in an email address is
fairly simple. We can see that

```
λ> splitOn "@" ""
[""]

λ> splitOn "@" "test"
["test"]

λ> splitOn "@" "test@example.com"
["test", "example.com"]

λ> splitOn "@" "test@example@com"
["test", "example", "com"]
```

So if the split gives us just two elements back, we know the address
contains just one `@` symbol, and we also as a bonus know that the second
element of the list is the domain we wanted. We can put this in a file.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError = InvalidEmail
  deriving Show


getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail
```

This draws on our previous discoveries and is pretty self-explainatory. The
function returns `Right domain` if the address is valid, otherwise
`Left InvalidEmail`, a custom error type we use to make handling the errors
easier later on. (Why this is called `LoginError` will be apparent soon.)

This function behaves as we expect it to.

```
λ> getDomain "test@example.com"
Right "example.com"

λ> getDomain "invalid.email@example@com"
Left InvalidEmail
```

To deal with the result of this function immediately, we have a couple of
alternatives. The basic tool to deal with `Either` values is pattern matching,
in other words,

```haskell
printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text        -> T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"
```

Testing in the interpreter shows us that

```
λ> printResult' (getDomain "test@example.com")
Domain: example.com

λ> printResult' (getDomain "test#example.com")
ERROR: Invalid domain
```

Another way of dealing with `Either` values is by using the `either` function.
`either` has the type signature

```haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
```

In other words, it "unpacks" the `Either` value and applies one of the two
functions to get a `c` value back. In this program, we have an
`Either LoginError Text` and we want just a `Text` back, which tells us what
to print. So we can view the signature of `either` as

```haskell
either :: (LoginError -> Text) -> (Text -> Text) -> (Either LoginError Text -> Text)
```

and writing `printResult` with the help of `either` yields a pretty neat
function.

```haskell
printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")
```

This function works the same way as the previous one, except with the
pattern matching hidden inside the call to `either`.


<a name="side-effects"/>

### Introducing Side-Effects

Now we'll use the domain as some sort of "user token" – a value the user
uses to prove they have authenticated. This means we need to ask the user
for their email address and return the associated token.

```haskell
getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)
```

So when `getToken` runs, it'll get an email address from the user and
return the domain of the email address.

```
λ> getToken
Enter email address:
test@example.com
Right "example.com"
```

and, importantly,

```
λ> getToken
Enter email address:
not.an.email.address
Left InvalidEmail
```

Now, let's complete this with an authentication system. We'll have two users
who both have terrible passwords:

```haskell
users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]
```

With an authentication system, we can also run into two new kinds of errors,
so let's change our `LoginError` data type to reflect that.

```haskell
data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show
```

We also need to write the actual authentication function. Here we go...

```haskell
userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userpw == password
             then return token

             else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left
```

This beast of a function gets the email and password from the user,
checks that the email was processed without problems, finds the
user in the collection of users, and if the passwords match, it returns
the token to show the user is of authenticated.

If anything goes wrong, such as the passwords not matching, there not
being a user with the entered domain, or the `getToken` function failing to
process, then a `Left` value will be returned.

This function is *not* something we want to deal with. It's big, it's bulky,
it has several layers of nesting... it's not the Haskell we know and love.

Sure, it's possible to rewrite it using function calls to `either` and
`maybe`, but that wouldn't help very much. The real reason the code is this
ugly is that we're trying to mix both `Either` and `IO`, and they don't seem
to blend well.

The core of the problem is that the `IO` monad is designed for dealing with
`IO` actions, and it's terrible at handling errors. On the other hand, the
`Either` monad is great at handling errors, but it can't do `IO`. So let's
explore what happens if you imagine a monad that is designed to *both*
handle errors *and* `IO` actions.

Too good to be true? Read on and find out.


<a name="either-io"/>

### We Can Make Our Own Monads

We keep coming across the `IO (Either e a)` type, so maybe there is something
special about that. What happens if we make a Haskell data type out of that
combination?

```haskell
data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}
```

What did we get just by doing this? Let's see:

```
λ> :type EitherIO
EitherIO :: IO (Either e a) -> EitherIO e a

λ> :type runEitherIO
runEitherIO :: EitherIO e a -> IO (Either e a)
```

So already we have a way to go between our own type and the combination
we used previously! That's *gotta* be useful somehow.


<a name="type-classes"/>

### Implementing Instances for Common Typeclasses

This section might be a little difficult if you're new to the language and
haven't had a lot of exposure to the internals of how common typeclasses
work. You don't *need* to understand this section to continue reading the
article, but I strongly suggest you put on your to-do list to learn enough
to understand this section. It touches on many of the core components of
what makes Haskell Haskell and not just another functional language.

If you want to read more about this kind of thing, [The Typeclassopedia][2]
by Brent Yorgey is a comprehensive reference of the most common typeclasses
used in Haskell code. Learn You a Haskell has a popular [introduction to the
three typeclasses we use here][3]. Additionally, Adit provides us with [a
humourous picture guide to the same three typeclasses][4].

But before we do anything else, let's make `EitherIO` a functor, an
applicative and a monad, starting with the functor, of course.

```haskell
instance Functor (EitherIO e) where
  fmap f ex = wrapped
    where
      unwrapped = runEitherIO ex
      fmapped   = fmap (fmap f) unwrapped
      wrapped   = EitherIO fmapped
```

This may look a little silly initially, but it does make sense. First, we
"unwrap" the `EitherIO` type to expose the raw `IO (Either e a)` value. Then
we `fmap` over the inner `a`, by combining two `fmap`s. Then we wrap the
new value up in `EitherIO` again, and return the wrapped value. If you are
a more experienced Haskell user, you might prefer the following, equivalent,
definition instead.

```haskell
instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO
```

In a sense, that definition makes it more clear that you are just unwrapping,
running a function on the inner value, and then wrapping it together again.

The two other instances are more of the same, really. Creating them is a
mostly mechanical process of following the types and unwrapping and wrapping
our custom type. I challenge the reader to come up with these instances on
their own before looking below how I did it, because trying to figure these
things out *will* improve your Haskell abilities in the long run.

In any case, explaining them gets boring, so I'll just show you the instances
as an experienced Haskell user might write them.

```haskell
instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)
```

If your definitions look nothing like these, don't worry. As long as your
definitions give the correct results, they are just as good as mine. There
are many ways to write these definitions, and none is better than the other
as long as all are correct.


<a name="using"/>

### Using `EitherIO`

Now that our `EitherIO` type is a real monad, we'll try to put it to work!
If we change the type signature of our `getToken` function, we'll run into
problems quickly, though.

```haskell
getToken :: EitherIO LoginError Text
getToken = do
  T.putStrLn "Enter email address: "
  input <- T.getLine
  return (getDomain input)
```

We get three type errors from this function alone, now! In order:

 1. `T.putStrLn "email"` returns `IO ()`, but we want `EitherIO LoginError ()`
 2. `T.getLine` returns `IO Text`, we want `EitherIO LoginError Text`.
 3. `getDomain input` returns `Either LoginError Text`, we want `EitherIO
LoginError Text`.

Converting `Either e a` to `EitherIO e a` isn't terribly difficult. We have
two functions to help us with that.

```haskell
return   :: Either e a -> IO (Either e a)
EitherIO :: IO (Either e a) -> EitherIO e a
```

With both of those, we can take the `getDomain` function call and fit it in
the new `getToken` function, like so:

```haskell
EitherIO (return (getDomain input))
```

Remember this line, because we're going to put it into the function soon
enough. But first, let's find out how to convert the two `IO a` values to an
`EitherIO e a`. Again, we will have use of the `EitherIO` function. For the
rest, it's useful to know your functors. If you do, you'll realise that

```haskell
fmap Right :: IO a -> IO (Either e a)
```

so our `IO` actions would both be

```haskell
EitherIO (fmap Right (T.putStrLn "email"))
EitherIO (fmap Right (T.getLine))
```

With these three lines, the `getToken` function is now written as

```haskell
getToken :: EitherIO LoginError Text
getToken = do
  EitherIO (fmap Right (T.putStrLn "Enter email address:"))
  input <- EitherIO (fmap Right T.getLine)
  EitherIO (return (getDomain input))
```

But this looks even *more* horrible than it was before! Relax. We'll take a
detour to clean this up slightly.


<a name="lifting"/>

### Do You Even Lift?

The more general pattern here is that we have two kinds of functions: Those
that return `IO` something, and those that return `Either` something. We want
to use both of those in our `EitherIO` monad. Converting a "lesser" monad to
a "more powerful" one, like we want to do here, is often called *lifting* the
lesser operation *into* the more powerful monad.

We can define two lift operations to do exactly this for our case.

```haskell
liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)
```

With these two functions, the `getToken` function in turn is a little more
clean.

```haskell
getToken :: EitherIO LoginError Text
getToken = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)
```

If you try to run this in the interpreter, you'll get a nasty type error. The
reason is that the interpreter expects something of type `IO a`, but
`getToken` has type `EitherIO e a`, so we need to convert it back to `IO a`
when we run it in the interpreter. Fortunately, we had a function that does
just that – `runEitherIO`.

```
λ> runEitherIO getToken
Enter email address:
test@example.com
Right "example.com"
```

We'll also want to do the same conversion for `userLogin`, of course. First
we change the type signature, and then we fix the values that are of the
wrong type. If you haven't been paying 100% attention, the new look of
`userLogin` might surprise you.

```haskell
userLogin :: EitherIO LoginError Text
userLogin = do
  token      <- getToken
  userpw     <- maybe (liftEither (Left NoSuchUser))
                  return (Map.lookup token users)
  password   <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)

  if userpw == password
     then return token
     else liftEither (Left WrongPassword)
```

Where did all the nesting go? It's gone. All the nesting was there simply
because we had to handle a bunch of error cases in the `IO` monad. The IO
monad is not meant to handle error cases, so you have to do it manually.
Our `EitherIO` monad on the other hand, is built *both* to handle errors
*and* to perform IO actions, so we get the best of both worlds. We just
have to signal when errors occur, and the `EitherIO` monad takes care of
the rest.

If we want to, say, print the result of this, we'll need a print function
similar to the one we had previously.

```haskell
printResult :: Either LoginError Text -> IO ()
printResult res =
  T.putStrLn $ case res of
    Right token        -> append "Logged in with token: " token
    Left InvalidEmail  -> "Invalid email address entered."
    Left NoSuchUser    -> "No user with that email exists."
    Left WrongPassword -> "Wrong password."
```

This function, just like the previous one, takes an `Either` value, so we'll
need to "unwrap" our result before we send it over.

```
λ> runEitherIO userLogin >>= printResult
Enter email address:
test@example.com
Enter your password:
qwerty123
Logged in with token: example.com

λ> runEitherIO userLogin >>= printResult
Enter email address:
test@127.0.0.1
No user with that email exists.
```

As you can see, we've got a lot of convenience already, being able to just
signal errors and let our `EitherIO` monad take care of them just like it
takes care of `IO` actions.


<a name="throwing"/>

### Signalling Errors

But how *are* we signalling errors, really? It turns out that to signal
something like `WrongPassword`, we have to return

```haskell
liftEither (Left WrongPassword)
```

and that's not very tidy. We can easily make a function

```haskell
throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)
```

When we have this function, `userLogin` gets even better.

```haskell
userLogin :: EitherIO LoginError Text
userLogin = do
  token      <- getToken
  userpw     <- maybe (throwE NoSuchUser)
                  return (Map.lookup token users)
  password   <- liftIO $ T.putStrLn "Enter your password:" >> T.getLine

  if userpw == password
     then return token
     else throwE WrongPassword
```

<a name="java"/>

### `throwE`? What Is This, Java?

No, of course not. But I did choose that name deliberately. What we have with
our `EitherIO` monad is looking more and more like exceptions in languages like
Java, Python, C++ and so on. And that's not a bad way to view it.

However, there are some differences. One big difference is that our
"exceptions" are just normal values that are being returned from the
function, while more traditional (Java, Python) exceptions are interruptions of normal
control flow. Another difference is that our "exceptions" are checked by the
type system, so we can't forget to catch our exceptions.


<a name="except-io">
  
### `ExceptIO`

But let's entertain that idea further. What happens if we just say goodbye to
`Either` and talk about exceptions instead? First, we'll need to rename our
`EitherIO` monad:

```haskell
data ExceptIO e a = ExceptIO {
    runExceptIO :: IO (Either e a)
}
```

It still works the same as before, it's just been renamed. The names need to
be changed throughout the code, but other than that, the code still works.


<a name="catch"/>

### Gotta Catch 'Em All

So if we can *throw* what basically amounts to exceptions...

Yes! Keep going!

Can we also...

Yes?

...catch them?

Oh, I'm so happy you asked! Of course we can. And it's real simple too! We
need to define a function that does the catching. Call it `catchE`, so it
looks similar to `throwE`.

```haskell
catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler =
  ExceptIO $ do
    result <- runExceptIO throwing
    case result of
      Left failure -> runExceptIO (handler failure)
      success      -> return success
```

This unwraps the `throwing` computation and inspects its result, if it was
successful, it just returns it right back without doing anything. If it was
a failure, it runs the `handler` with the error as an argument, and returns
the result of that instead.

We can use this to spice up our application a little. We'll start by writing
two exception handlers – one that catches just a single exception, and one
that catches all exceptions.

```haskell
wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err
```

The `wrongPasswordHandler` handles only the `WrongPassword` exception, and
rethrows everything else. It responds to a `WrongPassword` exception by
running the `userLogin` function again to give the user a second chance.

The other exception handler will respond to exceptions by printing an
error message and then re-throwing the exception to abort the current
execution.

```haskell
printError :: LoginError -> ExceptIO LoginError a
printError err = do
  liftIO . T.putStrLn $ case err of
    WrongPassword -> "Wrong password. No more chances."
    NoSuchUser -> "No user with that email exists."
    InvalidEmail -> "Invalid email address entered."

  throwE err
```

We'll create a third function where we use these exceptions.

```haskell
loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
  let retry =  userLogin `catchE` wrongPasswordHandler
  token     <- retry `catchE` printError
  liftIO $ T.putStrLn (append "Logged in with token: " token)
```

Note in particular how we "wrap" exception handlers around each other. In
the innermost layer is the `userLogin` computation, which gets wrapped by
the `wrongPasswordHandler` handler, and then that entire package gets wrapped
by the `printError` handler. You need to wrap your handlers in the order you
expect them to catch exceptions from underneath each other.


<a name="transformer"/>

### Going General

There is just one, tiny, little thing I want to change in our `ExceptIO` type.
Currently, we're stuck with only being able to combine `IO` and exceptions.
What if we wanted to combine exceptions with database transactions, or lists,
or some other kind of monad? We can make the other monad an argument to our
exception monad.

Then we'll also have to change the name from `ExceptIO` to `ExceptT`. Why
the `T`, you ask? Because it's a monad transformer. Congratulations! You made
it all the way. You've created and used your first monad transformer. That's
no small feat.

The following is our entire program after that change. Most of the functions
we defined manually already exist in [the `transformers` library][5], in [the
`Control.Monad.Trans.Except` module][6]. Use that when you can instead of creating
your own types, but feel free to reference our program here when you are
curious how something works!


<a name="code"/>

### Appendix A: Complete Program


```haskell
{-# LANGUAGE OverloadedStrings #-}


import Data.Text
import Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data ExceptT e m a = ExceptT {
    runExceptT :: m (Either e a)
}

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure    = ExceptT . pure . Right
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return  = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT . f)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

lift :: Functor m => m a -> ExceptT e m a
lift x = ExceptT (fmap Right x)

throwE :: Monad m => e -> ExceptT e m a
throwE x = liftEither (Left x)

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE throwing handler = ExceptT $ do
  x <- runExceptT throwing
  case x of
    Left failure  -> runExceptT (handler failure)
    Right success -> return (Right success)



data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]



main :: IO ()
main = do
  runExceptT loginDialogue
  return ()

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  let retry =  userLogin `catchE` wrongPasswordHandler
  token     <- retry `catchE` printError
  lift $ T.putStrLn (append "Logged in with token: " token)

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
  lift (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
  lift . T.putStrLn $ case err of
    WrongPassword -> "Wrong password. No more chances."
    NoSuchUser -> "No user with that email exists."
    InvalidEmail -> "Invalid email address entered."
  throwE err



userLogin :: ExceptT LoginError IO Text
userLogin = do
  token      <- getToken
  userpw     <- maybe (throwE NoSuchUser)
                  return (Map.lookup token users)
  password   <- lift (T.putStrLn "Enter your password:" >> T.getLine)

  if userpw == password
     then return token
     else throwE WrongPassword

getToken :: ExceptT LoginError IO Text
getToken = do
  lift (T.putStrLn "Enter email address:")
  input <- lift T.getLine
  liftEither (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail
```


[1]: http://davidcel.is/blog/2012/09/06/stop-validating-email-addresses-with-regex/
[2]: https://wiki.haskell.org/Typeclassopedia
[3]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
[4]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[5]: http://hackage.haskell.org/package/transformers-0.4.1.0
[6]: http://hackage.haskell.org/package/transformers-0.4.1.0/docs/Control-Monad-Trans-Except.html
