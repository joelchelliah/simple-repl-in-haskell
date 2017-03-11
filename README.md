# simple-repl-in-haskell
> How to make you very own REPL in Haskell

This is a simple step-by-step tutorial on how to create a very simple **REPL** in Haskell. 

Prior knowledge or experience with Haskell is not required to grasp the basic principles of this walkthrough, 
although some basic understanding of functional programming will probably make it a lot easier to understand 
the example code.

## What's a REPL
REPL stands for Read-Eval-Print-Loop, and as the name implies, its job is to:
  1. **Read** the given input, and parse it into a data structure that can be evaluated.
  2. **Evaluate** the parsed expression, based on some set of rules or logic.
  3. **Print** the result of the evaluated epxression.
  4. **Repeat** (loop) until signaled to stop.

REPLs are most commonly associated with programming languages, where they are used as an interactive 
shell for interpreting the code written that language. Most languages often come with such REPLs already bundled 
into their toolkit (e.g. Ruby's [IRB](https://en.wikipedia.org/wiki/Interactive_Ruby_Shell), or Haskell's [GHCi](https://wiki.haskell.org/GHC/GHCi)). 
These days, there's also an evergrowing number of online REPLs that support several different languages, 
such as [repl.it](https://repl.it/site/about). 

In these cases, the **read** and **evaluate** parts are focused around interpreting the input 
based on the language that provides the REPL. However, stricly speaking, a REPL doesn't necessarily have 
to be connected to a fully fleshed out programming language (or any language at all). Although that is its
most common use case, one can argue that a REPL can pretty much be anything you want it to be, as long as 
it can **read**, **evaluate** and **print** whatever you throw at it in a repeating **loop**. 

So by that definition, let's get started on creating our very own, very simple, REPL!


## How to make a REPL
As stated earlier, a REPL consists of four steps. We will start by implementing each of these steps as seperate functions and finally compose them together to create our REPL.

### Read
The first step is reading and parsing the input. Since we want to keep things simple, we'll ignore the parsing bit and stick to just reading the input in as a regular string:
```Haskell
read' :: IO String
read' =  getLine
```
For this, all we need is the `getLine` function. This will read the input and return it as an `IO String`. The `IO` wrapper is there to indicate that the string was produced by an IO action, which in this case is some input provided by the user. 

Note that the *tick* in the name of our `read'` function has no special purpose other than to differentiate it from Haskell's built-in `read` function.

Let's go ahead and add a few more lines here for convenience:
```Haskell
read' :: IO String
read' = putStr "REPL> "
     >> hFlush stdout
     >> getLine
```
We've added two additional actions here: `putStr "REPL> "` simply prints **REPL>** at the start of the prompt, and `hFlush stdout` is to immediately flush the stream just to make sure that nothing is stuck in the buffers. Finally we combine all three IO actions together with the `>>` sequencing operator (which you can read as *and-then*).

### Eval
The next step is to evaluate the input that we have read. The easiest implementation would be to just skip this part entirely, and let the function just return its given input. Let's do that for now:
```Haskell
eval' :: String -> String
eval' input = input
```

### Print
No need for anything fancy here. The `putStrLn` will print the given string to the console:
```Haskell
print' :: String -> IO ()
print' = putStrLn
```
The `IO ()` type in the signature indicates that we are not returning anything, but still performing an IO action (i.e printing to the console).

### Loop
The last step is to create a repeating loop around our three previous steps:
```Haskell
if (input == ":quit")
then return ()
else print' (eval' input) >> loop'
```
We simply check if the input equals `":quit"`, in which case we exit our REPL. Otherwise, we evaluate the input, 
print the result and restart the loop. 

### Glueing it all together

Finally, let's go ahead and put everything together inside our `main` entry point:
```Haskell
main :: IO ()
main = do
  input <- read'
  
  unless (input == ":quit")
       $ print' (eval' input) >> main
```
First, the input string is extracted from the `IO String` value returned by our `read'` function, using the `<-` operator. It is then passed on to the looping logic which we defined earlier. The `unless` function here works exactly like our `if`/`else` logic in the previous code. It will exit the program if `input == ":quit"`.

So there we have it! A very simple, albeit rather useless REPL.

## Running the REPL
> Todo

## Playing around with evaluators
> Todo!


## Additional functionality
> Todo!
