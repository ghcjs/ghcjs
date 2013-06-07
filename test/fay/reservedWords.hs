
main = do
  -- All reserved words
  let break = "break" in putStrLn break
  let catch = "catch" in putStrLn catch
  let const = "const" in putStrLn const
  let continue = "continue" in putStrLn continue
  let debugger = "debugger" in putStrLn debugger
  let delete = "delete" in putStrLn delete
  let enum = "enum" in putStrLn enum
  let export = "export" in putStrLn export
  let extends = "extends" in putStrLn extends
  let finally = "finally" in putStrLn finally
  let for = "for" in putStrLn for
  let function = "function" in putStrLn function
  let implements = "implements" in putStrLn implements
  let instanceof = "instanceof" in putStrLn instanceof
  let interface = "interface" in putStrLn interface
  let new = "new" in putStrLn new
  let null = "null" in putStrLn null
  let package = "package" in putStrLn package
  let private = "private" in putStrLn private
  let protected = "protected" in putStrLn protected
  let public = "public" in putStrLn public
  let return = "return" in putStrLn return
  let static = "static" in putStrLn static
  let super = "super" in putStrLn super
  let switch = "switch" in putStrLn switch
  let this = "this" in putStrLn this
  let throw = "throw" in putStrLn throw
  let try = "try" in putStrLn try
  let typeof = "typeof" in putStrLn typeof
  let undefined = "undefined" in putStrLn undefined
  let var = "var" in putStrLn var
  let void = "void" in putStrLn void
  let while = "while" in putStrLn while
  let with = "with" in putStrLn with
  let yield = "yield" in putStrLn yield

  putStrLn ""
  -- Stdlib functions that need to be encoded
  putStrLn $ const "stdconst" 2
