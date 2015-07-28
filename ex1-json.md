# Exercise 1 - JSON

In many cases, web applications do not return HTML, but rather some more data-oriented format such as XML or JSON. In this exercise we are going to look at the [aeson](http://hackage.haskell.org/package/aeson) library, considered the most performant and nicest library to manipulate JSON in the Haskell world.

Throughout all the exercises we are going to move from a template with holes to a working solution. In case you had not downloaded the template code from the repository, please do it now by either cloning the repository or [downloading a ZIP](https://github.com/serras/lambdaconf-2015-web/archive/master.zip).

## Retuning JSON

If you look at the [main file](https://github.com/serras/lambdaconf-2015-web/blob/master/ex1-json/src/Main.hs) from the `ex1-json` project, you shall find the following code:

```haskell
json $ object [ "hello" .= String name ]
```

The `json` function comes from Spock and is similar to the `text` and `html` functions which we have introduced before. Its purpose is to signal the web framework that a JSON value is the result of executing the route. But in this case, the argument to the call is no longer a string-like value, but rather some other kind of data.

This new kind of data comes must be of the type `Value` from the `aeson` package, and represents a JSON value as a Haskell data type. Indeed, if you look at the definition of `Value` (extracted directly from `aeson`'s source code):

```haskell
data Value = Object !Object
           | Array  !Array
           | String !Text
           | Number !Scientific
           | Bool   !Bool
           | Null
```

you shall notice that the constructors correspond to the basic ways to build JSON values: objects, arrays, strings, numbers, booleans and null. Around this simple data type, `aeson` builds a series of niceties to make it easier to work with JSON.

You may have notice, however, that we haven't used the `Object` constructor in our example. Inside the library, `Object` is defined as needing a hashmap of strings to values: this is a very performant way to operate with it, but unfortunately does not provide a very nice interface for writing a JSON value as we are doing here. Instead, it is more convenient to use the `object` function, which expects a list of pairs as key-values to JSON. That is, if we want to represent the JSON value:

```
{ "key1": value1, "key2": value2, ...}
```

we write inside `aeson`:

```haskell
object [ "key1" .= value1
       , "key2" .= value2
       , ...]
```

**Task 1**: rewrite the code for the `/allow/:age` route to return either `{ "allowed": true }` or `{ "allowed": false }`.

**Task 2**: the template code includes the definition of a `Person` data type and a list of people. Add a route to your Spock application of the form `/person/:n`, which returns the information of the n-th person in the list as JSON.

*Hint*: to obtain the n-th element of a list, use the `(!!)` function from the `Data.List` module.

## ToJSON

For the second task it is quite probable that you had written a function, or a piece of code, which transforms a `Person` value into a corresponding `Value` from `aeson`. Given that this a very common operation, `aeson` defines a `ToJSON` type class. Each type which is an instance of this class has a way to be converted to JSON. For example:

```haskell
instance ToJSON Person where
  toJSON (Person nm ag ad) = object [ "name"    .= nm
                                    , "age"     .= ag
                                    , "address" .= ad
                                    ]
```

This instance definition itself makes use of the `ToJSON` type class. Note that we didn't have to mark explicitly how to convert a string or an `Integer` to JSON: the conversion is done automatically for us when using the `(.=)` operator.

The `json` function from Spock also works on any `ToJSON` value, not only of those from the `Value` type. In conclusion, task 2 can be solved with only two lines of code once you have the instances in place:

```haskell
get ("person" <//> ix) $ \ix ->
  json $ listPeople !! ix
```

**Task 3**: create a new data type `Task` which stores a title, a description and the `Person` who is in charge of the task. Write a corresponding `ToJSON` instance. Note that you can reuse the definition for `Person` in the code for `Task`.

Imagine however that your code does not include two, but dozens of data types which you might need to show to the user as JSON: what a nightmare of boilerplate code! Luckily, GHC has a feature called *generic deriving* which alleviates this problem.

The first step is to make your type an instance of the `Generic` type class, which can be done automatically by the compiler. Given that generic deriving is not part of the Haskell standard, you need to enable a GHC extension. The way extensions are added to a file is via a `{-# LANGUAGE Extension #-}` line at the beginning of the source. In our case, it should read:

```haskell
{-# LANGUAGE DeriveGeneric #-}
```

Then, you need to import the `Generic` type class into scope, via:

```haskell
import GHC.Generics
```

Finally, include a `deriving` clause in your data type:

```haskell
data Person = Person { name    :: String
                     , age     :: Integer
                     , address :: String
                     } deriving Generic
```

In short, being an instance of `Generic` means that the code can perform reflection on the structure of the data type. In particular, functions or instances which depend on the shape of a data type can be defined all at once for those types which are `Generic`.

The class `ToJSON` is one of those type classes. Given a record definition, like that of `Person`, you can generically build a way to show the information in JSON format by using as keys the names of the records. To signal the compiler that you want that instance to be automatically generated for you, just write:

```haskell
instance ToJSON Person
```

## Conclusion

In this exercise we have looked at the capabilities for creating JSON values out of Haskell values. In particular, we have looked at the generic deriving mechanism in GHC which helps us removing lots of boilerplate code.
