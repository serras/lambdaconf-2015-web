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

```json
{ "key1": value1, "key2": value2, ...}
```

we write inside `aeson`:

```haskell
object [ "key1" .= value1
       , "key2" .= value2
       , ...]
```
