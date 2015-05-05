# Exercise 2 - Software Transactional Memory (STM)

In this exercise we will focus on building a small web application to save a list of tasks in memory. The interface is going to be super-simple:

* `/show` prints the list of values,
* `/add/:title` adds a task with the corresponding title to the list,
* `/remove/:n` deletes the n-th element from the list.

In a non-pure language (think of any imperative one, or even Clojure or OCaml), you can build a simple solution by simply using a variable available in the whole web application code, which is queried or updated by each of the methods.

However, any web application is inherently a concurrent application, since several users may request at the same time. This creates a data race problem with a global variable: two users might update the list with different values. Then, at the moment of querying it, one of the values is lost.

## Introducing STM

A nice solution to this problem, pioneered by Clojure, is Software Transactional Memory (STM for short). In this model, instead of plain variables, you use *transactional variables*. This means that any access to them works in the context of a transaction, and as in the database world, it fulfills properties of atomicity, consistency and isolation.

Haskell's implementation of STM is found in the aptly-named [`stm`](http://hackage.haskell.org/package/stm) package. The two most important pieces in the puzzle are the [`STM` monad](http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Monad-STM.html#t:STM), which is used to describe transaction scripts, and the [`atomically` function](http://hackage.haskell.org/package/stm-2.4.4/docs/Control-Monad-STM.html#v:atomically), which executes one of these transactions.

The `stm` packages provides several data structures with transactional access, but here we shall focus only on the variables. A transactional variable holding values of type `t` is represented by a `TVar t`. Along with the type, it provides four primitive operations:

* `newTVar :: a -> STM (TVar a)` creates a new transactional variable holding a given initial value.
* `readTVar :: TVar a -> STM a` queries the current value.
* `writeTVar :: TVar a -> a -> STM ()` changes the content of the transactional variable to the given one.
* `modifyTVar :: TVar a -> (a -> a) -> STM ()` mutates the content of a variable by applying a *function* to the current value. You can think of it as sort of a restricted `fmap` over the current contents.

Since `STM` forms a monad, you can combine several of these operations in a larger transaction script. For example, suppose that `number` is a `TVar Int`, which holds some global counter for your program. We want that counter to get back to zero once it gets past 100. The update operation doing this looks like:

```haskell
do n <- readTVar number
   if n >= 100
      then writeTVar 0
      else modifyTVar (+1)
```

Even though it looks like we are performing two operations, and that concurrent access could slip one thread between the logic between them, the `stm` package ensures that this is not the case, and that the entire transaction is executed as a whole.

## `liftIO . atomically`

Up to now we have talked about writing transaction scripts, but our only mention of execution was the `atomically` function. Let's have a closer look at its type:

```haskell
atomically :: STM a -> IO a
```

You might wonder why we need to resort to `IO`. The first reason is that, even though transactional, `TVar`s work as sort of global variables. Executing the same transaction script twice might not result in the same outcome, because of the values of those variables.

Furthermore, it is possible that the runtime decides to block a user while waiting for a previous transaction to end, of that a certain transaction needs to be rolled back. All of these are visible outputs of the execution which are not reflected merely on the output type.

So, I have already given you the way to execute a transaction. But the result is in `IO`, and I am writing a Spock application! How am I suppose to integrate these two worlds? The answer is simple: if you look at the [documentation of the `SpockT` monad](http://hackage.haskell.org/package/Spock-0.7.9.0/docs/Web-Spock-Safe.html#t:SpockT), you will find that it is an instance of `MonadIO`.

`MonadIO` is a humble type class in terms of methods. It only provides one:

```haskell
liftIO :: IO a -> m a
```

but the consequences are big. In any `MonadIO` instances you can execute any `IO` action by wrapping it with `liftIO`. Thus, executing a STM transaction script in your code will look similar to:

```haskell
-- Version 1
liftIO $ atomically $ transaction
-- Version 2
liftIO . atomically $ transaction
```

## Counter example

Putting transaction scripts, execution and `MonadIO` together in the code can be challenging at first. The following code provides an example of web application which holds a counter which is incremented in a transactional way:

```haskell
main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- newTVarIO = atomically . newTVar
  number <- liftIO $ newTVarIO (0 :: Integer)
  get "show" $ do
    n <- liftIO $ atomically $ readTVar number
    text (show n)
  get "increment" $ do
    liftIO $ atomically $ modifyTVar number (+ 1)
```

## Further reading

I have only shown a small drop of the power of STM, but the `stm` package is a vast ocean. Further sources of information are the freely available [*Parallel and Concurrent Programming in Haskell book*](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html), my own book [*Beginning Haskell*](http://www.apress.com/9781430262503) or some of the [research papers](https://wiki.haskell.org/Research_papers/Parallelism_and_concurrency#Lock_free_data_structures_and_transactional_memory) in the subject.

One thing transactions can do that we haven't taken advantage of is failing via the `retry` function. For example, because your variable represents a limited amount of a shared resource and the current counter is now at 0. The semantics of `retry` is that the transaction will be re-executed until it succeeds. But this is clearly a waste of time: instead, the run-time keeps track of which variables were looked at to decide a `retry` and only gets up the transaction when any of these changes.

In this exercise we have only spoken about transactional variables, but the `stm` packages also provides transactional arrays, channels and queues in a handful of forms (closable, unbounded/bounded...).
