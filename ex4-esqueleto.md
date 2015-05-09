# Exercise 4 - Esqueleto

This exercise is a follow-up to [exercise 3](https://github.com/serras/lambdaconf-2015-web/blob/master/ex3-db.md). I strongly suggest to work out at least tasks #1 to #3 before continuing reading.

... don't worry, I won't go anywhere while you do exercise 3 ...

OK, let's move on. In this exercise I will present the [Esqueleto](http://hackage.haskell.org/package/esqueleto) library, a wonderful companion to Persistent for SQL querying. Instead of a custom set of functions like Persistent, Esqueleto embeds the SQL language itself as a set of Haskell combinators. In that way, you can reuse your knowledge of SQL to write more powerful queries.

Apart from the useful SQL dressing, there is a reason why you might need Esqueleto in your web application. Persistent works with a wide range of database systems, including both SQL systems such as MySQL, PostgreSQL or Sqlite, but algo NoSQL software such as MongoDB. As a result, the ways to query and interact with a database are restricted to those supported by all of them. Esqueleto only works on SQL databases, which means that it's possible to use this extra power on queries and changes to the database.

## Preliminaries

Esqueleto gives a superset of Persistent functionality. To start using it without conflicts with the previous Persistent module, you should change the imports in the top of the code from

```haskell
import Database.Persist
```

to the corresponding statement

```haskell
import Database.Esqueleto
```

Note that only `Database.Persist` is to be changed. The rest of imports, such as `Database.Persist.TH` shall remain the same.

## Simple queries

Let's start by translating the queries for obtaining users by name to Esqueleto. We move from:

```haskell
selectList ([UserFirstName ==. name] ||. [UserLastName ==. name]) []
```

to a initially longer version:

```haskell
select $ from $ \user -> do
where_ (user ^. UserFirstName ==. val name ||. user ^. UserLastName ==. val name)
return client
```

For those who use or have used SQL in the past, note the similarities with:

```sql
SELECT * FROM Users WHERE FistName = name OR LastName = name
```

In order to understand what is going on, dissecting the previous code is a good way to move forward. First of all we find the operation, in this case `select`, which informs that the system should use the next expression to perform a *query* over the database. Other available operations are `selectDistinct`, `update`, `insertSelect` and `delete`.

The next element to specify is the entity (or entities, as we shall we later) we want to work with. We do so by bringing into scope variables which represent a record in that table, in this case `user`. Still, you might be wondering how the `User` is chosen, instead of `Task`. The answer lies in the types: the entity is chose by the type of that variable. Fortunately, thanks to type inference, we don't need to write the type manually.

From the combinator `from` on we enter a monadic scope where each statement is ultimately going to be translated into SQL. In our case we find a `where_` operation, which decides whether to include or not a record in the query. These restrictions work a bit differently from Persistent, though: instead of a list we need to combine the constraints using either `(&&.)` or `(||.)`, corresponding to conjunction and disjunction, respectively. Also, the conditions need to specify from which record we work, and the field to be accessed using `(^.)`, as the example shows.

Finally, we declare which information to return as result to the query. In this case it is the complete record, but we could also return a projection of it. For example, say that we only want the last name in the previous query, we modify the last line to:

```haskell
return (user ^. UserLastName)
```

**Task #1**: rewrite the other route which gives a paginated list of users by name using Esqueleto. The [documentation](http://hackage.haskell.org/package/esqueleto-2.2/docs/Database-Esqueleto.html) should be helpful, especially the part of the tutorial which tells how to use `orderBy`, and the functions `limit` and `offsetBy`.

## Joins

Up to now, we haven't seen anything new from Persistent to Esqueleto, apart from the SQL-esque syntax. The action comes when we introduce the joining combinators for `from`, something which we are going to discuss right now!

Take the task #3 from the previous exercise: we want to obtain all tasks, including the information from their users. In order to do that, we needed a round-trip from the database to the application: we first obtained all tasks, and then for each one we obtain its user. Using Esqueleto we can do all of that in only one go:

```haskell
select $ from $ \(user `InnerJoin` task) -> do
on (user ^. UserId ==. task ^. TaskUser)
orderBy [ asc ]
return (task, user)
```

Instead of one entity, we declare our query to work on the *join* of `User` and `Task` via the `InnerJoin` combinator. Then, we are free to use all normal Esqueleto operations, plus a new one `on`, which restricts which pairs of records are returned together. In this case, this constraint is the user identifier recorded in the task to be the same to the identifier of the user record.

Using Esqueleto we can also perform aggregation. For example, we can ask for the number of tasks each user has in the database. In order to do so we need to perform a *grouping* of tasks by user, and count the number of elements in each group.

```haskell
select $ from $ \(user `LeftOuterJoin` task) -> do
on (user ^. UserId ==. task ^. TaskUser)
groupBy (user ^. UserId)
return (user, countRows)
```

Note a small change from the previous code: we moved from `InnerJoin` to `LeftOuterJoin`. The reason is a bit technical, but boils down to the fact that with an inner join those users without any task won't be returned, and we want it to happend with a count of 0.

**Task #2**: try to rewrite all queries, updates and deletions from the previous exercise into Esqueleto.
