# Exercise 3 - Database

This exercise helps you in getting more acquainted with the Persistent library. The basis for the exercise is the Tasks web application that we have been looking at so far: your goal is to code all the parts related to tasks themselves.

The code for this exercises includes placeholders in `Main.hs` for the new routes that are to be added. The [documentation](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html) for the `persistent` library will come handy in these tasks.

### Task 1 - Create a new task

First, write the code for the `/task/new/:userId/:title` which adds a new task to the database with the given title and related to the given user. Note that you need to check that the user exists prior to inserting the record in the database.

The response in case of error should contain a sensible error code. When the task is correctly inserted in the database, return a JSON representation of the record including its given identifier.

### Task 2 - Select task by user

Implement the code for the `/task/by-user/:userId` route, which returns all the tasks for a given user. Those tasks must be ordered by title. Return simply the titles as a JSON array, or the error code 404 if the given user does not exist.

**Extra**: implement a version of this route suitable for pagination. That is, including extra parameters which tell the initial offset and the number of records to return from the query.

### Task 3 - Show all tasks, including their users

The router `/task/all` should return the information from all the tasks in the system. The result must be a list of either JSON or HTML data in the form:

```
{ "id"    : 1
, "title" : "This is the title"
, "user"  : { "id"    : 1
            , "first" : "Alejandro"
            , "last"  : "Serrano" } }
```

Note that a `Task` value only stores an `User` key, not the entire value, so you would need to recover all that user information using extra queries. The following function will be helpful in treating lists in a monadic way:

```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
```

**Extra**: if you think that Persistent does not give you the right tools for a performant implementation, then you are right: Persistent does not allow doing joins in the database. For information about how to solve this problem, look at exercise 4.

### Task 4 - Add a `completed` field

The next task will require that you change both `Db.hs` and `Main.hs`. Being more concrete, the task is adding a new piece of data to a task, which informs whether the task is yet to be completed or it is already finished. Thus, as a first step you will need to change the database schema to include an extra Boolean field.

The initial state for a task should be not completed, so the code you wrote for task #1 should be changed to reflect this fact. Afterwards, a call to `/task/completed/:taskId` shall change this state to completed.

In order to implement this state change, Persistent gives you two different options. My suggestion is to write both pieces of code, and compare them in terms of readability and performance:

* The first option is using the [`replace`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:replace) function. The replacement operation takes a key in the database and the new value which should be associated to that key. That is, you need to obtain the information for the entire task, and then perform a replacement using the same information with the corresponding field changed.
* Another possibility is using [`update`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:update). In this case, instead of completely new value, you tell Persistent which are the updates to be done in the record which matches a given key.

### Task 5 - Remove a user

Sometimes people need to start a new life, and sadly that life might not involve our task web application at all. To break with the past, they need to be deleted from our records.

When an user surfs to `/user/delete/:userId`, the aim is to remove it from the database. But of course you do not want orphan tasks, so you first need to remove all of them too. In this task you are asked to do this in no less than three different ways:

* The first one is doing a query to get all tasks whose owner is the given user, and then call [`delete`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:delete) on each of them. Note that, given that `delete` only requires the identifier of the task to delete, you can change from using `selectList` to [`selectKeys`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:selectKeys).
* Anther possibility is to delete all the tasks with a given user in bulk, by means of the [`deleteWhere`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:deleteWhere) function. This function is similar to `selectList`, but instead of returning the records that match the data, it deletes them.
* Finally, you may perform a *cascade delete*, which removes a value, in this case the user, with all the associated data from other entities. The corresponding function in Persistent is [`deleteCascade`](http://hackage.haskell.org/package/persistent-2.1.3/docs/Database-Persist-Class.html#v:deleteCascade).
