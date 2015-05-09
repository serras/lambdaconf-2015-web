# Exercise 5 - Digestive Functors

After more than an hour working hard, we are starting to have something which looks like a web application! In this exercise we are going to extend it from user registration to task creation. We are going to do so step by step, using the power of Digestive Functors.

### Task 1 - Add a task with user identifier

Using the same ideas as for user registration, you ought to develop a form which requests the information about a new task to be created and saves it into the database. Note that in the database schema there is now a `completed` field, as developed in Exercise 3. Since this new field is Boolean, it's better that you show it using some kind of checkbox or radio buttons. You can surf the [`digestive-functors` ](http://hackage.haskell.org/package/digestive-functors-0.8.0.0/docs/Text-Digestive-Form.html) and [`digestive-functors-blaze`](http://hackage.haskell.org/package/digestive-functors-blaze-0.6.0.6/docs/Text-Digestive-Blaze-Html5.html) documentation for inspiration.

There is something we are going to keep simple for the time being. A `Task` needs a `UserId` to be saved on the database. Normally, you would not ask the user to directly input an identifier, but in this first iteration of the form this is what you should do. Of course, remember to include some sort of validation which ensures that the identifier corresponds to an actual record in the database.

### Task 2 - Show all tasks by user

Once the number of tasks in the database starts growing, looking at all of them at once becomes a bit overwhelming. For that reason, our next goal is developing a simple page in which you can select the name of an user from a list, and that would make the tasks associated to that user to appear. The chosen route for this feature is `/tasks`.

The new part in this task is how to implement the list of users.  Note the [`choice` function](http://hackage.haskell.org/package/digestive-functors-0.8.0.0/docs/Text-Digestive-Form.html#v:choice) in the Digestive Functors library:

```haskell
choice :: (Eq a, Monad m, Monoid v) => [(a, v)] -> Formlet v m a
```

We supply it with a list of key-value pairs, and it generates a field which provides one of the keys. In our case, the keys should be the user identifier, and the values should be the name of each user. Of course, to generate the list of key-value pairs you need to perform a prior request to the database.

Once the form is submitted and Spock gets it via `POST /tasks`, you just need to query the database and show the titles of the corresponding tasks. Optionally, you can include again the list of users so that the person using your web application doesn't need to go back to the previous page.

### Task 3 - Add a task by choosing an user

The final task involves combining the two previous ones, in order to get a better task creation interface. Once again, you need to create a simple form to input the title of a new task and its associated user. However, instead of an identifier, it should be possible to choose an user from a dropdown list.

## Creating a REST+JSON API

Up to now, the advantages of separating the definition of the `Form` from its `View` might not be apparent. However, if you decide to support a new way of interacting with your data, such as a JSON API, you can reuse all the validation logic from a `Form`. Furthermore, many connectors exist from Digestive Functors to other libraries, making it even easier to swap between them.

In this last section I propose you to provide a REST API which communicates using JSON. We assume that some front-end application generates the information as a JSON value and gives it in a POST request to your application. This happens, for example, when your front-end application using AJAX-like technologies. Providing this kind of API allows seamless integration with other front-end technologies such as [PureScript](http://www.purescript.org/) or [ClojureScript](https://github.com/clojure/clojurescript).

The goal is to provide the code for the `/json/user/new` and `/json/task/new` routes, using the [`digestive-functors-aeson` library](http://hackage.haskell.org/package/digestive-functors-aeson-1.1.14/docs/Text-Digestive-Aeson.html). Note that you don't need to parse the JSON yourself, as hinted in that documentation, since Spock provides [`jsonBody`](http://hackage.haskell.org/package/Spock-0.7.9.0/docs/Web-Spock-Shared.html#v:jsonBody) for this same task.
