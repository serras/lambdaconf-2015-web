# Exercise 6 - Deployment on Heroku

In contrast to other exercises, in this one we are not going to enhance the web application we have been developing, but rather deploy it to the cloud. In particular we are going to deploy it as a [Heroku](https://www.heroku.com/) application using the fantastic [GHC buildpack](https://github.com/begriffs/heroku-buildpack-ghc) (I am not affiliated to any of them, but still think it is one of the easiest ways to deploy Haskell applications).

The first steps are [signing up](https://signup.heroku.com) and installing the [Heroku Toolbelt](https://toolbelt.heroku.com/) in your platform. The last step is platform-dependent, but usually involves just running an executable. Once you have installed the Toolbelt, you need to log-in for the first time, issuing

```
$ heroku login
```

on a terminal. After this step, you will be asked for your credentials no more: it will be assumed that you always deploy as that user.

Heroku uses Git as a way to communicate when an application should be updated. In other words, updating the deployed code is done via a simple `git push` command. This implies that the code you want to deploy needs to live inside a Git repository. My recommendation for this exercise is that you take one of the initial exercises or examples which do not involve database access and build a new Git repository over it.

For example, you can copy the `p2-html` folder in a new place, and execute `git init` on it. Then, add all the files that make up the application, and make an initial commit. Usually this involves running:

```
$ cd route/to/your/app
$ git init
$ git add *.cabal Setup.hs src
$ git commit -m "Initial commit"
```

However, this code is not yet ready to be deployed. The reason is that it has a hard-coded port number in its code (in this case, 8080). When running inside Heroku, you need to make your application listen to the port given in the `PORT` environment variable. In order to make this happen, you need to change some code in your `Main.hs` source file. First of all, you need to add a new import:

```haskell
import System.Environment
```

and then change the `main` function to read the environment variable and pass it to Spock:

```haskell
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  runSpock port $ spockT id $ ...
```

Note that you may need to reindent the `do` block that described the routes in order for the code to be parsed.  Finally, we need to create a `Procfile`. This file tells Heroku which is the code to be run when the application needs to be started. To get started, just write the following:

```json
web: cabal run
```

We can commit all these new change in our repository:

```
$ git add src/Main.hs Procfile
$ git commit -m "Read PORT env variable, add Procfile"
```

It is now time to create a new Heroku application! It is very simple, just run:

```
$ heroku create -b https://github.com/begriffs/heroku-buildpack-ghc.git
Creating morning-forest-9773... done, stack is cedar-14
```

The last part of the command specifies a *buildpack*, a set of instructions that tell Heroku how to deploy code on a specific platform. There are buildpacks for most mainstream programming languages: Java, Ruby, Erlang... As an output of the application creation process, you get back a random name for your new application. There are ways to change it, but we won't stop there now.

As said before, deploying code to Heroku is done simply via a `git push` command. In particular, the Heroku Toolbelt configures a new remote `heroku` which is the one to be used. Thus, we need to run:

```
git push heroku master
```

The buildpack will perform its magic, download all dependencies from Cabal and finally compile your application. Note than Heroku imposes a 15 minutes build time for uploaded code, so if you have something bigger that our tiny application, it is quite common to surpass the limit. In such cases, the GHC buildpack recommends using an [Anvil cache](https://github.com/begriffs/heroku-buildpack-ghc#beating-the-fifteen-minute-build-limit) or moving towards the more featureful [Haskell on Heroku buildpack](https://haskellonheroku.com/tutorial/).

Finally you can see your application running by issuing the `heroku open` command. Remember to add whatever route is needed to make your Spock application respond. Et voilà, you have just deployed a Haskell web application in the cloud!

**Note:** if you need to use more Heroku services such a PostgreSQL or MongoDB database, it is worthy to look at the [`heroku`](http://hackage.haskell.org/package/heroku) and [`heroku-persistent`](http://hackage.haskell.org/package/heroku-persistent) packages, which take care of reading the database configuration as exposed by the Heroku API.
