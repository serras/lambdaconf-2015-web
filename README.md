# LambdaConf 2015 Haskell web workshop

### Preparing for the workshop

1. Install Haskell following the instructions of the [download](https://www.haskell.org/downloads) page.
2. Open a terminal.
3. Run `cabal update`.
4. Run `cabal install Spock blaze-html shakespeare aeson stm persistent-template persistent-sqlite esqueleto digestive-functors-blaze Spock-digestive digestive-functors-aeson wai-middleware-static`.
5. If you want to follow the deployment exercise, install [Heroku Toolbelt](https://toolbelt.heroku.com/).

### [Slides](https://docs.google.com/presentation/d/10m9zTl3Lir68VsT-yV_Ke-hvBBuq7Lj08WQ-yxElIKg/edit?usp=sharing)

### Exercises

1. [Returning JSON](https://github.com/serras/lambdaconf-2015-web/blob/master/ex1-json.md)
2. [Software Transactional Memory](https://github.com/serras/lambdaconf-2015-web/blob/master/ex2-stm.md) (advanced)
3. [Database](https://github.com/serras/lambdaconf-2015-web/blob/master/ex3-db.md)
4. [Esqueleto](https://github.com/serras/lambdaconf-2015-web/blob/master/ex4-esqueleto.md) (advanced)
5. [Digestive Functors](https://github.com/serras/lambdaconf-2015-web/blob/master/ex5-digestive.md)
6. [Deployment on Heroku](https://github.com/serras/lambdaconf-2015-web/blob/master/ex6-heroku.md)

### Running an exercise

1. Open a terminal.
2. Go to the folder corresponding to the exercise.
3. Execute `cabal install --only-dependencies`.
4. Execute `cabal build`.
5. The resulting program should be on the `dist` folder, inside a folder with the name of the exercise. You can run this program to start the web server. For example, the first exercise is run with `./dist/ex1-json/ex1-json`.
6. Open your browser of choice and point it to `http://localhost:8080`.
