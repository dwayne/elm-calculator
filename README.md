# Calculator

![A screenshot of the Calculator](/calculator.png)

A calculator to add, subtract, multiply and divide rational numbers.

This Elm app is based on [freeCodeCamp](https://www.freecodecamp.com/)'s
[Build a JavaScript Calculator](https://learn.freecodecamp.org/front-end-libraries/front-end-libraries-projects/build-a-javascript-calculator/)
front-end project. Its look and feel is "borrowed" from
[this example](https://codepen.io/freeCodeCamp/full/wgGVVX) CodePen app.

## Thoughts

I enjoyed thinking my way through this project. Union types, data abstraction
with modules and [elm-test](https://github.com/elm-explorations/test) all
helped to make implementing the calculator's logic manageable.

I had two major goals I wanted to accomplish when building this application.

1. To have a clean separation between the UI and the application logic.
2. To have an example where tests (in spite of types) are helpful in catching
bugs.

I think I accomplished both but you can be the judge.

With respect to the first goal I did it by starting with the right abstraction.
If you follow the commits from when I
[added addition and subtraction](https://github.com/elm-school/calculator/commit/21b89460ee169de1f30efd66b327bb299852688f)
to when I finally [made decimal input work](https://github.com/elm-school/calculator/commit/1e0785589a3b2f5ff559b1c1617038df199f9cb7)
you'd see the evolution of the abstraction and the minor changes I needed to
make to the UI.

With respect to the second goal I don't think I could have maintained my sanity
without the tests. Don't get me wrong, types helped a ton but they couldn't
guarantee the correctness of some of the algorithms I needed to write.

For e.g. in my rational to decimal representation logic I had a bug where
`-1/3` was being represented as `0.(3)` when it should have been `-0.(3)`. A
test caught that, see [here](https://github.com/elm-school/calculator/blob/f55790975ee68167f762da318f23c301be982b2c/tests/Test/Calculator.elm#L282).
And, I was able to [fix](https://github.com/elm-school/calculator/commit/87c49787dea873d1897317ffce799f6ae5b76586)
it soon afterwards.

## Takeaways

1. [Make data structures](https://www.youtube.com/watch?v=x1FU3e0sT1I).
2. Types are great but tests are still needed for complex application logic or
algorithms. There's a myth in the community that once it compiles it works.
But I think that that's a bad expectation to set.
3. Union types are really great for domain modeling.
4. You can learn a lot with a well chosen project.

   > "In science, one can learn
the most by studying what seems the least." ~ Marvin Minsky, Society of Mind

## That's all folks!

Check out the [demo](https://elm-school.github.io/calculator/).
