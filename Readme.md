# Runge Phenomenon
It is well-known that on a compact subset every smooth function can be approximated by polynomials (in the supremum norm). This is called the [Stone-Weierstrass Approximation Theorem](https://en.wikipedia.org/wiki/Stone%E2%80%93Weierstrass_theorem). However it is not as well-known that we cannot simply take uniformly spaced interpolation points.
The Runge function $x\mapsto \frac{1}{25x^2+1}$ (defined on $[-1,1]$) is a simple, benign function for which increasing the number of interpolation points makes the approximation *worse*.

As an exercise for Elm and the [line-charts](https://package.elm-lang.org/packages/terezka/line-charts/2.0.1/) package, I decided to make a small demonstration.


