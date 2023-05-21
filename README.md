# pegthing-axialized

A modified version of the pegthing game introduced in
[Functional Programming chapter of Clojure for the Brave and True](http://www.braveclojure.com/functional-programming/#4__Peg_Thing_).

This version of the game uses axialized hex-grid coordinates in place of the triangular number system used in the original code. The axialized coordinate system and the map data structure allows for arbitrarily shaped boards, including boards with holes.

The axial coordinate system takes heavy inspiration from the [Hexagonal Grids](https://www.redblobgames.com/grids/hexagons/) article by Red Blob games.

To play, just run "lein run".
