## Day 17: Conway Cubes

As your flight slowly drifts through the sky, the Elves at the Mythical Information Bureau at the North Pole contact you. They'd like some help debugging a malfunctioning experimental energy source aboard one of their super-secret imaging satellites.

The experimental energy source is based on cutting-edge technology: a set of Conway Cubes contained in a pocket dimension! When you hear it's having problems, you can't help but agree to take a look.

The pocket dimension contains an infinite 3-dimensional grid. At every integer 3-dimensional coordinate ( `x,y,z` ), there exists a single cube which is either _active_ or _inactive_ .

In the initial state of the pocket dimension, almost all cubes start _inactive_ . The only exception to this is a small flat region of cubes (your puzzle input); the cubes in this region start in the specified _active_ ( `#` ) or _inactive_ ( `.` ) state.

The energy source then proceeds to boot up by executing six _cycles_ .

Each cube only ever considers its _neighbors_ : any of the 26 other cubes where any of their coordinates differ by at most `1` . For example, given the cube at `x=1,y=2,z=3` , its neighbors include the cube at `x=2,y=2,z=2` , the cube at `x=0,y=2,z=3` , and so on.

During a cycle, _all_ cubes _simultaneously_ change their state according to the following rules:

- If a cube is _active_ and _exactly `2` or `3`_ of its neighbors are also active, the cube remains _active_ . Otherwise, the cube becomes _inactive_ .
- If a cube is _inactive_ but _exactly `3`_ of its neighbors are active, the cube becomes _active_ . Otherwise, the cube remains _inactive_ .

The engineers responsible for this experimental energy source would like you to simulate the pocket dimension and determine what the configuration of cubes should be at the end of the six-cycle boot process.

For example, consider the following initial state:

```
.#.
..#
###
```

Even though the pocket dimension is 3-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1 region of the 3-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given `z` coordinate (and the frame of view follows the active cells in each cycle):

```
Before any cycles:

z=0
.#.
..#
###


After 1 cycle:

z=-1
#..
..#
.#.

z=0
#.#
.##
.#.

z=1
#..
..#
.#.


After 2 cycles:

z=-2
.....
.....
..#..
.....
.....

z=-1
..#..
.#..#
....#
.#...
.....

z=0
##...
##...
#....
....#
.###.

z=1
..#..
.#..#
....#
.#...
.....

z=2
.....
.....
..#..
.....
.....


After 3 cycles:

z=-2
.......
.......
..##...
..###..
.......
.......
.......

z=-1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=0
...#...
.......
#......
.......
.....##
.##.#..
...#...

z=1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=2
.......
.......
..##...
..###..
.......
.......
.......
```

After the full six-cycle boot process completes, _`112`_ cubes are left in the _active_ state.

Starting with your given initial configuration, simulate six cycles. _How many cubes are left in the active state after the sixth cycle?_

## Part Two

For some reason, your simulated results don't match what the experimental energy source engineers expected. Apparently, the pocket dimension actually has _four spatial dimensions_ , not three.

The pocket dimension contains an infinite 4-dimensional grid. At every integer 4-dimensional coordinate ( `x,y,z,w` ), there exists a single cube (really, a _hypercube_ ) which is still either _active_ or _inactive_ .

Each cube only ever considers its _neighbors_ : any of the 80 other cubes where any of their coordinates differ by at most `1` . For example, given the cube at `x=1,y=2,z=3,w=4` , its neighbors include the cube at `x=2,y=2,z=3,w=3` , the cube at `x=0,y=2,z=3,w=4` , and so on.

The initial state of the pocket dimension still consists of a small flat region of cubes. Furthermore, the same rules for cycle updating still apply: during each cycle, consider the _number of active neighbors_ of each cube.

For example, consider the same initial state as in the example above. Even though the pocket dimension is 4-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1x1 region of the 4-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given `z` and `w` coordinate:

```
Before any cycles:

z=0, w=0
.#.
..#
###


After 1 cycle:

z=-1, w=-1
#..
..#
.#.

z=0, w=-1
#..
..#
.#.

z=1, w=-1
#..
..#
.#.

z=-1, w=0
#..
..#
.#.

z=0, w=0
#.#
.##
.#.

z=1, w=0
#..
..#
.#.

z=-1, w=1
#..
..#
.#.

z=0, w=1
#..
..#
.#.

z=1, w=1
#..
..#
.#.


After 2 cycles:

z=-2, w=-2
.....
.....
..#..
.....
.....

z=-1, w=-2
.....
.....
.....
.....
.....

z=0, w=-2
###..
##.##
#...#
.#..#
.###.

z=1, w=-2
.....
.....
.....
.....
.....

z=2, w=-2
.....
.....
..#..
.....
.....

z=-2, w=-1
.....
.....
.....
.....
.....

z=-1, w=-1
.....
.....
.....
.....
.....

z=0, w=-1
.....
.....
.....
.....
.....

z=1, w=-1
.....
.....
.....
.....
.....

z=2, w=-1
.....
.....
.....
.....
.....

z=-2, w=0
###..
##.##
#...#
.#..#
.###.

z=-1, w=0
.....
.....
.....
.....
.....

z=0, w=0
.....
.....
.....
.....
.....

z=1, w=0
.....
.....
.....
.....
.....

z=2, w=0
###..
##.##
#...#
.#..#
.###.

z=-2, w=1
.....
.....
.....
.....
.....

z=-1, w=1
.....
.....
.....
.....
.....

z=0, w=1
.....
.....
.....
.....
.....

z=1, w=1
.....
.....
.....
.....
.....

z=2, w=1
.....
.....
.....
.....
.....

z=-2, w=2
.....
.....
..#..
.....
.....

z=-1, w=2
.....
.....
.....
.....
.....

z=0, w=2
###..
##.##
#...#
.#..#
.###.

z=1, w=2
.....
.....
.....
.....
.....

z=2, w=2
.....
.....
..#..
.....
.....
```

After the full six-cycle boot process completes, _`848`_ cubes are left in the _active_ state.

Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. _How many cubes are left in the active state after the sixth cycle?_

## Link

[https://adventofcode.com/2020/day/17](https://adventofcode.com/2020/day/17)
