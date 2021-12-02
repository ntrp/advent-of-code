## Day 12: The N-Body Problem

The space near Jupiter is not a very safe place; you need to be careful of a [big distracting red spot](https://en.wikipedia.org/wiki/Great_Red_Spot) , extreme [radiation](https://en.wikipedia.org/wiki/Magnetosphere_of_Jupiter) , and a [whole lot of moons](https://en.wikipedia.org/wiki/Moons_of_Jupiter#List) swirling around. You decide to start by tracking the four largest moons: _Io_ , _Europa_ , _Ganymede_ , and _Callisto_ .

After a brief scan, you calculate the _position of each moon_ (your puzzle input). You just need to _simulate their motion_ so you can avoid them .

Each moon has a 3-dimensional position ( `x` , `y` , and `z` ) and a 3-dimensional velocity. The position of each moon is given in your scan; the `x` , `y` , and `z` velocity of each moon starts at `0` .

Simulate the motion of the moons in _time steps_ . Within each time step, first update the velocity of every moon by applying _gravity_ . Then, once all moons' velocities have been updated, update the position of every moon by applying _velocity_ . Time progresses by one step once all of the positions are updated.

To apply _gravity_ , consider every _pair_ of moons. On each axis ( `x` , `y` , and `z` ), the velocity of each moon changes by _exactly +1 or -1_ to pull the moons together. For example, if Ganymede has an `x` position of `3` , and Callisto has a `x` position of `5` , then Ganymede's `x` velocity _changes by +1_ (because `5 > 3` ) and Callisto's `x` velocity _changes by -1_ (because `3 < 5` ). However, if the positions on a given axis are the same, the velocity on that axis _does not change_ for that pair of moons.

Once all gravity has been applied, apply _velocity_ : simply add the velocity of each moon to its own position. For example, if Europa has a position of `x=1, y=2, z=3` and a velocity of `x=-2, y=0,z=3` , then its new position would be `x=-1, y=2, z=6` . This process does not modify the velocity of any moon.

For example, suppose your scan reveals the following positions:

Simulating the motion of these moons would produce the following:

    After 0 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 1 step:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 2 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 3 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 4 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 5 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 6 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 7 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 8 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 9 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 10 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

Then, it might help to calculate the _total energy in the system_ . The total energy for a single moon is its _potential energy_ multiplied by its _kinetic energy_ . A moon's _potential energy_ is the sum of the [absolute values](https://en.wikipedia.org/wiki/Absolute_value) of its `x` , `y` , and `z` position coordinates. A moon's _kinetic energy_ is the sum of the absolute values of its velocity coordinates. Below, each line shows the calculations for a moon's potential energy ( `pot` ), kinetic energy ( `kin` ), and total energy:

    Energy after 10 steps:
    pot: 2 + 1 + 3 =  6;   kin: 3 + 2 + 1 = 6;   total:  6 * 6 = 36
    pot: 1 + 8 + 0 =  9;   kin: 1 + 1 + 3 = 5;   total:  9 * 5 = 45
    pot: 3 + 6 + 1 = 10;   kin: 3 + 2 + 3 = 8;   total: 10 * 8 = 80
    pot: 2 + 0 + 4 =  6;   kin: 1 + 1 + 1 = 3;   total:  6 * 3 = 18
    Sum of total energy: 36 + 45 + 80 + 18 = 179

In the above example, adding together the total energy for all moons after 10 steps produces the total energy in the system, `_179_` .

Here's a second example:

Every ten steps of simulation for 100 steps produces:

    After 0 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 10 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 20 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 30 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 40 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 50 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 60 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 70 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 80 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 90 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    After 100 steps:
    pos=, vel=
    pos=, vel=
    pos=, vel=
    pos=, vel=

    Energy after 100 steps:
    pot:  8 + 12 +  9 = 29;   kin: 7 +  3 + 0 = 10;   total: 29 * 10 = 290
    pot: 13 + 16 +  3 = 32;   kin: 3 + 11 + 5 = 19;   total: 32 * 19 = 608
    pot: 29 + 11 +  1 = 41;   kin: 3 +  7 + 4 = 14;   total: 41 * 14 = 574
    pot: 16 + 13 + 23 = 52;   kin: 7 +  1 + 1 =  9;   total: 52 *  9 = 468
    Sum of total energy: 290 + 608 + 574 + 468 = 1940

_What is the total energy in the system_ after simulating the moons given in your scan for `1000` steps?

## Link

[https://adventofcode.com/2019/day/12](https://adventofcode.com/2019/day/12)
