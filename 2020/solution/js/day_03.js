var loadData = require("./utils.cjs").loadData;
data = loadData(3)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => str.split(""));

compute = (pattern) => {
  return pattern
    .map((cfg) => {
      let y = 0,
        x = 0,
        c = 0;
      do {
        y += cfg[1];
        x = (x + cfg[0]) % data[0].length;
        if (y < data.length && data[y][x] === "#") {
          c++;
        }
      } while (y < data.length);
      return c;
    })
    .reduce((a, b) => a * b);
};

// P1
console.log(`Part1: ${compute([[3, 1]])}`);
// P2
console.log(
  `Part2: ${compute([
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2],
  ])}`
);
