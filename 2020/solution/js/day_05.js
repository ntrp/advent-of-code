var loadData = require("./utils.cjs").loadData;
data = loadData(5)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => ({
    row: str
      .substr(0, 7)
      .split("")
      .reduce(
        (prev, curr) =>
          curr === "F"
            ? prev.slice(0, prev.length / 2)
            : prev.slice(prev.length / 2, prev.length),
        [...Array(128).keys()]
      )[0],
    col: str
      .substr(7, 3)
      .split("")
      .reduce(
        (prev, curr) =>
          curr === "L"
            ? prev.slice(0, prev.length / 2)
            : prev.slice(prev.length / 2, prev.length),
        [...Array(8).keys()]
      )[0],
  }))
  .map((data) => data.row * 8 + data.col)
  .sort((a, b) => a - b);

// P1
console.log(`Part1: ${data[data.length - 1]}`);
// P2
console.log(
  `Part2: ${data.find((num, idx, arr) => arr[idx + 1] !== num + 1) + 1}`
);
