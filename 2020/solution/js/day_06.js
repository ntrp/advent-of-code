var loadData = require("./utils.cjs").loadData;
data = loadData(6)
  .split("\n\n")
  .map((str) => str.trim());

// P1
console.log(
  `Part1: ${data
    .map((str) => new Set([...str.replaceAll(/\n/g, "")]).size)
    .reduce((prev, curr) => prev + curr, 0)}`
);
// P2
console.log(
  `Part2: ${data
    .map((str) => ({
      people: str.split("\n").length,
      chars: [...str.replaceAll(/\n/g, "")].reduce(
        (prev, curr) => ({ ...prev, [curr]: (prev[curr] || 0) + 1 }),
        {}
      ),
    }))
    .map(
      (data) =>
        Object.values(data.chars).filter((val) => val === data.people).length
    )
    .reduce((prev, curr) => prev + curr, 0)}`
);
