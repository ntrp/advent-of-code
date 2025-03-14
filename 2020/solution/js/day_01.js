var loadData = require("./utils.cjs").loadData;
data = loadData(1)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => parseInt(str))
  .sort((a, b) => a - b);

compute = (prev, n) => {
  if (n > 1) {
    let find;
    data.find((num) => {
      return (find = compute([...prev, num], n - 1));
    });
    return find;
  }

  end = data.find((num) => [...prev, num].reduce((a, b) => a + b) == 2020);
  return end ? [...prev, end].reduce((a, b) => a * b) : 0;
};

// P1
console.log(`Part1: ${compute([], 2)}`);
// P2
console.log(`Part2: ${compute([], 3)}`);
