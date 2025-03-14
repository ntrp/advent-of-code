var loadData = require("./utils.cjs").loadData;
data = loadData(10)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => parseInt(str))
  .sort((a, b) => a - b);

compute = (data) => {
  let all = [0, ...data, data[data.length - 1] + 3];
  let counts = { 1: 0, 3: 0 };
  for (let i = 0; i < all.length - 1; i++) {
    counts[all[i + 1] - all[i]]++;
  }
  return counts[1] * counts[3];
};

cache = {};
getOrCache = (data, i) => {
  return (cache[i] = cache[i] || computeArrangments(data, i));
};

computeArrangments = (data, idx) => {
  if (idx >= data.length - 1) {
    return 1;
  }

  let i = idx + 1;
  let count = 0;
  while (data[i] - data[idx] <= 3) {
    count += getOrCache(data, i++);
  }
  return count;
};

// P1
console.log(`Part1: ${compute(data)}`);
// P2
console.log(`Part2: ${computeArrangments([0, ...data], 0)}`);
