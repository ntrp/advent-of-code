var loadData = require("./utils.cjs").loadData;
data = loadData(11)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => str.split(""));

normalization = {
  L: 0,
  "#": 1,
  ".": 0,
};

normalize = (map) => {
  let center = map.map((arr) => [
    normalization["."],
    ...arr.map((char) => normalization[char]),
    normalization["."],
  ]);
  return [
    Array(center[0].length).fill(normalization["."]),
    ...center,
    Array(center[0].length).fill(normalization["."]),
  ];
};

compute = (normalizedMap, reference) => {
  let mod = 0;
  let res = Array(normalizedMap.length)
    .fill()
    .map(() => Array(normalizedMap[0].length).fill(0));
  for (let y = 1; y < normalizedMap.length - 1; y++) {
    for (let x = 1; x < normalizedMap[0].length - 1; x++) {
      if (reference[y - 1][x - 1] == ".") {
        continue;
      }
      let sum = [-1, 0, 1]
        .flatMap((val) => [-1, 0, 1].map((val2) => [val, val2]))
        .filter(([dx, dy]) => !(dx == 0 && dy == 0))
        .map(([dx, dy]) => normalizedMap[y + dy][x + dx])
        .reduce((p, c) => p + c, 0);
      if (normalizedMap[y][x] == 0 && sum == 0) {
        res[y][x] = 1;
        mod++;
      } else if (normalizedMap[y][x] == 1 && sum >= 4) {
        res[y][x] = 0;
        mod++;
      } else {
        res[y][x] = normalizedMap[y][x];
      }
    }
  }
  return {
    mod,
    res,
  };
};

converge = (data, reference) => {
  let result = compute(data, reference);
  if (result.mod == 0) {
    return result.res.reduce(
      (prev, curr) => prev + curr.reduce((prev, curr) => prev + curr, 0),
      0
    );
  } else {
    return converge(result.res, reference);
  }
};

fetchPos = (data, y, x) => {
  if (x < 0 || x > data[0].length - 1 || y < 0 || y > data.length - 1) {
    return 0;
  }
  return data[y][x] == "#" ? 1 : data[y][x] == "L" ? 0 : undefined;
};

computeP2 = (map) => {
  let mod = 0;
  let res = Array(map.length)
    .fill()
    .map(() => Array(map[0].length).fill("."));
  for (let y = 0; y < map.length; y++) {
    for (let x = 0; x < map[0].length; x++) {
      if (map[y][x] == ".") {
        continue;
      }
      let dirs = Array(8);
      let i = 1;
      while (dirs.filter((val) => val !== undefined).length < 8) {
        dirs[0] = dirs[0] !== undefined ? dirs[0] : fetchPos(map, y - i, x - i);
        dirs[1] = dirs[1] !== undefined ? dirs[1] : fetchPos(map, y - i, x);
        dirs[2] = dirs[2] !== undefined ? dirs[2] : fetchPos(map, y - i, x + i);
        dirs[3] = dirs[3] !== undefined ? dirs[3] : fetchPos(map, y, x + i);
        dirs[4] = dirs[4] !== undefined ? dirs[4] : fetchPos(map, y + i, x + i);
        dirs[5] = dirs[5] !== undefined ? dirs[5] : fetchPos(map, y + i, x);
        dirs[6] = dirs[6] !== undefined ? dirs[6] : fetchPos(map, y + i, x - i);
        dirs[7] = dirs[7] !== undefined ? dirs[7] : fetchPos(map, y, x - i);
        i++;
      }
      sum = dirs.reduce((prev, curr) => prev + curr, 0);
      if (map[y][x] == "L" && sum == 0) {
        res[y][x] = "#";
        mod++;
      } else if (map[y][x] == "#" && sum >= 5) {
        res[y][x] = "L";
        mod++;
      } else {
        res[y][x] = map[y][x];
      }
    }
  }
  return {
    mod,
    res,
  };
};

convergeP2 = (data) => {
  let result = computeP2(data);
  if (result.mod == 0) {
    return result.res.reduce(
      (prev, curr) =>
        prev + curr.reduce((prev, curr) => prev + normalization[curr], 0),
      0
    );
  } else {
    return convergeP2(result.res);
  }
};

// P1
console.log(`Part1: ${converge(normalize(data), data)}`);
// P2
console.log(`Part2: ${convergeP2(data)}`);
