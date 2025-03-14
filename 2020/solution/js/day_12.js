var loadData = require("./utils.cjs").loadData;
data = loadData(12)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => ({ .../(?<op>\w)(?<val>\d+)/.exec(str).groups }));

vectorMap = {
  E: [1, 0],
  N: [0, 1],
  W: [-1, 0],
  S: [0, -1],
};

rotate = (vec, ang) => {
  ang = -ang * (Math.PI / 180);
  let cos = Math.cos(ang);
  let sin = Math.sin(ang);
  return [
    Math.round(10000 * (vec[0] * cos - vec[1] * sin)) / 10000,
    Math.round(10000 * (vec[0] * sin + vec[1] * cos)) / 10000,
  ];
};

sum = (va, vb) => {
  return [va[0] + vb[0], va[1] + vb[1]];
};

mul = (v, k) => {
  return [v[0] * k, v[1] * k];
};

applyP1 = ({ pos, dir }, { op, val }) => {
  val = parseInt(val);
  switch (op) {
    case "F":
      return { pos: sum(pos, mul(dir, val)), dir };
    case "R":
    case "L":
      return { pos, dir: rotate(dir, val * (op == "R" ? 1 : -1)) };
    default:
      return { pos: sum(pos, mul(vectorMap[op], val)), dir };
  }
};

applyP2 = ({ pos, dir }, { op, val }) => {
  val = parseInt(val);
  let state;
  switch (op) {
    case "F":
      state = { pos: sum(pos, mul(dir, val)), dir };
      break;
    case "R":
    case "L":
      return { pos, dir: rotate(dir, val * (op == "R" ? 1 : -1)) };
    default:
      return { pos, dir: sum(dir, mul(vectorMap[op], val)) };
  }
  return state;
};

compute = (data, fn, dir) => {
  return data.reduce((prev, curr) => fn.apply(null, [prev, curr]), {
    pos: [0, 0],
    dir,
  });
};

// P1
console.log(
  `Part1: ${compute(data, applyP1, [1, 0]).pos.reduce(
    (a, b) => a + Math.abs(b),
    0
  )}`
);
// P2
console.log(
  `Part2: ${compute(data, applyP2, [10, 1]).pos.reduce(
    (a, b) => a + Math.abs(b),
    0
  )}`
);
