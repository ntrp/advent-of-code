data = document
  .querySelector("body>pre")
  .innerText.split("\n")
  .filter((str) => str.length > 0)
  .map((str) => str.split(""));

ACTIVE = "#";

genGrid = (x, y, z, w, cycles) => {
  return Array.from(Array(x + cycles * 2).keys()).map((_) =>
    Array.from(Array(y + cycles * 2).keys()).map((_) =>
      Array.from(Array(z + cycles * 2).keys()).map((_) =>
        Array.from(Array(w + cycles * 2).keys()).fill(0)
      )
    )
  );
};

initGrid = (data, cycles) => {
  let shift = cycles + 1;
  let width = data[0].length;
  let height = data.length;
  let grid = genGrid(width, height, 1, 1, cycles + 1);
  data.flat().forEach((val, idx) => {
    if (val === ACTIVE) {
      grid[shift + (idx % width)][shift + Math.floor(idx / width)][shift][
        shift
      ] = 1;
    }
  });
  return { grid, width, height, cycles, shift };
};

activeNeighbors = (grid, point) => {
  return (
    [-1, 0, 1]
      .flatMap((x) =>
        [-1, 0, 1].flatMap((y) =>
          [-1, 0, 1].flatMap((z) =>
            [-1, 0, 1].map(
              (w) =>
                grid[point[0] + x][point[1] + y][point[2] + z][point[3] + w]
            )
          )
        )
      )
      .reduce((prev, curr) => prev + curr, 0) -
    grid[point[0]][point[1]][point[2]][point[3]]
  );
};

computeCycle = ({ grid, width, height, shift }, cycle, four) => {
  let ops = [];
  let wmin = four ? shift - cycle : shift;
  let wmax = four ? shift + cycle : shift;
  for (let w = wmin; w <= wmax; w++) {
    for (let z = shift - cycle; z <= shift + cycle; z++) {
      for (let y = shift - cycle; y < shift + height + cycle; y++) {
        for (let x = shift - cycle; x < shift + width + cycle; x++) {
          let neighbors = activeNeighbors(grid, [x, y, z, w]);
          let curr = grid[x][y][z][w];
          if ((curr == 1 && neighbors < 2) || neighbors > 3) {
            ops.push({ x, y, z, w, val: 0 });
          } else if (curr == 0 && neighbors == 3) {
            ops.push({ x, y, z, w, val: 1 });
          }
        }
      }
    }
  }
  ops.forEach(({ x, y, z, w, val }) => (grid[x][y][z][w] = val));
};

compute = (data, cycles, four) => {
  let state = initGrid(data, cycles);
  for (let i = 0; i < cycles; ) {
    computeCycle(state, ++i, four);
  }
  return state.grid
    .flat()
    .flat()
    .flat()
    .reduce((prev, curr) => prev + curr, 0);
};

// P1
console.time("Part1");
part1 = compute(data, 6);
console.timeEnd("Part1");
console.log(`> ${part1}`);
// P2
console.time("Part2");
part2 = compute(data, 6, true);
console.timeEnd("Part2");
console.log(`> ${part2}`);
