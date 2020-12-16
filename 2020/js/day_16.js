data = document
  .querySelector("body>pre")
  .innerText.split("\n\n")
  .filter((str) => str.length > 0);

parse = ([ranges, ticket, tickets]) => ({
  ranges: ranges
    .split("\n")
    .map((str) => ({
      .../^(?<cat>[^:]+): (?<r1min>\d+)-(?<r1max>\d+) or (?<r2min>\d+)-(?<r2max>\d+)$/.exec(
        str
      ).groups,
    }))
    .map(({ cat, r1min, r1max, r2min, r2max }) => ({
      cat,
      r: [...range(r1min, r1max), ...range(r2min, r2max)],
    })),
  ticket: ticket
    .split("\n")[1]
    .split(",")
    .map((str) => parseInt(str)),
  tickets: tickets
    .split("\n")
    .slice(1)
    .filter((str) => str.length > 0)
    .map((str) => str.split(",").map((str) => parseInt(str))),
});

function range(start, end) {
  start = parseInt(start);
  end = parseInt(end);
  return Array(end - start + 1)
    .fill()
    .map((_, idx) => start + idx);
}

computeValidNum = (ranges, num) => {
  return ranges
    .map(({ r }) => r.includes(num))
    .reduce((prev, curr) => prev || curr, false);
};

computeP1 = (data) => {
  let { ranges, tickets } = parse(data);
  return tickets
    .map((ticket) =>
      ticket.reduce((prev, num) => {
        return computeValidNum(ranges, num) ? prev : prev + num;
      }, 0)
    )
    .reduce((prev, curr) => prev + curr, 0);
};

powerOf2 = (n) => {
  return n && (n & (n - 1)) === 0;
};

computeP2 = (data) => {
  let { ranges, ticket, tickets } = parse(data);
  let arr = [
    ticket,
    ...tickets.filter((ticket) =>
      ticket.reduce((prev, curr) => prev && computeValidNum(ranges, curr), true)
    ),
  ]
    .map((ticket) =>
      ticket
        .map((num) =>
          ranges
            .flatMap(({ r }) => (r.includes(num) ? [1] : [0]))
            .reverse()
            .join("")
        )
        .map((str) => parseInt(str, 2))
    )
    .reduce((prev, curr) =>
      prev ? prev.map((val, idx) => val & curr[idx]) : curr
    );

  let mem = {};
  let stack = [];
  for (let i = 0; i < arr.length; i++) {
    if (powerOf2(arr[i])) {
      stack.push(arr[i]);
    }
  }
  while (stack.length > 0) {
    let cat = stack.pop();
    mem[cat] = 1;
    for (let p = 0; p < arr.length; p++) {
      if (!powerOf2(arr[p])) {
        arr[p] = cat ^ arr[p];
        if (powerOf2(arr[p]) && !mem[arr[p]]) {
          stack.push(arr[p]);
        }
      }
    }
  }
  categoryMap = arr.map((num) => Math.log2(num));
  return ranges
    .reduce(
      (prev, { cat }, idx) =>
        cat.startsWith("departure") ? [...prev, idx] : prev,
      []
    )
    .map((idx) => ticket[categoryMap.indexOf(idx)])
    .reduce((prev, curr) => prev * curr, 1);
};

// P1
console.time("Part1");
part1 = computeP1(data);
console.timeEnd("Part1");
console.log(`> ${part1}`);
// P2
console.time("Part2");
part2 = computeP2(data);
console.timeEnd("Part2");
console.log(`> ${part2}`);
