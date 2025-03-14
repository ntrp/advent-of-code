var loadData = require("./utils.cjs").loadData;
data = loadData(7)
  .split("\n")
  .filter((str) => str.length > 0)
  .map((str) => ({
    .../^(?<bag>[a-z]+ [a-z]+) bags contain(?<rest>( (\d+ \w+ \w+|no other) bags?,?)+)/g.exec(
      str
    ).groups,
  }))
  .reduce((prev, curr) => ({ ...prev, [curr.bag]: curr.rest }), {});

computeParents = (bagName) => {
  parents = Object.entries(data)
    .filter(([, rest]) => rest.indexOf(bagName) > -1)
    .map(([bag]) => bag);
  if (parents.length > 0) {
    return new Set([
      ...parents,
      ...parents
        .map((parent) => computeParents(parent))
        .reduce((prev, curr) => [...prev, ...curr], []),
    ]);
  }
  return [];
};

computeAllBags = (bagName) => {
  return Object.entries(
    data[bagName]
      .split(",")
      .map((str) => ({
        .../(?<weight>\d+) (?<destination>\w+ \w+)/.exec(str)?.groups,
      }))
      .filter((obj) => !!obj.destination)
      .reduce(
        (prev, curr) => ({
          ...prev,
          [curr.destination]: parseInt(curr.weight),
        }),
        {}
      )
  )
    .map(([bag, num]) => num * computeAllBags(bag))
    .reduce((a, b) => a + b, 1);
};

// P1
console.log(`Part1: ${computeParents("shiny gold").size}`);
// P2
console.log(`Part2: ${computeAllBags("shiny gold") - 1}`);
