var loadData = require("./utils.cjs").loadData;
data = loadData(18)
  .split("\n")
  .filter((str) => str.length > 0);

OPERATIONS = {
  "+": (acc, num) => acc + num,
  "-": (acc, num) => acc - num,
  "*": (acc, num) => acc * num,
  "/": (acc, num) => acc / num,
};

LEFT = 1;
RIGHT = 0;

OP_ASSOCIATIVITY = {
  "+": LEFT,
  "-": LEFT,
  "*": LEFT,
  "/": LEFT,
};

parser = (expression, priority, associativity) => {
  let tokens = expression.split("").filter((str) => str !== " ");
  let output = [];
  let operator = [];
  for (let token of tokens) {
    if (token.match(/[0-9]/)) {
      output.push(token);
    } else if (Object.keys(priority).includes(token)) {
      while (
        (operator.length > 0 &&
          priority[operator[operator.length - 1]] > priority[token]) ||
        (priority[operator[operator.length - 1]] === priority[token] &&
          associativity[token] === LEFT &&
          !operator[operator.length - 1].match(/\(/))
      ) {
        output.push(operator.pop());
      }
      operator.push(token);
    } else if (token.match(/\(/)) {
      operator.push(token);
    } else if (token.match(/\)/)) {
      while (operator[operator.length - 1] !== "(") {
        output.push(operator.pop());
      }
      if (operator[operator.length - 1] === "(") {
        operator.pop();
      }
    }
  }
  while (operator.length > 0) {
    output.push(operator.pop());
  }
  return output;
};

evaluator = (comp, priority) => {
  let stack = [];
  for (let token of comp) {
    if (token.match(/[0-9]+/)) {
      stack.push(parseInt(token));
    } else if (Object.keys(priority).includes(token)) {
      let a = stack.pop();
      let b = stack.pop();
      stack.push(OPERATIONS[token].apply(null, [a, b]));
    }
  }
  return stack.pop();
};

compute = (data, priority, associativity) => {
  return data
    .map((str) => parser(str, priority, associativity))
    .map((stack) => evaluator(stack, priority))
    .reduce((prev, curr) => prev + curr, 0);
};

// P1
console.time("Part1");
part1 = compute(data, { "+": 1, "*": 1 }, OP_ASSOCIATIVITY);
console.timeEnd("Part1");
console.log(`> ${part1}`);
// P2
console.time("Part2");
part2 = compute(data, { "+": 2, "*": 1 }, OP_ASSOCIATIVITY);
console.timeEnd("Part2");
console.log(`> ${part2}`);
