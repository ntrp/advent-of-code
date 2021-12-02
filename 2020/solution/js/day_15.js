data = document
  .querySelector(".puzzle-input")
  .innerText.split(",")
  .map((str) => parseInt(str));

compute = (data, finalTurn) => {
  memory = {};
  for (let i = 0; i < data.length; i++) {
    memory[data[i]] = [i + 1];
  }
  let last = data[data.length - 1];
  let turn = data.length + 1;
  while (turn <= finalTurn) {
    if (memory[last].length <= 1) {
      num = 0;
    } else {
      num = memory[last][0] - memory[last][1];
    }
    last = num;
    let temp = memory[num];
    memory[num] = [turn];
    if (temp) {
      memory[num].push(temp[0]);
    }
    turn++;
  }
  return last;
};

console.time("solve");
// P1
console.log(`Part1: ${compute(data, 2020)}`);
// P2
console.log(`Part2: ${compute(data, 30_000_000)}`);
console.timeEnd("solve");
