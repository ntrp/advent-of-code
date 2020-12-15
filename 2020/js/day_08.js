data = document
  .querySelector("body>pre")
  .innerText.split("\n")
  .filter((str) => str.length > 0)
  .map((str) => ({ .../^(?<istr>\w+) (?<val>[+-]\d+)$/.exec(str).groups }));

compute = (swapIstr) => {
  let memory = new Set();
  let acc = 0;
  let i = 0;
  for (; i < data.length; ) {
    if (memory.has(i)) {
      return { i, acc };
    }
    memory.add(i);
    let istr = data[i].istr;
    if (swapIstr > -1 && i == swapIstr) {
      istr = istr == "nop" ? "jmp" : "nop";
    }
    switch (istr) {
      case "nop":
        i++;
        break;
      case "acc":
        acc += parseInt(data[i].val);
        i++;
        break;
      case "jmp":
        i += parseInt(data[i].val);
    }
  }
  return { i, acc };
};

computeFix = () => {
  let lastIdx = 0;
  let acc = 0;
  let trySwapIdx = -1;
  while (lastIdx < data.length && trySwapIdx < data.length) {
    trySwapIdx = data.findIndex(
      (obj, idx) => obj.istr.match(/(nop|jmp)/) && idx > trySwapIdx
    );
    let res = compute(trySwapIdx);
    lastIdx = res.i;
    acc = res.acc;
  }
  return acc;
};

// P1
console.log(`Part1: ${compute().acc}`);
// P2
console.log(`Part2: ${computeFix()}`);
