data = document
  .querySelector("body>pre")
  .innerText.split("\n")
  .filter((str) => str.length > 0)
  .map((str) => str.split(" = "));

processDataP1 = ({ memory, base, mask }, [op, val]) => {
  if (op.startsWith("mask")) {
    mask = val.split("").reverse().join("");
  } else {
    let loc = parseInt(op.substr(4, op.length - 1));
    vala = parseInt(val)
      .toString(2)
      .padStart(mask.length, "0")
      .split("")
      .reverse();
    memory[loc] = parseInt(
      mask
        .split("")
        .map((char, idx) => (char == "X" ? vala[idx] : char))
        .reverse()
        .join(""),
      2
    );
  }
  return { memory, base, mask };
};

processDataP2 = ({ memory, base, mask }, [op, val]) => {
  if (op.startsWith("mask")) {
    mask = val.split("").reverse();
  } else {
    let loc = parseInt(op.substr(4, op.length - 1))
      .toString(2)
      .padStart(mask.length, 0)
      .split("")
      .reverse();
    maskedLoc = mask
      .map((char, idx) => (char == "0" ? loc[idx] : char))
      .reverse();
    let bits = maskedLoc.filter((str) => str === "X").length;
    let combs = [...Array(Math.pow(2, bits)).keys()].map((num) =>
      parseInt(num).toString(2).padStart(bits, 0).split("")
    );
    let locs = combs
      .map((comb) =>
        comb.reduce((prev, curr) => prev.replace("X", curr), maskedLoc.join(""))
      )
      .map((comb) => parseInt(comb, 2));
    for (let loc of locs) {
      memory[loc] = parseInt(val);
    }
  }
  return { memory, base, mask };
};

res = (data, fn) => {
  return data.reduce((prev, curr) => fn.apply(null, [prev, curr]), {
    memory: {},
    mask: "",
  });
};

// P1
console.log(
  `Part1: ${Object.values(res(data, processDataP1).memory).reduce(
    (prev, curr) => prev + curr,
    0
  )}`
);
// P2
console.log(
  `Part2: ${Object.values(res(data, processDataP2).memory).reduce(
    (prev, curr) => prev + curr,
    0
  )}`
);
