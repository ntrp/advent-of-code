data = document
  .querySelector("body>pre")
  .innerText.split("\n")
  .filter((str) => str.length > 0)
  .map((str) => {
    let [policy, char, pwd] = str.split(" ");
    char = char.replace(":", "");
    let [low, high] = policy.split("-");
    return { low, high, char, pwd };
  });

// P1
console.log(
  `Part1: ${
    data.filter((cfg) => {
      let matches = cfg.pwd.match(new RegExp(cfg.char, "g"));
      return matches && matches.length >= cfg.low && matches.length <= cfg.high;
    }).length
  }`
);
// P2
console.log(
  `Part2: ${
    data.filter(
      (cfg) =>
        (cfg.pwd.split("")[cfg.low - 1] === cfg.char) +
          (cfg.pwd.split("")[cfg.high - 1] === cfg.char) ===
        1
    ).length
  }`
);
