data = document.querySelector("body>pre").innerText.split("\n\n");

parse = ([rules, strings]) => ({
  rules: rules
    .split("\n")
    .map((str) => ({ .../^(?<num>\d+): (?<rest>.*)$/.exec(str).groups }))
    .reduce(
      ({ composite, final }, { num, rest }) => {
        return rest.match(/"[a-b]"/)
          ? { composite, final: { ...final, [num]: rest.replace(/"/g, "") } }
          : {
              composite: {
                ...composite,
                [num]: rest
                  .split(" | ")
                  .map((str) => str.split(" ").map((str) => parseInt(str))),
              },
              final,
            };
      },
      { composite: {}, final: {} }
    ),
  strings: strings.split("\n").slice(0, -1),
});

rulesToRegex = (rules, ruleId = 0, recursiveRuleDepth = 0) => {
  let rule = rules.composite[ruleId];
  if (rule) {
    return `(${rule
      .map((or) => {
        let parts = or
          .filter((id) => id !== ruleId)
          .map((id) => rulesToRegex(rules, id, recursiveRuleDepth));
        if (or.includes(ruleId)) {
          if (parts.length == 1) {
            parts[0] += "+";
          } else {
            parts = [
              [...Array(recursiveRuleDepth).keys()]
                .map(
                  (n) =>
                    `(${parts.map((part) => part + `{${n + 1}}`).join("")})`
                )
                .join("|"),
            ];
          }
        }
        return parts.join("");
      })
      .join("|")})`;
  } else {
    return rules.final[ruleId];
  }
};

compute = (data, overrides = {}, recursiveRuleDepth = 0) => {
  let { rules, strings } = parse(data);
  Object.keys(overrides).forEach(
    (key) => (rules.composite[key] = overrides[key])
  );
  let amount = 0;
  let prevAmount = 0;
  do {
    prevAmount = amount;
    let regexStr = rulesToRegex(rules, 0, recursiveRuleDepth++);
    let rex = new RegExp(`^${regexStr}$`);
    amount = strings.filter((str) => str.match(rex)).length;
  } while (prevAmount !== amount);
  return amount;
};

// P1
console.time("Part1");
part1 = compute(data);
console.timeEnd("Part1");
console.log(`> ${part1}`);
// P2
console.time("Part2");
part2 = compute(data, { 8: [[42, 8]], 11: [[42, 11, 31]] }, 1);
console.timeEnd("Part2");
console.log(`> ${part2}`);
