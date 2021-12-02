data = document
  .querySelector("body>pre")
  .innerText.split("\n\n")
  .map((str) => str.replaceAll(/\n/g, " ").split(" ").sort());

computeMandatoryFields = (arr) => {
  return arr.filter(
    (arr) =>
      arr.length == 8 ||
      (arr.length == 7 && arr[0].startsWith("byr") && arr[1].startsWith("ecl"))
  );
};

range = (num, min, max) => {
  return parseInt(num) >= min && parseInt(num) <= max;
};

validationMap = {
  byr: (val) => range(val, 1920, 2002),
  iyr: (val) => range(val, 2010, 2020),
  eyr: (val) => range(val, 1920, 2030),
  hgt: (val) =>
    val.match(/^[0-9]+cm$/)
      ? range(val, 150, 193)
      : val.match(/^[0-9]+in/) && range(val, 59, 76),
  hcl: (val) => val.match(/^#[0-9a-f]{6}$/),
  ecl: (val) => val.match(/^(amb|blu|brn|gry|grn|hzl|oth)$/),
  pid: (val) => val.match(/^[0-9]{9}$/),
  cid: () => true,
};

computeValidFields = (arr) => {
  return arr.filter((pass) =>
    pass.reduce(
      (prev, curr) =>
        prev &&
        validationMap[curr.split(":")[0]]?.call(null, curr.split(":")[1]),
      true
    )
  );
};
// P1
console.log(`Part1: ${computeMandatoryFields(data).length}`);
// P2
console.log(
  `Part2: ${computeValidFields(computeMandatoryFields(data)).length}`
);
