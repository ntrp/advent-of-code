data = document
  .querySelector("body>pre")
  .innerText.split("\n")
  .filter((str) => str.length > 0)
  .map((str) => parseInt(str));

compute = (data, prev, n, target) => {
  if (n > 1) {
    let find;
    data.find((num) => {
      return (find = compute(data, [...prev, num], n - 1, target));
    });
    return find;
  }

  end = data.find((num) => [...prev, num].reduce((a, b) => a + b) == target);
  return end ? [...prev, end] : null;
};

find = (data, window) => {
  let pool = data.slice(0, window);
  let i = window;
  while (compute(pool, [], 2, data[i])?.length == 2) {
    pool = [...pool.slice(1, pool.length), data[i++]];
  }
  return { i, num: data[i] };
};

findKey = (data, numIdx, num) => {
  let sum = data[0];
  let i = 0;
  let j = 0;
  while (sum <= num) {
    sum += data[++j];
  }
  while (sum != num && j < numIdx) {
    if (sum > num) {
      sum -= data[i++];
    } else if (sum < num) {
      sum += data[++j];
    }
  }
  let range = data.slice(i, j + 1).sort((a, b) => b - a);
  return { i, j, res: range[0] + range[range.length - 1] };
};

res = find(data, 25);

// P1
console.log(`Part1: ${res.num}`);
// P2
console.log(`Part2: ${findKey(data, res.i, res.num).res}`);
