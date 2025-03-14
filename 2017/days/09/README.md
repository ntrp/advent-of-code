## Day 9: Stream Processing

A large stream blocks your path. According to the locals, it's not safe to cross the stream at the moment because it's full of _garbage_ . You look down at the stream; rather than water, you discover that it's a _stream of characters_ .

You sit for a while and record part of the stream (your puzzle input). The characters represent _groups_ - sequences that begin with `{` and end with `}` . Within a group, there are zero or more other things, separated by commas: either another _group_ or _garbage_ . Since groups can contain other groups, a `}` only closes the _most-recently-opened unclosed group_ - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many smaller ones.

Sometimes, instead of a group, you will find _garbage_ . Garbage begins with `<` and ends with `>` . Between those angle brackets, almost any character can appear, including `{` and `}` . _Within_ garbage, `<` has no special meaning.

In a futile attempt to clean up the garbage, some program has _canceled_ some of the characters within it using `!` : inside garbage, _any_ character that comes after `!` should be _ignored_ , including `<` , `>` , and even another `!` .

You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups, and garbage always terminates according to the rules above.

Here are some self-contained pieces of garbage:

- `<>` , empty garbage.
- `
  ` , garbage containing random characters.
- `<<<<>` , because the extra `<` are ignored.
- `<{!>}>` , because the first `>` is canceled.
- `
  `, because the second`!`is canceled, allowing the`>` to terminate the garbage.
- `>` , because the second `!` and the first `>` are canceled.
- `<{o"i!a,<{i` [, which ends at the first `>` .]()[]()

[Here are some examples of whole streams and the number of groups they contain:]()

- [`{}` , `1` group. `{{{}}}` , `3` groups. `{{},{}}` , also `3` groups. `{{{},{},{{}}}}` , `6` groups. `{<{},{},{{}}>}` , `1` group (which itself contains garbage).]()
- [`{`]()`,,,}` [, `1` group.]()[]()
- [`{{`]()`},{},{},{}}` [, `5` groups.]()[]()
- [`{{},{},{},{`]()`}}` [, `2` groups (since all but the last `>` are canceled).]()[]()

[Your goal is to find the total score for all groups in your input. Each group is assigned a _score_ which is one more than the score of the group that immediately contains it. (The outermost group gets a score of `1` .)]()

- [`{}` , score of `1` . `{{{}}}` , score of `1 + 2 + 3 = 6` . `{{},{}}` , score of `1 + 2 + 2 = 5` . `{{{},{},{{}}}}` , score of `1 + 2 + 3 + 3 + 3 + 4 = 16` .]()
- [`{`]()`,,,}` [, score of `1` .]() [`{{},{},{},{}}` , score of `1 + 2 + 2 + 2 + 2 = 9` . `{{},{},{},{}}` , score of `1 + 2 + 2 + 2 + 2 = 9` . `{{},{},{},{}}` , score of `1 + 2 = 3` .]()

[_What is the total score_ for all groups in your input?]()

## [Link]()

[https://adventofcode.com/2017/day/9](https://adventofcode.com/2017/day/9)
