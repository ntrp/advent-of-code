# Advent Of Code

This is the code used to solve the [Advent of code](https://adventofcode.com) challenges over the years.

## Prepare

Copy the `.env.example` file to `.env` and fill in you session cookie after logging into the website

## Usage

To initialize a day you can use the command:

```
./utils/init-day <year> <day>
```

To initialize an entire year (fish shell):

```
for i in (seq 1 25); ./utils/init-day <year> $i; end
```
