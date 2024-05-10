# For home

### 📢 Advice

The tasks have the sole purpose of helping you understand the material better and faster. Therefore:

1. Avoid using external tools or help for solving the tasks.
2. Drop us an email with your question/problem when you have difficulties.
3. If you can't solve a task:
   1. Spend at least 30 minutes trying out different ways to achieve what you want - maybe try to break down the task into smaller tasks and create functions that solve them.
   2. Solving a task manually with pen and paper will help you break it down into smaller tasks.
   3. If the above points don't help, it's best to reach out to us, skip this task and continue with the others. This would allow you to continue solving, while you're waiting for feedback from us.
4. If you are struggling a lot with the tasks, don't hesitate to ask us for a `1-1 session` in-person or via Google Meet 😇!

### ⭐ Task 1

**Description:**

Define a function that returns the prime numbers in the range `(x, y)` that contain the digit `7`.

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.
3. `getPrimesLC` is defined using list comprehension.
4. `getPrimesLC` is defined on a single line of code.
5. `getPrimesHOF` is defined using higher order functions.
6. `getPrimesHOF` is defined on a single line of code.

**Test cases:**

```haskell
print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
```

### ⭐ Task 2

**Description:**

Define a function that returns the sum of the smallest and greatest palindrome divisors of a natural number greater than `1`.

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ getPalindromes 132465 == 8
print $ getPalindromes 654546 == 8
print $ getPalindromes 100001 == 100012
print $ getPalindromes 21612 == 21614
print $ getPalindromes 26362 == 26364
```

### 🌟 Task 3

**Description:**

Define a predicate that checks whether a sequence of numbers forms an [arithmetic progression](https://en.wikipedia.org/wiki/Arithmetic_progression).

> **Hint**: You may find the operator `!!` useful 😇.

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.
3. No `if-else` statements are present.
4. No guards that return `True` and/or `False` are present.

**Test cases:**

```haskell
print $ isArithmetic [3] == True
print $ isArithmetic [3, 5] == True
print $ isArithmetic [1, 2, 3, 4, 5] == True
print $ isArithmetic [3, 5, 7, 9, 11] == True
print $ isArithmetic [3, 5, 8, 9, 11] == False
print $ isArithmetic [3, 5, 9, 9, 11] == False
```

### ⭐ Task 4

**Description:**

Define a function that returns the sum of `the special numbers` in the interval `[x, y]` (`x <= y`). A number is `special` if it contains `6` and can be expressed as `4k + 1`, where `k` is a whole number.

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.
3. The task is solved on a single line of code.

**Test case:**

```haskell
print $ specialSum 1 100 == 195 -- 61, 65, 69
```

### 🌟 Task 5

**Description:**

Define a function that takes a single argument function and returns it applied `n` times.

**Acceptance criteria:**

1. All tests pass.
2. Typeclasses are used.
3. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ (applyN (\x -> 2 * x) 5) 2 == 64
print $ (applyN (\x -> div x 10) 2) 100 == 1
```

Explanation for test case 1:

```text
We apply 2 * x first to 2, then to (2 * 2) and so on. Thus, we get: (((((2 * 2) * 2) * 2) * 2) * 2) => ((((4 * 2) * 2) * 2) * 2) => (((8 * 2) * 2) * 2) => ((16 * 2) * 2) => (32 * 2) => 64
```

### 💫 Task 6

**Description:**

![Alt text](assets/forHomeTask5.png?raw=true "forHomeTask5.png")

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8
```

### 💫 Task 7

**Description:**

![Alt text](assets/forHomeTask6.png?raw=true "forHomeTask6.png")

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
```

### 💫 Task 8

**Description:**

Дефинирайте функция `repeater str`, която получава като аргумент символен низ и връща анонимна функция на два аргумента - `count` и `glue` (число и низ). Оценката на обръщението към върнатата функция е низ, който се получава чрез `count`-кратно повтаряне на низа `str`, при което между всеки две съседни повторения на `str` стои низът `glue`.

**Acceptance criteria:**

1. All tests pass.
2. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
```

### ⭐ Task 9

**Description:**

For the Vector data type we defined in class, define the following functions:

- `dotProduct`: scalar product;
- `crossProduct`: vector product;
- `magnitude`: length of a vector.

**Acceptance criteria:**

1. All tests pass.
2. Typeclasses are used.
3. Add one new test case. Place a comment after it with the words `my test`.

**Test cases:**

```haskell
print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)

print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)

print $ magnitude (1, 2, 3) == 3.7416573867739413
print $ magnitude (7, 4, 1) == 8.12403840463596
print $ magnitude (-10, 20, -10) == 24.49489742783178
print $ magnitude (5, 2, 159) == 159.0911688309568
print $ magnitude (0, -1, -2) == 2.23606797749979
print $ magnitude (155, 10, -5) == 155.40270267920053
```