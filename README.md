[![progress-banner](https://backend.codecrafters.io/progress/interpreter/22d0aedc-438f-4a1e-a88c-5736eb7b71db)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is a starting point for Zig solutions to the
["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview).

This challenge follows the book
[Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

In this challenge you'll build an interpreter for
[Lox](https://craftinginterpreters.com/the-lox-language.html), a simple
scripting language. Along the way, you'll learn about tokenization, ASTs,
tree-walk interpreters and more.

Before starting this challenge, make sure you've read the "Welcome" part of the
book that contains these chapters:

- [Introduction](https://craftinginterpreters.com/introduction.html) (chapter 1)
- [A Map of the Territory](https://craftinginterpreters.com/a-map-of-the-territory.html)
  (chapter 2)
- [The Lox Language](https://craftinginterpreters.com/the-lox-language.html)
  (chapter 3)

These chapters don't involve writing code, so they won't be covered in this
challenge. This challenge will start from chapter 4,
[Scanning](https://craftinginterpreters.com/scanning.html).

**Note**: If you're viewing this repo on GitHub, head over to
[codecrafters.io](https://codecrafters.io) to try the challenge.

# Passing the first stage

The entry point for your program is in `src/main.zig`. Study and uncomment the
relevant code, and push your changes to pass the first stage:

```sh
git commit -am "pass 1st stage" # any msg
git push origin master
```

Time to move on to the next stage!

# Stage 2 & beyond

Note: This section is for stages 2 and beyond.

1. Ensure you have `zig (0.13+)` installed locally
2. Run `./your_program.sh` to run your program, which is implemented in
   `src/main.zig`.
3. Commit your changes and run `git push origin master` to submit your solution
   to CodeCrafters. Test output will be streamed to your terminal.

```markdown
Congratulations!
Challenge Complete!
Congratulations are in order. Only ~15% of users that attempt this challenge end up completing all stages, and you're one of them!

Here's what you can do next:

Polish your code. Perhaps you took a shortcut when approaching the challenge the first time. Now is a great time to clean up your code. You can simply push new commits to the existing repo, and we'll run tests just like before.
Publish to GitHub. Share your work with the world. With one click, you can publish your CodeCrafters project to GitHub. Click here to get started.
Try a different approach. You can re-approach the same challenge with a new programming language, a new constraint, or a new style. To launch the challenge again, use the dropdown on the top left.
If you've got any feedback or feature requests, feel free to let us know at hello@codecrafters.io. We respond to every single email.
```
