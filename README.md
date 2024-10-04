# Lox interpreter in Zig

This extends the ["Build your own Interpreter"
Challenge](https://app.codecrafters.io/courses/interpreter/overview) with
functions, returns, classes, etc using Zig for
[Lox](https://craftinginterpreters.com/the-lox-language.html), a simple
scripting language.

This implementation of Lox, follows the book [Crafting
Interpreters](https://craftinginterpreters.com/) by Robert Nystrom, apart from
tiny aesthetic tweaks.

## Challenge Status

<detail>

[![progress-banner](https://backend.codecrafters.io/progress/interpreter/22d0aedc-438f-4a1e-a88c-5736eb7b71db)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

```markdown
Congratulations!
Challenge Complete!
Congratulations are in order. Only ~15% of users that attempt this challenge
end up completing all stages, and you're one of them!
```

</detail>

Highlights:

- Recursive-descent parser
- Tokenization
- ASTs (Zig tagged unions really shine here)
- tree-walk interpreters and more.
