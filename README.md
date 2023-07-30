# whoareu

A blog generator made with Haskell.

## Installation

```bash
git clone git@github.com:augustohdias/whoareu.git     # ssh
git clone https://github.com/augustohdias/whoareu.git # http
cd whoareu
stack install # Will install it inside ~/.local/bin/ directory. Make sure it is also inside your PATH.
```

## Usage

Use `init` to start a blog;
Create some `md` files inside the newly created `/posts` directory. Follow the template from `Post.md`;
Generate the html with `build` command;
Commit to github and setup github pages;

```
whoareu init  # Starts a new blog with a raw template
whoareu build # Generate blog html
```

## Showcase

[This](https://augustohdias.github.io/whoami/) is my blog. It was entirely generated by `whoareu`.

## Features

- Generate html pages from markdown
- Generate a profile sidebar from markdown
- Generate index from markdown posts

## TODO

- Basic template/theme support
- JS support
- Live-preview functionality

---

This is a personal project. Feel free to fork and contribute as you wish.
