LC3
===================

A Haskell implementation of the [LC3 virtual machine](https://justinmeiners.github.io/lc3-vm/)

This is still a heavy WIP. Some todo items..

### Todo
  - [x] Support `ADD` instructions
  - [x] Now draw the rest of the owl.

### Play!

```bash
$ nix-build
$ ./result/bin/LC3 ./2048.obj
```

### Result

```
+--------------------------+
|                          |
|   16    4     4          |
|                          |
|   2     2                |
|                          |
|               2          |
|                          |
|                          |
|                          |
+--------------------------+
```
