LC3
===================

A Haskell implementation of the [LC3 virtual machine](https://justinmeiners.github.io/lc3-vm/)

### Play 2048!

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

### Play Rogue!

```bash
$ nix-build
$ ./result/bin/LC3 ./rogue.obj
```

### Result

```
##################  ############
###################     ########
#######################        #
########################  #  #
###############################D
################################
################################
  ##############################
#  #############################
## @  ##########################
#####  #########################
######  ########################
#######   ######################
#########    ###################
############  ##  ##############
#############      #############
```
