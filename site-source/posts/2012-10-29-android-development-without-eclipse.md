---
title: Android development without eclipse
tags: android, vim, eclipse
---

Of course it was really handy using eclipse for android development,
however recently I don\'t know what the hell eclipse is doing background, 
it does not respond frequently when I am editing java codes and the only thing I could do is to kill it.

Maybe it is because I\'ve added a 1G memory? And everything keep working except eclipse?
Or maybe it is because I\'ve got too much plugin? But all I have is the plugins installed following the ADT installation instructions.

Get rid of eclipse! At least I don\'t want to get me mad again.

Then currently I might have found the way, by making use of vim and eclim, the latter runs eclipse headlessly.

Just keep a memo for myself about how to develop android programs without eclipse.

This post will keep merging things I think necessary for me to develop android apps.

Since I\'m not familiar with vim though I\'ve used it for a long time, I might keep some basic knowledge about vim here.

# Tools
* vim
* [Eclim](http://eclim.org/install.html)

# How to XXX
## connect with Eclim
Eclim works as a daemon, go to your eclipse root directory, 
run `./eclimd start` so that eclim will be started as daemon.

to shutdown the daemon, use command `:ShutdownEclim` or run `$ECLIPSE_HOME/eclim -command shutdown`

Open a vim, use command `:PingEclim`, if you can see the version of eclim and eclipse, the installation of eclim is done

## create android project

open vim, create project: 

`:ProjectCreate {project_path} -n android`

and `.project` as well as `.classpath` should have been generated.

## project commands
`:ProjectList` prints a list of all projects available in eclipse.

`:ProjectTree {project name}` show the project root directory and its structure.

`:ProjectDelete {project name}` removes project 

## build & install project
run `:Ant debug install` in the project\'s root directory, and the built .apk file is located at `./bin`

note that `ant` requires `build.xml` in the project\'s root directory, if you cannot find one, 
use `android update project -p {project path} --n {project name}` to generate one.

## code completion
`Ctrl+x Ctrl+u` to open suggestions
`Ctrl+p` move to the next

## command mapping
edit `.vimrc`

```vim
set <m-i>=^[i
set <m-o>=^[o
set <m-c>=^[c

"alt+i for auto import neccessary packages
"alt+o for open project
nmap <m-i> :JavaImportOrganize<CR>
nmap <m-o> :ProjectTree<CR>
nmap <m-c> :JavaCorrect<CR>
```

note: `^[i` is inputted by `<ctrl+v><alt+i>` and `^[o` is inputted by `<ctrl+v><alt+o>`, etc.
