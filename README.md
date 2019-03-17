# My Dotfiles

*Disclaimer: README might not be up to date. 
Install script is not stable and probably will not work!* 

My humble collection of dotfiles and programs I like. 
I try to not go overboard with features so I keep only what I use often.
If you're just passing through and see something you think could be better, then let me know!

## Vim

Been using Vim daily since January 2019 so by no means an expert. 
This setup might be useful if you are just starting out.
Main language support is for Haskell, Python and Typescript. Most other languages have linting support from ALE.
Most plugins that offer built in commands for terminal interaction (e.g. debugging) are unnecessary due to Tmux.

<details><summary>Plugins</summary>
<p>

Language specific plugins are only loaded for the specified filetype to speedup startup time.

**General**

  - [vim-plug](https://github.com/junegunn/vim-plug)
        Plugin manager for vim which allows for lazy loading.
  - [NERDCommenter](https://github.com/scrooloose/nerdcommenter)
        Multi-lingual commenting plugin.
  - [FZF](https://github.com/junegunn/fzf.vim)
        Fuzzy file finding to open files from child directories.
  - [vim-surround](https://github.com/tpope/vim-surround)
        Allows for surrounding text objects with any character.
  - [auto-pairs](https://github.com/jiangmiao/auto-pairs)
        Creates pairs of quotes and brackets when one is typed.
  - [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
        Shows git status for  each line in gutter (Left side of buffer).
  - [lightline](https://github.com/itchyny/lightline.vim)
        Prettier statusbar.
  - [vim-fugitive](https://github.com/tpope/vim-fugitive)
        Better git integration.
  - [ALE](https://github.com/w0rp/ale)
        Asynchronous linting.
  - [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)
        Runs language servers.
  - [echodoc](https://github.com/Shougo/echodoc.vim)
        Shows function signatures without opening new window.
  - [deoplete](https://github.com/Shougo/deoplete.nvim)
        Asynchronous auto complete popup with custom sources.

**Python Specific**:

  - [SimpylFold](https://github.com/tmhedberg/SimpylFold)
        Python friendly code folding.
  - [vim-virtualenv](https://github.com/plytophogy/vim-virtualenv) *NB: Install pylint in virtualenv*.
        Enables virtual environments.

**Haskell Specific**:

  - [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
        Better haskell syntax highlighting.

**Typescript Specific**

  - [typescript-vim](https://github.com/leafgarland/typescript-vim)
        Better typescript syntax highlighting.

**Markdown and LaTex Specific**

  - [vim-easy-align](https://github.com/junegunn/vim-easy-align)
        Auto align markdown tables
  - [vimtex](https://github.com/lervag/vimtex)
        Integrated latex compiler, viewer and other features
  - [Thesauras Query](https://github.com/Ron89/thesaurus_query.vim)
        Built in thesauras (Only in markdown and latex files)

Also a couple of others used only to support the above.

<p>
</details>

<details><summary>Mappings - Tries to follow vim's mnemonics</summary>
<p>

#### Langage Server Commands

My leader key is set to default "\\" key.

*Prefix*: `<Leader>l` (Lower case L)

| Suffix | Command                              |
| :----: | :----------------------------------- |
| `d`    | Definition                           |
| `r`    | Rename                               |
| `f`    | Format Document                      |
| `t`    | Type Definition                      |
| `x`    | References                           |
| `a`    | Code Actions Menu                    |
| `k`    | Hover (Loo**k**up)                   |
| `m`    | Menu of all Language Server commands |
| `h`    | Hightlight                           |

#### Git Commands

*Prefix*: `<Leader>g`

| Suffix | Command                       |
| :----: | :---------------------------- |
| `s`    | Status                        |
| `p`    | Push                          |
| `d`    | Diff                          |
| `b`    | Browse (Open repo in browser) |
| `l`    | Blame                         |

  - *NB* Type "cc" in status window to commit changes.

#### FZF Commands

| Suffix       | Command                               |
| :----------: | :------------------------------------ |
| `<Leader>f`  | Fuzzy File Finder                     |
| `<Leader>ag` | Fuzzy File Contents Search (Using Ag) |

#### LaTex Commands

*Prefix*: `<Leader>l`

| Suffix | Command                               |
| :----: | :------------------------------------ |
| `l`    | Run compile server for LaTex document |
| `v`    | View compiled document                |

#### Misc:

| Suffix           | Command                          |
| :------:         | :------------------------------- |
| `<Leader>nv`     | Open netrw vertical split        |
| `<Leader>ns`     | Open netrw horizontal split      |
| `Tab`            | Next Completion                  |
| `Ctrl+(h/j/k/l)` | Switch Window in Direction       |
| `<Leader>th`     | Open thesauras for selected word |
| `<Leader>a`      | Align highlighted markdown table |

Arrow keys are disabled in normal mode.

</p>
</details>

## Zsh

Using oh-my-zsh just for config settings. All plugins managed by zplug.
If you like a more featureful shell then would recommend using spaceship theme.
However I found this caused slight delay between commands so using the pure theme
for buttery smooth performance.

<details><summary>Plugins</summary>
<p>

  - [zsh-completions](https://github.com/zsh-users/zsh-completions)
  - [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
  - [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
  - [autojump](https://github.com/wting/autojump)
  - [k](https://github.com/supercrabtree/k)
  - [pure](https://github.com/sindresorhus/pure)
  - [zsh-async](https://github.com/mafredri/zsh-async)

</p>
</details>

<details><summary>Extras</summary>
<p>

  - [ZPlug](https://github.com/zplug/zplug)

</p>
</details>


## Tmux

If you don't know what tmux is well then I can't do it justice. Check out a basic intro [here](https://hackernoon.com/a-gentle-introduction-to-tmux-8d784c404340).
Basic tmux is useful but my config file is taken (shamelessly ripped) from [this fantastic repo](https://github.com/gpakosz/.tmux) which makes it even better!

## Kitty

Kitty is a GPU powered terminal emulator. To be honest my main reason for picking it was that it supported ligatures (e.g. Fira Code) and it uses a config file rather than a GUI which means it is easy to maintain a consistent terminal across machines. 

## Bonus Stuff

<details><summary>Programs to Install</summary>
<p>
 These are just programs I use often and so I want to just have them listed.  

- Terminal file browser: [Ranger](https://github.com/ranger/ranger)
- Haskell project manager: [Stack](https://docs.haskellstack.org/en/stable/README/)
- Fast code searching: [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)

- Language Servers: 
  - [Python Language Server](https://github.com/palantir/python-language-server)
  - [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)
  - [Typescript Language Server](https://github.com/theia-ide/typescript-language-server)

</p>
</details>

<details><summary>Guides and References</summary>
<p>
A collection of great resources for learning about all things terminal and programming

- [Bash scripting cheatsheet](https://devhints.io/bash)
- [List of random but useful tools](https://kkovacs.eu/cool-but-obscure-unix-tools)
- [Stack guide](https://guide.aelve.com/haskell/stack-cookbook-ai0adh03)

</p>
</details>
