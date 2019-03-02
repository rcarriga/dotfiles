# My Dotfiles

*Disclaimer: README might not be up to date. Use install scripts at your own risk (Read them before using)*

My humble collection of dotfiles. I try aim for performance over features so I keep only what I use often.
If you're just passing through and see something you think could be better, then let me know!

## Vim

Been using Vim daily since January 2019 so by no means an expert. 
This setup might be useful if you are just starting out.
Main language support is for Haskell, Python and Typescript. Most other languages have linting support from ALE.
I've attempted to strike a balance between performance and IDE-like tools.
Most plugins that offer built in commands for terminal interaction (e.g. debugging) are unnecessary due to Tmux.

<details><summary>Plugins</summary>
<p>

  - [vim-plug](https://github.com/junegunn/vim-plug)
  - [NERDTree](https://github.com/scrooloose/nerdtree)
  - [NERDCommenter](https://github.com/scrooloose/nerdcommenter)
  - [FZF](https://github.com/junegunn/fzf.vim) *NB: Uses Ag - The Silver Searcher*
  - [vim-surround](https://github.com/tpope/vim-surround)
  - [auto-pairs](https://github.com/jiangmiao/auto-pairs)
  - [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
  - [lightline](https://github.com/itchyny/lightline.vim)
  - [vim-fugitive](https://github.com/tpope/vim-fugitive)
  - [ALE: Asynchronous Lint Engine](https://github.com/w0rp/ale)
  - [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)
  - [echodoc](https://github.com/Shougo/echodoc.vim)
  - [deoplete](https://github.com/Shougo/deoplete.nvim)
  - [ondark colorscheme](https://github.com/joshdick/onedark.vim)
  - **Python Specific**:
      - [SimpylFold](https://github.com/tmhedberg/SimpylFold)
      - [vim-virtualenv](https://github.com/plytophogy/vim-virtualenv) *NB: Install pylint in virtualenv*.
  - **Terraform Specific**:
      - [vim-terraform](https://github.com/hashivim/vim-terraform)
  - **Haskell Specific**:
      - [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
  - Also a couple of others used only to support the above.

<p>
</details>

<details><summary>Programs Needed</summary>
<p>

  - [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)
  - [Python Language Server](https://github.com/palantir/python-language-server)
  - [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)
  - [Typescript Language Server](https://github.com/theia-ide/typescript-language-server)

</p>
</details>

<details><summary>Mappings - Tries to follow vim's mnemonics</summary>
<p>

#### Langage Server Commands

*Prefix*: `Leader+l` (Lower case L)

|  Suffix  | Command  |
| :------: | :------- |
|   `d`    | Definition |
|   `r`    | Rename |
|   `f`    | Format Document |
|   `t`    | Type Definition |
|   `x`    | References |
|   `a`    | Code Actions Menu |
|   `k`    | Hover (Loo*k*up) |
|   `m`    | Menu of all Language Server commands |
|   `h`    | Hightlight |

#### Git Commands

*Prefix*: `Leader+g`

|  Suffix  | Command  |
| :------: | :------- |
|   `s`    | Status |
|   `p`    | Push |
|   `d`    | Diff |
|   `b`    | Browse (Open repo in browser)  |
|   `l`    | Blame |

  - *NB* Type "cc" in status window to commit changes.
#### FZF Commands

|   Suffix    | Command  |
|  :------:   | :------- |
| `Leader+f`  | Fuzzy File Finder |
| `Leader+ag` | Fuzzy File Contents Search (Using Ag) |

#### Misc:

|   Suffix         | Command  |
|  :------:        | :------- |
| `Ctrl+o`         | Toggle NERDTree |
| `Tab`            | Next Completion |
| `Ctrl+(h/j/k/l)` | Switch Window in Direction  |

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
  - [Oh-My-Zsh](https://github.com/robbyrussell/oh-my-zsh)

</p>
</details>


## Tmux
Config is shamelessly ripped from [this fantastic repo](https://github.com/gpakosz/.tmux)
