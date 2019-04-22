# My Dotfiles

*Disclaimer: README might not be up to date. 
Install script is reasonably stable (Only for Ubuntu)*

If you're just passing through and see something you think could be better, then let me know!

## NeoVim/Vim

This setup might be useful if you are just starting out.
I recommend using NeoVim for better plugin support and faster updates.
This config [here](https://github.com/rcarriga/dotfiles/blob/master/.config/nvim/init.vim) will work for NeoVim and (hopefully) Vim 8.1 or above.
If you want a Vim 8.0 config look (here)[https://github.com/rcarriga/dotfiles/blob/master/.vimrc], though I do not update it.

Main language support is for Haskell, Python and Typescript.
Most other languages have linting support from ALE.
Also includes support for LaTex and Markdown documents

<details><summary>Plugins</summary>
<p>

Language specific plugins are only loaded for the specified filetype to speedup startup time.

**General**

  - [dein.vim](https://github.com/Shougo/dein.vim)
        Plugin manager for vim which allows for lazy loading.
  - [NERDCommenter](https://github.com/scrooloose/nerdcommenter)
        Multi-lingual commenting plugin.
  - [FZF](https://github.com/junegunn/fzf.vim)
        Fuzzy file finding to open files from child directories.
  - [vim-sandwich](https://github.com/machakann/vim-sandwich)
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
  - [coc.nvim](https://github.com/neoclide/coc.nvim/)
        Fast and powerful language server client.
  - [echodoc](https://github.com/Shougo/echodoc.vim)
        Shows function signatures without opening new window.

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
        Built in thesauras
  - [vim-grammarous](https://github.com/rhysd/vim-grammarous)
        Grammar checking (Requires Java to be installed)

Also a couple of others used only to support the above.

<p>
</details>

<details><summary>Mappings - Tries to follow vim's mnemonics</summary>
<p>

#### Langage Server Commands

My leader key is set to default "\\" key.

*Prefix*: `<Leader>l` (Lower case L)

| Suffix           | Command                               |
| :----:           | :-----------------------------------  |
| `d`              | Definition                            |
| `r`              | Rename                                |
| `f`              | Format Document                       |
| `t`              | Type Definition                       |
| `x`              | References                            |
| `a`              | Code Actions Menu                     |
| `k`              | Hover (Loo**k**up)                    |
| `m`              | Menu of all Language Server commands  |
| `h`              | Hightlight                            |
| `g`              | Diagnostic Info at Cursor

#### Git Commands

*Prefix*: `<Leader>g`

| Suffix           | Command                               |
| :----:           | :----------------------------         |
| `s`              | Status                                |
| `p`              | Push                                  |
| `d`              | Diff                                  |
| `b`              | Browse (Open repo in browser)         |
| `l`              | Blame                                 |

  - *NB* Type "cc" in status window to commit changes.

#### FZF Commands

| Suffix           | Command                               |
| :----------:     | :------------------------------------ |
| `<Leader>f`      | Fuzzy File Finder                     |
| `<Leader>ag`     | Fuzzy File Contents Search (Using Ag) |

#### LaTex Commands

*Prefix*: `<Leader>l`

| Suffix           | Command                               |
| :----:           | :------------------------------------ |
| `l`              | Run compile server for LaTex document |
| `v`              | View compiled document                |

#### Misc:

| Suffix           | Command                               |
| :------:         | :-------------------------------      |
| `<Leader>nv`     | Open netrw vertical split             |
| `<Leader>ns`     | Open netrw horizontal split           |
| `Tab`            | Next Completion                       |
| `Ctrl+(h/j/k/l)` | Switch Window in Direction            |
| `<Leader>th`     | Open thesauras for selected word      |
| `<Leader>a`      | Align highlighted markdown table      |

Arrow keys are disabled in normal mode.

</p>
</details>

## Zsh

Using oh-my-zsh just for config settings. 
Plugins managed by zplug.
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

## XMonad

XMonad is a tiling window manager.
It is written in Haskell which might be difficult to setup quickly.
For a quick, easy-to-use WM try out [i3](https://i3wm.org/https://i3wm.org/).
XMonad site can be found [here](https://xmonad.org/).
i3 is faster to setup but I find XMonad to be much more powerful in configuration and less restrictive in what you can do.


## Tmux

If you don't know what tmux is well then I can't do it justice. 
Check out a basic intro [here](https://hackernoon.com/a-gentle-introduction-to-tmux-8d784c404340).
Basic tmux is useful but my config file is taken (shamelessly ripped) from [this fantastic repo](https://github.com/gpakosz/.tmux) which makes it even better!

## Kitty

Kitty is a GPU powered terminal emulator. 
To be honest my main reason for picking it was that it supported ligatures (e.g. Fira Code) and it uses a config file rather than a GUI which means it is easy to maintain a consistent terminal across machines. 

## Development Tools

These are some of the tools I use with Vim and standalone for software development

- Haskell project manager: [Stack](https://docs.haskellstack.org/en/stable/README/)
- Fast code searching: [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)
- Language Servers: 
  - [Python Language Server](https://github.com/palantir/python-language-server)
  - [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)
  - [Typescript Language Server](https://github.com/theia-ide/typescript-language-server)


## Enviroment Management and Utilities

Programs for managing the environment in i3 and in the terminal.
Some links are just for extra info. 
In those cases it should just be a package to install

- Terminal file browser: [ranger](https://github.com/ranger/ranger)
- Terminal Music Visualiser: [CLI Visualiser](https://github.com/dpayne/cli-visualizer)
- Backlight Control that just works: [light](https://github.com/haikarainen/light)
- Bluetooth Control: [Blueman](https://wiki.archlinux.org/index.php/Blueman#Usage)
- Wallpaper Setter: [feh](https://feh.finalrewind.org/)
- Custom Workspace Icons: [Font Awesome](https://fontawesome.com)
- Great Dark GTK Theme: [Arc](https://github.com/horst3180/arc-theme)
- GTK Themesetter: [lxappearance](http://www.linuxfromscratch.org/blfs/view/svn/lxde/lxappearance.html)
- Window Switcher: [rofi](https://github.com/DaveDavenport/rofi)
- Battery Info Command: [acpi](https://en.wikipedia.org/wiki/Advanced_Configuration_and_Power_Interface)
- Status Bar: [xmobar](https://archives.haskell.org/projects.haskell.org/xmobar/)
- Temperature Monitoring: [lm-sensors](https://github.com/lm-sensors/lm-sensors)
- Nice Lock Screen: [i3lock-fancy](https://github.com/meskarune/i3lock-fancy)
- Automatic Screen Locking: [xautolock](https://linux.die.net/man/1/xautolock)
- Screen Compositor (Transparent Backgrounds): [compton](https://github.com/chjj/compton)
- System Tray: [trayer-srg](https://github.com/sargon/trayer-srg)


<details><summary>Guides and References</summary>
<p>
A collection of great resources for learning about all things terminal and programming

- [Bash scripting cheatsheet](https://devhints.io/bash)
- [List of random but useful tools](https://kkovacs.eu/cool-but-obscure-unix-tools)
- [Stack guide](https://guide.aelve.com/haskell/stack-cookbook-ai0adh03)

</p>
</details>
