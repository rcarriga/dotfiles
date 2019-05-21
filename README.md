# My Dotfiles

*Disclaimer: README might not be up to date. 
Setup script is reasonably stable (Only for Ubuntu)*

If you're just passing through and see something you think could be better, then let me know!

## Text Editor / IDE - NeoVim

Recommend using NeoVim over Vim for better plugin support and faster updates.
For dependencies run `:checkhealth`.

<details><summary>Language Support</summary>
<p>

 - Haskell\*
 - Python
 - JavaScript/TypeScript
 - C/C++\*
 - Java
 - Docker
 - HTML/CSS
 - YAML/JSON
 - LaTex/Markdown

\*_Requires manual install. See language servers below._

</p>
</details>

<details><summary>Plugins</summary>
<p>

**General**

  - [dein.vim](https://github.com/Shougo/dein.vim)
        Plugin manager for vim which allows for lazy loading.
  - [coc.nvim](https://github.com/neoclide/coc.nvim/)
        Fast and powerful language server client.
  - [git-messenger.vim](https://github.com/rhysd/git-messenger.vim)
        Provides descriptive git history for any line in a file.
  - [NERDCommenter](https://github.com/scrooloose/nerdcommenter)
        Multi-lingual commenting plugin.
  - [FZF](https://github.com/junegunn/fzf.vim)
        Fuzzy file finding to open files from child directories.
  - [vim-sandwich](https://github.com/machakann/vim-sandwich)
        Allows for surrounding text objects with any character.
  - [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
        Shows git status for  each line in gutter (Left side of buffer).
  - [lightline](https://github.com/itchyny/lightline.vim)
        Prettier statusbar.
  - [vim-fugitive](https://github.com/tpope/vim-fugitive)
        Better git integration.

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



<p>
</details>

<details><summary>Exterior Tools</summary>
<p>

- Fast code searching: [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)
- Language Servers: 
  - [Clangd](https://clang.llvm.org/extra/clangd/)
  - [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)

</p>
</details>

<details><summary>Key Mappings</summary>
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
| `m`              | Messenger *NB Does not use `g` prefix*|

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

_Arrow keys are disabled in normal mode._

</p>
</details>

## Shell - Zsh

Plugins managed by [Zgen](https://github.com/tarjoilija/zgen).

<details><summary>Plugins</summary>
<p>

  - [zsh-completions](https://github.com/zsh-users/zsh-completions)
  - [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
  - [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
  - [autojump](https://github.com/wting/autojump)
  - [pure](https://github.com/sindresorhus/pure)
  - [clean](https://github.com/BrandonRoehl/zsh-clean)

</p>
</details>

## Window Manager - XMonad

[XMonad](https://xmonad.org/)

<details><summary>Features</summary>
<p>

 - Tiling Window Manager
 - Written and configured in Haskell
 - Powerful/Flexible
 - Lightweight

</p>
</details>

<details><summary>Other Tiling WMs</summary>
<p>

XMonad requires GHC which is large so if you're not writing Haskell anyway maybe check out these

 - [i3](https://i3wm.org/https://i3wm.org/)
 - [awesome](https://awesomewm.org/)

</p>
</details>

<details><summary>Tmux</summary>
<p>

Working on Mac or just don't want to commit to a window manager?\
then tmux is good for emulating some of their features for terminals.\
Check out a basic intro [here](https://hackernoon.com/a-gentle-introduction-to-tmux-8d784c404340).\
Great config can be found in [this repo](https://github.com/gpakosz/.tmux)

</p>
</details>

## Terminal - Kitty

<details><summary>Features</summary>
<p>

  - Uses GPU
  - Buttery smooth performance
  - Support for images
  - Unicode support (even with shortcut to input)
  - Font ligature support (Very nice with Haskell)
  - Works with pywal

</p>
</details>

## Linux Enviroment Management

<details><summary>General tools for system management</summary>
<p>

The ones listed here are the ones I am currently using.

- Dotfiles Management: [YADM](https://yadm.io/)
- Backlight Control: [light](https://github.com/haikarainen/light)
- Bluetooth Control: [Blueman](https://wiki.archlinux.org/index.php/Blueman#Usage)
- Wallpaper Setter & Colorscheme Generator: [pywal](https://github.com/dylanaraps/pywal)
- Window Switcher: [rofi](https://github.com/DaveDavenport/rofi)
- Status Bar and System Tray: [polybar](https://archives.haskell.org/projects.haskell.org/xmobar/)
- Temperature Monitoring: [lm-sensors](https://github.com/lm-sensors/lm-sensors)
- Screen Compositor: [compton](https://github.com/chjj/compton)
- Terminal file browser: [vifm](https://vifm.info/)

</p>
</details>

<details><summary>Appearance and _Ricing_</summary>
<p>
 
- GTK Themesetter: [lxappearance](http://www.linuxfromscratch.org/blfs/view/svn/lxde/lxappearance.html)
- Terminal Music Visualiser: [CLI Visualiser](https://github.com/dpayne/cli-visualizer)
- Custom Workspace Icons: [Font Awesome](https://fontawesome.com)
- Great Dark GTK Theme: [Arc](https://github.com/horst3180/arc-theme)

</p>
</details>

## Guides and References

A collection of great resources I've used.

- [Bash scripting cheatsheet](https://devhints.io/bash)
- [List of random but useful tools](https://kkovacs.eu/cool-but-obscure-unix-tools)
- [Stack guide](https://guide.aelve.com/haskell/stack-cookbook-ai0adh03)
