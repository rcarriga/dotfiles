# vimrc
Just a simple vimrc

Current Setup: 
  - Basic Plugins & Config:
      - NERDTree for file structure with git plugin.
      - NERD Comment for commenting.
      - Tagbar for showing ctags.
      - FZF for file searching.
      - Surround for... well surrounding with quotes, brackets etc.
      - Gitgutter to show git diff beside lines.
      - Lightline for better status bar.
      - Codedark colorscheme (Copy of vscode colors)
      - Open files in chrome using "\ch" (Mainly for pdfs)
      - Vim-pandoc for pandoc support
      - Vim-fugitive for some fantastic git commands
  - Python:
      - Pylint linting using Syntastic (Be aware this is not done asynchronously so can be a little slow, but is more stable than ALE).
      - Vim-lsp and pyls for all other language server stuff (IE: Flake8 linting, jedi autocomplete, black and yapf formatting etc).
      - SimpylFold for folding.
      - Pydocstring to generate docstrings
      - Vim-virtualenv for virtualenv support.
  - Terraform:
      - vim-terrform for syntax highlighting and formatting
  - Ruby:
      - Vim-lsp and solargraph for linting etc (Probably not the most advanced lsp for Ruby but don't use it enough to bother changing).
   
Work in Progress:
  - Haskell Development Plugins
