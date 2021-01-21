local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
    execute "packadd packer.nvim"
end

vim.cmd [[packadd packer.nvim]]

require("packer").startup(
    function()
        use {
            "wbthomason/packer.nvim",
            opt = true
        }
        use {
            "windwp/nvim-autopairs",
            config = "require('config.autopairs').post()"
        }
        use {
            "kyazdani42/nvim-tree.lua",
            setup = "require('config.tree').pre()",
            cmd = "NvimTreeToggle"
        }
        use {
            "glepnir/galaxyline.nvim",
            config = "require('config.galaxyline').post()"
        }
        use {
            "romgrk/barbar.nvim",
            setup = "require('config.barbar').pre()"
        }
        use {
            "indrewRadev/splitjoin.vim",
            keys = {
                "gS",
                "gJ"
            }
        }
        use {
            "lukas-reineke/format.nvim",
            cmd = "Format",
            config = "require('config.format').pre()"
        }
        use {
            "nvim-telescope/telescope.nvim",
            config = "require('config.telescope').post()",
            cmd = "Telescope",
            requires = {
                {
                    "nvim-telescope/telescope-dap.nvim"
                },
                {
                    "nvim-lua/popup.nvim"
                },
                {
                    "nvim-lua/plenary.nvim"
                },
                {
                    "kyazdani42/nvim-web-devicons"
                },
                {
                    "nvim-telescope/telescope-fzy-native.nvim"
                }
            }
        }
        use {
            "rrethy/vim-hexokinase",
            run = "make hexokinase"
        }
        use {
            "mfussenegger/nvim-dap",
            config = "require('config.dap').post()",
            requires = {
                {
                    "theHamsta/nvim-dap-virtual-text"
                },
                {
                    "mfussenegger/nvim-dap-python"
                }
            }
        }
        use {
            "sodapopcan/vim-twiggy",
            cmd = "Twiggy"
        }
        use {
            "kristijanhusak/vim-dadbod-ui",
            cmd = {
                "DBUI"
            },
            requires = {
                "tpope/vim-dadbod",
                opt = true
            }
        }
        use {
            "janko/vim-test",
            cmd = {"TestNearest", "TestFile"},
            opt = true
        }
        use {
            "rcarriga/vim-ultest",
            cmd = {
                "Ultest",
                "UltestNearest"
            }
        }
        use {
            "svermeulen/vim-subversive",
            keys = {
                "<leader>s"
            }
        }
        use {
            "voldikss/vim-floaterm",
            cmd = "FloatermNew"
        }
        use {
            "rhysd/conflict-marker.vim"
        }
        use {
            "Konfekt/FastFold"
        }
        use {
            "Yggdroot/hiPairs"
        }
        use {
            "godlygeek/tabular",
            cmd = "Tabularize"
        }
        use {
            "junegunn/fzf",
            requires = {
                {
                    "junegunn/fzf.vim"
                }
            }
        }
        use {
            "junegunn/goyo.vim",
            cmd = "Goyo"
        }
        use {
            "kkoomen/vim-doge",
            cmd = "DogeGenertate",
            run = ":call doge#install()"
        }
        use {
            "machakann/vim-sandwich"
        }
        use {
            "mhinz/vim-signify"
        }
        use {
            "neovim/nvim-lspconfig",
            setup = "require('config.lsp').pre()",
            config = "require('config.lsp').post()",
            requires = {
                {
                    "RishabhRD/popfix"
                },
                {
                    "RishabhRD/nvim-lsputils",
                    setup = "require('config.lsp_utils').pre()"
                }
            }
        }
        use {
            "nvim-treesitter/nvim-treesitter",
            config = "require('config.treesitter').post()",
            requires = {
                {
                    "nvim-treesitter/playground"
                },
                {
                    "nvim-treesitter/nvim-treesitter-textobjects"
                },
                {
                    "nvim-treesitter/nvim-treesitter-refactor"
                }
            }
        }
        use {
            "nvim-lua/completion-nvim",
            setup = "require('config.completion').pre()",
            requires = {
                {
                    "hrsh7th/vim-vsnip"
                },
                {
                    "hrsh7th/vim-vsnip-integ"
                },
                {
                    "steelsojka/completion-buffers"
                },
                {
                    "kristijanhusak/completion-tags"
                }
            }
        }
        use {
            "rhysd/clever-f.vim",
            keys = {
                "f",
                "t"
            }
        }
        use {
            "rhysd/git-messenger.vim",
            keys = "<leader>gm"
        }
        use {
            "simnalamburt/vim-mundo",
            cmd = "MundoToggle"
        }
        use {
            "tomtom/tcomment_vim",
            keys = {
                "gcc"
            }
        }
        use {
            "tpope/vim-abolish",
            cmd = "S"
        }
        use {
            "tpope/vim-eunuch",
            cmd = {
                "Rename",
                "Delete",
                "Remove",
                "Chmod"
            }
        }
        use {
            "tpope/vim-fugitive",
            requires = {
                {
                    "tpope/vim-rhubarb"
                }
            }
        }
        use {
            "tpope/vim-unimpaired"
        }
        use {
            "wellle/targets.vim",
            requires = {"wellle/line-targets.vim"}
        }
        use {
            "MTDL9/vim-log-highlighting"
        }
        use {
            "ekalinin/Dockerfile.vim",
            ft = {
                "dockerfile",
                "Dockerfile"
            }
        }
        use {
            "neovimhaskell/haskell-vim",
            ft = {
                "haskell"
            }
        }
        use {
            "posva/vim-vue",
            ft = "vue"
        }
    end
)
