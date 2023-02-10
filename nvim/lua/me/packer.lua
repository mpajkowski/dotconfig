vim.cmd.packadd("packer.nvim")

return require("packer").startup(function(use)
        use "wbthomason/packer.nvim"

        use {
            "nvim-telescope/telescope.nvim", tag = "0.1.0",
            requires = { { "nvim-lua/plenary.nvim" }, { "BurntSushi/ripgrep" } }
        }

        use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
        use("tpope/vim-fugitive")
        use("kristijanhusak/vim-hybrid-material")
        use("ojroques/nvim-bufdel")
        use({"nvim-lualine/lualine.nvim", requires = {'RRethy/nvim-base16'}})
        use("nvim-telescope/telescope-file-browser.nvim")
        use("nvim-tree/nvim-web-devicons")

        use("neovim/nvim-lspconfig")
        use("j-hui/fidget.nvim")

        use("hrsh7th/nvim-cmp")
        use({
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-vsnip",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-buffer",
            after = { "hrsh7th/nvim-cmp" },
            requires = { "hrsh7th/nvim-cmp" },
        })
    end)
