vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
        use 'wbthomason/packer.nvim'

        use {
            'nvim-telescope/telescope.nvim', tag = "0.1.4",
            requires = { { 'nvim-lua/plenary.nvim' }, { 'BurntSushi/ripgrep' } }
        }

        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
        use 'tpope/vim-fugitive'
        use 'kristijanhusak/vim-hybrid-material'
        use 'ojroques/nvim-bufdel'
        use { 'nvim-lualine/lualine.nvim', requires = { 'RRethy/nvim-base16' } }
        use 'nvim-telescope/telescope-file-browser.nvim'
        use 'nvim-tree/nvim-web-devicons'

        use { 'neovim/nvim-lspconfig' }
        use { 'j-hui/fidget.nvim', tag = 'legacy' }
        use 'folke/trouble.nvim'
        use 'diepm/vim-rest-console'
        use 'lervag/vimtex'

        use 'hrsh7th/nvim-cmp'
        use {
            'hrsh7th/cmp-nvim-lsp',
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-buffer',
            after = { 'hrsh7th/nvim-cmp' },
            requires = { 'hrsh7th/nvim-cmp' },
        }
    end)
