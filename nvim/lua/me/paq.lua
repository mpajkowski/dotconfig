local function clone_paq()
    local path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"
    local is_installed = vim.fn.empty(vim.fn.glob(path)) == 0
    if not is_installed then
        vim.fn.system { "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", path }
        return true
    end
end

local function bootstrap_paq(packages)
    local first_install = clone_paq()
    vim.cmd.packadd("paq-nvim")
    local paq = require("paq")
    if first_install then
        vim.notify("Installing plugins... If prompted, hit Enter to continue.")
    end

    -- Read and install packages
    paq(packages)
    paq.install()
end

bootstrap_paq {
    'savq/paq-nvim',
    'BurntSushi/ripgrep',
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope.nvim', tag = '0.1.5' },
    'nvim-tree/nvim-tree.lua',
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
    'tpope/vim-fugitive',
    'kristijanhusak/vim-hybrid-material',
    'ojroques/nvim-bufdel',
    'RRethy/nvim-base16',
    'nvim-tree/nvim-web-devicons',
    'nvim-lualine/lualine.nvim',
    'akinsho/bufferline.nvim',
    'neovim/nvim-lspconfig',
    { 'j-hui/fidget.nvim', branch = 'legacy' },
    'folke/trouble.nvim',
    'lervag/vimtex',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-vsnip',
    'hrsh7th/vim-vsnip',
    "FabijanZulj/blame.nvim"
}
