local telescope = require("telescope")
local builtin = require("telescope.builtin")

local is_in_config = string.find(vim.loop.cwd(), 'nvim') ~= nil

local find_files = function()
    return builtin.find_files {
        hidden = true,
        no_ignore = is_in_config,
        file_ignore_patterns = { 'node_modules', '.git', 'target' }
    }
end

local live_grep = function()
    return builtin.live_grep {
        hidden = true,
        no_ignore = is_in_config,
        file_ignore_patterns = { 'node_modules', '.git', 'target' }
    }
end

vim.keymap.set("n", "<leader>pf", find_files, {})
vim.keymap.set("n", "<leader>rg", live_grep, {})
vim.keymap.set("n", "<leader>b", builtin.buffers, {})

telescope.setup({
    wrap_results = true,
    color_devicons = false,
    extensions = {}
})
