local telescope = require("telescope")
local builtin = require("telescope.builtin")

local find_files = function()
    return builtin.find_files({ hidden = true,
        --no_ignore = true,
        file_ignore_patterns = { 'node_modules', '.git', 'target' } })
end

vim.keymap.set("n", "<leader>pf", find_files, {})
vim.keymap.set("n", "<leader>rg", builtin.live_grep, {})
vim.keymap.set("n", "<leader>pb", builtin.buffers, {})

telescope.setup({
    wrap_results = true,
    color_devicons = false,
    extensions = {
        file_browser = {
            color_devicons = false,
            respect_gitignore = false,
            theme = "ivy",
            -- disables netrw and use telescope-file-browser in its place
            hijack_netrw = true,
        },
    },
})

telescope.load_extension("file_browser")

vim.keymap.set("n", "<leader>nn", ":Telescope file_browser<CR>", {})
