local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader>pf", builtin.git_files, {})
vim.keymap.set("n", "<leader>rg", builtin.live_grep, {})
vim.keymap.set("n", "<leader>pb", builtin.buffers, {})
