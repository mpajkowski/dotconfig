vim.g.mapleader = " "

local opts = { noremap = true };

vim.keymap.set("n", "<TAB>", vim.cmd.bnext, opts)
vim.keymap.set("n", "<S-TAB>", vim.cmd.bprev, opts)
vim.keymap.set("n", "zs", ":w<CR>")

local function set_wincmd(arg)
    vim.keymap.set("n", "<leader>" .. arg, function() vim.cmd.wincmd(arg) end, opts)
end

set_wincmd("h")
set_wincmd("j")
set_wincmd("k")
set_wincmd("l")

vim.keymap.set("n", "<M-x>", ":")
