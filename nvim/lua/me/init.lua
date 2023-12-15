require("me.paq")
require("me.set")
require("me.remap")

local me_group = vim.api.nvim_create_augroup("MeGroup", {})

--autocmd BufRead,BufNewFile * setlocal signcolumn=yes
--autocmd FileType tagbar,nerdtree setlocal signcolumn=no

--vim.api.nvim_create_autocmd({ "BufWritePre" }, {
--    group = me_group,
--    pattern = "*",
--    command = [[%s/\s\+$//e]]
--})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    group = me_group,
    pattern = "*",
    command = [[ setlocal signcolumn=yes ]]
})

vim.api.nvim_create_autocmd("FileType", {
    group = me_group,
    pattern = { "nerdtree" },
    command = [[ setlocal signcolumn=no ]]
})

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

if vim.g.neovide then
    vim.opt.guifont = { "Monacob", ":h11" }
    vim.g.neovide_cursor_animation_length = 0
end
