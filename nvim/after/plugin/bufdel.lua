local bufdel = require('bufdel')

bufdel.setup {
    next = "tabs",
    quit = true
}

vim.keymap.set('n', '<leader>bd', vim.cmd.BufDel, { noremap = true })
