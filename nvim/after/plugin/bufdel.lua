local bufdel = require('bufdel')

bufdel.setup {
    next = "tabs",
    quit = true
}

vim.keymap.set('n', '<C-x>k', vim.cmd.BufDel, { noremap = true })
