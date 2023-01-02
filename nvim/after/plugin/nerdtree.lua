-- Close the tab if NERDTree is the only window remaining in it.
vim.cmd [[ autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif ]]

-- If another buffer tries to replace NERDTree, put it in the other window, and bring back NERDTree.
vim.cmd [[ autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
    \ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif ]]


vim.keymap.set("n", "<leader>nn", vim.cmd.NERDTreeToggle, { remap = false } )
