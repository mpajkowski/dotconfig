-- helpers
function map(mode, shortcut, command)
  vim.api.nvim_set_keymap(mode, shortcut, command, { noremap = true, silent = true })
end

function nmap(shortcut, command)
  map('n', shortcut, command)
end

function imap(shortcut, command)
  map('i', shortcut, command)
end

function vmap(shortcut, command)
  map('v', shortcut, command)
end

-- packages
vim.cmd [[ packadd packer.nvim ]]

require("packer").startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'scrooloose/nerdtree'
    use 'junegunn/fzf'
    use 'junegunn/fzf.vim'
    use 'vim-airline/vim-airline'
    use 'vim-airline/vim-airline-themes'
    use 'kristijanhusak/vim-hybrid-material'
    use 'hrsh7th/nvim-compe'
    use 'rust-lang/rust.vim'
    use 'jacquesbh/vim-showmarks'
    use 'elubow/cql-vim'
    use 'editorconfig/editorconfig-vim'
    use 'sheerun/vim-polyglot'
    use 'lervag/vimtex'
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    use { 'junnplus/lsp-setup.nvim', requires = {
        'neovim/nvim-lspconfig',
        'williamboman/mason.nvim',
        'williamboman/mason-lspconfig.nvim',
    }}
    use {
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
      config = function()
        require("trouble").setup{}
      end
}
end)

---- autocompletion
require("compe").setup({
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = false;
    ultisnips = false;
    luasnip = false;
    treesitter = true;
  };
})

-- select first option when none selected
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm({ 'keys': '<CR>', 'select': v:true })", { expr = true })

require("mason").setup()

vim.g.mapleader = " "
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.cmd [[ colorscheme hybrid_reverse" ]]

-- behaviour
vim.opt.autoread = true
vim.opt.hidden = true
vim.opt.swapfile = false
vim.opt.mouse = "a"
vim.opt.shortmess = "a"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.showmode = false

-- text edit settings
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.textwidth = 110

-- misc
vim.opt.hlsearch = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.autowriteall = true
vim.opt.wildmenu = true
vim.opt.lazyredraw = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.completeopt = "menuone,noselect"

-- lsp-setup
require('lsp-setup').setup({
    servers = {
        rust_analyzer = {}
    }
})

-- NERDTree
vim.cmd [[
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeHijackNetrw=1
let g:NERDTreeMapJumpPrevSibling=""
let g:NERDTreeMapJumpNextSibling=""
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
]]

-- airline
vim.cmd [[
let g:airline_theme = 'atomic'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#right_alt_sep = '|'
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = '|'
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = '|'
let g:airline#extensions#tabline#ignore_bufadd_pat = 'term:'
let g:airline#extensions#tabline#fnamemod = ':.'
let g:airline#extensions#tabline#fnamecollapse = 0
]]

-- tabs
nmap("<Tab>", "<Plug>AirlineSelectNextTab")
nmap("<S-Tab>", "<Plug>AirlineSelectPrevTab")

-- split movement
nmap("<leader>h", ":wincmd h<CR>")
nmap("<leader>j", ":wincmd j<CR>")
nmap("<leader>k", ":wincmd k<CR>")
nmap("<leader>l", ":wincmd l<CR>")
nmap("<leader>nn", ":NERDTreeToggle<CR>")

-- save the buffer
nmap("zs", ":w<CR>")

-- :^)
nmap("<Down>", "<Nop>")
nmap("<Left>", "<Nop>")
nmap("<Right>", "<Nop>")
nmap("<Up>", "<Nop>")
-- :^))
vmap("<Down>", "<Nop>")
vmap("<Left>", "<Nop>")
vmap("<Right>", "<Nop>")
vmap("<Up>", "<Nop>")

nmap("<leader>dg", ":TroubleToggle<CR>")

local INITLUA = os.getenv("HOME") .. "/.config/nvim/init.lua"

nmap("<leader>ov", ":e " .. INITLUA .. "<CR>")
nmap("<leader>sv", ":w<CR>" .. ":luafile " .. INITLUA .. "<CR>")

-- close the buffer nicely
vim.cmd [[
" Delete buffer while keeping window layout (don't close buffer's windows).
" Version 2008-11-18 from http://vim.wikia.com/wiki/VimTip165
" Licensed under CC-BY-SA
if v:version < 700 || exists('loaded_bclose') || &cp
  finish
endif
let loaded_bclose = 1
if !exists('bclose_multiple')
  let bclose_multiple = 1
endif

" Display an error message.
function! s:Warn(msg)
  echohl ErrorMsg
  echomsg a:msg
  echohl NONE
endfunction

" Command ':Bclose' executes ':bd' to delete buffer in current window.
" The window will show the alternate buffer (Ctrl-^) if it exists,
" or the previous buffer (:bp), or a blank buffer if no previous.
" Command ':Bclose!' is the same, but executes ':bd!' (discard changes).
" An optional argument can specify which buffer to close (name or number).
function! s:Bclose(bang, buffer)
  if empty(a:buffer)
    let btarget = bufnr('%')
  elseif a:buffer =~ '^\d\+$'
    let btarget = bufnr(str2nr(a:buffer))
  else
    let btarget = bufnr(a:buffer)
  endif
  if btarget < 0
    call s:Warn('No matching buffer for '.a:buffer)
    return
  endif
  if empty(a:bang) && getbufvar(btarget, '&modified')
    call s:Warn('No write since last change for buffer '.btarget.' (use :Bclose!)')
    return
  endif
  " Numbers of windows that view target buffer which we will delete.
  let wnums = filter(range(1, winnr('$')), 'winbufnr(v:val) == btarget')
  if !g:bclose_multiple && len(wnums) > 1
    call s:Warn('Buffer is in multiple windows (use ":let bclose_multiple=1")')
    return
  endif
  let wcurrent = winnr()
  for w in wnums
    execute w.'wincmd w'
    let prevbuf = bufnr('#')
    if prevbuf > 0 && buflisted(prevbuf) && prevbuf != w
      buffer #
    else
      bprevious
    endif
    if btarget == bufnr('%')
      " Numbers of listed buffers which are not the target to be deleted.
      let blisted = filter(range(1, bufnr('$')), 'buflisted(v:val) && v:val != btarget')
      " Listed, not target, and not displayed.
      let bhidden = filter(copy(blisted), 'bufwinnr(v:val) < 0')
      " Take the first buffer, if any (could be more intelligent).
      let bjump = (bhidden + blisted + [-1])[0]
      if bjump > 0
        execute 'buffer '.bjump
      else
        execute 'enew'.a:bang
      endif
    endif
  endfor
  execute 'bdelete'.a:bang.' '.btarget
  execute wcurrent.'wincmd w'
endfunction
command! -bang -complete=buffer -nargs=? Bclose call <SID>Bclose('<bang>', '<args>')

if exists ("g:bclose_no_plugin_maps") &&  g:bclose_no_plugin_maps
    "do nothing
elseif exists ("g:no_plugin_maps") &&  g:no_plugin_maps
    "do nothing
else
     nnoremap <silent> <Leader>bd :Bclose<CR>
endif
]]
