local cmp = require("cmp")
cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end
    },
    mapping = {
        ["<C-p>"] = cmp.mapping.select_prev_item(),
        ["<C-n>"] = cmp.mapping.select_next_item(),
        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.close(),
        ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        })
    },
    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "vsnips" },
        { name = "path" },
        { name = "buffer" },
    }),
})

require("fidget").setup({})

local lspconfig = require("lspconfig")

local function on_attach(client, bufnr)
    local opts = { buffer = bufnr, remap = false }

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
    vim.keymap.set("n", "<leader>dg", ":TroubleToggle<CR>", opts)
    vim.keymap.set("n", "[d", vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", "]d", vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "<leader>mv", vim.lsp.buf.rename, opts)
    vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)

    vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format({ async = false })]]
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()

local is_in_styx_bare_crates = function()
    return string.match(vim.fn.expand('%:p'), 'styx/bare_crates')
end

lspconfig.rust_analyzer.setup({
    capabilities = capabilities,
    on_attach = on_attach,
    on_init = function(client)
        local path = client.workspace_folders[1].name

        if string.match(path, 'styx/kernel') then
            print('executing styx quirks')
            client.config.settings["rust-analyzer"].check.allTargets = false
            client.config.settings["rust-analyzer"].cargo.target = 'x86_64-unknown-none'

            client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
        end

        return true
    end,
    settings = {
        ["rust-analyzer"] = {
            check = {
                allTargets = true,
            },
            checkOnSave = {
                enable = true,
                command = "clippy",
                extraArgs = { "--target-dir", "/tmp/rust-analyzer-check" }
            },
            cargo = {
                allFeatures = true,
            }
        }
    },
})

lspconfig.taplo.setup({
    capabilities = capabilities,
    on_attach = on_attach
})

lspconfig.clangd.setup({
    capabilities = capabilities,
    on_attach = on_attach
})

lspconfig.gopls.setup({
    capabilities = capabilities,
    on_attach = on_attach
})

lspconfig.tsserver.setup {
  capabilities = capabilities,
  on_attach = on_attach,
  filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
  cmd = { "typescript-language-server", "--stdio" }
} 

lspconfig.zls.setup({
    capabilities = capabilities,
    on_attach = on_attach
})

lspconfig.asm_lsp.setup({
    capabilities = capabilities,
    on_attach = on_attach,
    filetypes = { "asm", "s", "S", "inc" },
})

vim.diagnostic.config({
    virtual_text = false
})
