local cmp = require("cmp")
cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    mapping = {
        ["<C-p>"] = cmp.mapping.select_prev_item(),
        ["<C-n>"] = cmp.mapping.select_next_item(),
        -- Add tab support
        ["<S-Tab>"] = cmp.mapping.select_prev_item(),
        ["<Tab>"] = cmp.mapping.select_next_item(),
        ["<C-d>"] = cmp.mapping.scroll_docs( -4),
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
        { name = "vsnip" },
        { name = "path" },
        { name = "buffer" },
    }),
    experimental = {
        ghost_text = true,
    },
})

local lsp_status = require("lsp-status")
lsp_status.config({ status_symbol = "*" })
lsp_status.register_progress()

local lspconfig = require("lspconfig")

local function on_attach(client, bufnr)
    local telescope = require("telescope.builtin")
    local opts = { buffer = bufnr, remap = false }

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
    vim.keymap.set("n", "<leader>dg", telescope.diagnostics, opts)
    vim.keymap.set("n", "[d", vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", "]d", vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "<leader>mv", vim.lsp.buf.rename, opts)
    vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)

    vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
    lsp_status.on_attach(client, bufnr)
end

lspconfig.rust_analyzer.setup({
    on_attach = on_attach,
    handlers = lsp_status.handlers,
    settings = {
        ["rust-analyzer"] = {
            check = {
                allTargets = false
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
    capabilities = lsp_status.capabilities
})

vim.diagnostic.config({
    virtual_text = false
})
