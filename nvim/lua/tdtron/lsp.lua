require("mason").setup({
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
        }
    }
})

-- Since the introduction of :h vim.lsp.config in Neovim 0.11, this plugin's feature set has been reduced.
-- Use this plugin if you want to automatically enable installed servers (:h vim.lsp.enable()) or
-- have access to the :LspInstall command.m.lsp.config in Neovim 0.11, this plugin's feature set has been reduced.
-- Use this plugin if you want to automatically enable installed servers (:h vim.lsp.enable()) or have access to the :LspInstall command.
-- So maybe remove?
require("mason-lspconfig").setup({
    -- A list of servers to automatically install if they're not already installed.
    ensure_installed = { "lua_ls", "bashls" },
})

-- Set different settings for different languages' LSP.
-- LSP list: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- How to use setup({}): https://github.com/neovim/nvim-lspconfig/wiki/Understanding-setup-%7B%7D
--     - the settings table is sent to the LSP.
--     - on_attach: a lua callback function to run after LSP attaches to a given buffer.
local lspconfig = require("lspconfig")

local on_attach = function(_, bufnr)
    -- Set up buffer-local keymaps (vim.api.nvim_buf_set_keymap()), etc.
    local opts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set("n", "<leader>C-b", vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", "<leader>C-.", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "<leader>lh", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<leader>ld", vim.diagnostic.open_float, opts)
    vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, opts)
end

-- Bash
lspconfig.bashls.setup({ on_attach = on_attach })

-- Lua
lspconfig.lua_ls.setup({ on_attach = on_attach })

-- Ocaml
lspconfig.ocamllsp.setup({ on_attach = on_attach })
