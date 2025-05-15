-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

local lspconfig = require("lspconfig")
lspconfig.ocamllsp.setup({})

local camldebug = require("plugins.camldebug")
camldebug.setup()
