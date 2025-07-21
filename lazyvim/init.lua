-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")
require("config.godot")

local lspconfig = require("lspconfig")
lspconfig.ocamllsp.setup({})
lspconfig.gdscript.setup({})
