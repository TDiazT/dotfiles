-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")
require("config.godot")
require("util.git-format")

vim.lsp.enable("gdscript")

vim.lsp.config("ocamllsp", {
  cmd = { "ocamllsp", "--fallback-read-dot-merlin" },
})

vim.lsp.enable("ocamllsp")
