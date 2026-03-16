-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")
require("config.godot")
require("util.git-format")

vim.lsp.enable("gdscript")

vim.lsp.config("ocamllsp", {
  cmd = { "ocamllsp", "--fallback-read-dot-merlin" },
})

vim.lsp.enable("ocamllsp")

-- vim.lsp.config("cmake").setup({})
vim.lsp.enable("cmake")

local clangd = vim.fn.has("mac") == 1 and "/opt/homebrew/opt/llvm/bin/clangd" or "clangd"

vim.lsp.config("clangd", {
  cmd = {
    clangd,
    "--compile-commands-dir=build",
    "--background-index",
    "--clang-tidy",
    "--header-insertion=iwyu",
    "--completion-style=detailed",
    "--function-arg-placeholders=1",
  },
  filetypes = { "c", "cpp", "objc", "objcpp" },
  root_markers = { "compile_commands.json", "CMakeLists.txt", ".git" },
})

vim.lsp.enable("clangd")
