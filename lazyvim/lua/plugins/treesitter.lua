-- Possible solution to problems when updating ?
-- 1. Treesitter parser .so files in ~/.local/share/nvim/site/parser/ with invalid code signatures (macOS kills nvim when loading them)
-- 2. blink.cmp's pre-built binary also needed to be downloaded fresh
local function codesign_parsers()
  if vim.fn.has("mac") == 1 then
    local parser_dir = vim.fn.stdpath("data") .. "/site/parser"
    vim.fn.system("codesign -f -s - " .. parser_dir .. "/*.so 2>/dev/null")
  end
end

return {
  "nvim-treesitter/nvim-treesitter",
  build = function()
    vim.cmd("TSUpdate")
    codesign_parsers()
  end,
  opts = function(_, opts)
    -- add Ocaml
    vim.list_extend(opts.ensure_installed, {
      "ocaml",
      "ocaml_interface",
      "bibtex",
      "gitcommit",
      "gitignore",
      "cpp",
      "c",
      "cmake",
    })
  end,
}
