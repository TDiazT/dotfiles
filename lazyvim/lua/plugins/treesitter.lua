return {
  "nvim-treesitter/nvim-treesitter",
  opts = function(_, opts)
    -- add Ocaml
    vim.list_extend(opts.ensure_installed, {
      "ocaml",
      "ocaml_interface",
      "latex",
      "bibtex",
      "gitcommit",
      "gitignore",
    })
  end,
}
