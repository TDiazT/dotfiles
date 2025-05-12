return {
  opts = {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      -- add Ocaml
      vim.list_extend(opts.ensure_installed, {
        "ocaml",
      })
    end,
  },
}
