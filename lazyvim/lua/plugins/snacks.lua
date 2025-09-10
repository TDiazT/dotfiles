return {
  "folke/snacks.nvim",
  opts = {
    picker = {
      sources = {
        files = {
          hidden = true,
          ignored = true,
          exclude = { "node_modules", ".git", "_build", "*.vo*", "*.glob", "*.aux" },
        },
        explorer = {
          hidden = true,
          ignored = true,
          exclude = { "node_modules", ".git", "_build", "*.vo*", "*.glob", "*.aux" },
        },
      },
    },
  },
}
