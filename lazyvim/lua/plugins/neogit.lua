return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim", -- required
    "sindrets/diffview.nvim", -- optional - Diff integration

    -- Only one of these is needed.
    "folke/snacks.nvim", -- optional
  },
  keys = {
    { "<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit" },
  },
}
