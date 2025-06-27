return {
  "Pocco81/auto-save.nvim",
  lazy = false,
  opts = {
    enabled = false,
    debounce_delay = 500,
    execution_message = {
      message = function()
        return ""
      end,
    },
    keys = {
      { "<leader>uv", "<cmd>ASToggle<CR>", desc = "Toggle autosave" },
    },
  },
}
