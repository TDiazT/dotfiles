return {
  "whonore/Coqtail",
  config = function(_, opts)
    local wk = require("which-key")
    wk.add({
      mode = { "n" },
      { "<leader>r", group = "Rocq" },
      { "<leader>rj", "<cmd>RocqNext<cr>", desc = "Step forward" },
      { "<leader>rk", "<cmd>RocqUndo<cr>", desc = "Step back" },
      { "<leader>r<cr>", "<cmd>RocqToLine<cr>", desc = "Step to point" },
      { "<leader>rs", "<cmd>RocqStop<cr>", desc = "Stop" },
      { "<leader>ri", "<cmd>RocqInterrupt<cr>", desc = "Interrupt" },
      {
        "<leader>r<space>",
        function()
          wk.show({ keys = "<leader>r", loop = true })
        end,
        desc = "Rocq Hydra Mode (which-key)",
      },
    }, opts)
  end,
}
