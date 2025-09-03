return {
  "whonore/Coqtail",
  config = function(_, opts)
    local wk = require("which-key")
    wk.add({
      mode = { "n", "i" },
      { "<C-r>", group = "Rocq" },
      { "<C-r><C-j>", "<cmd>RocqNext<cr>", desc = "Step forward" },
      { "<C-r><C-k>", "<cmd>RocqUndo<cr>", desc = "Step back" },
      { "<C-r><C-cr>", "<cmd>RocqToLine<cr>", desc = "Step to point" },
      { "<C-r><C-s>", "<cmd>RocqStop<cr>", desc = "Stop" },
      { "<C-r><C-i>", "<cmd>RocqInterrupt<cr>", desc = "Interrupt" },
      {
        "<C-r><space>",
        function()
          wk.show({ keys = "<C-r>", loop = true })
        end,
        desc = "Rocq Hydra Mode (which-key)",
      },
    }, opts)
  end,
}
