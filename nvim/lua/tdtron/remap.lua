vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = 'Returns to file explorer' })

-- Opens help for the current word under the cursor
vim.keymap.set("n", "<leader>h", function()
    local word = vim.fn.expand("<cword>")
    vim.cmd('h ' .. word)
end, { desc = "Opens help for the word under the cursor" })
