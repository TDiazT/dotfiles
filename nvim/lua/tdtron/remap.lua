vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- Opens help for the current word under the cursor
vim.keymap.set("n", "<leader>h", function ()
    local word = vim.fn.expand("<cword>")
    vim.cmd('h ' .. word)
end)

