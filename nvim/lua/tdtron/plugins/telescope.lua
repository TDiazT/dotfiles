return {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = function()
        require('telescope').setup({
            defaults = {
                mappings = {
                    i = {
                        ["<C-j>"] = "move_selection_next",
                        ["<C-k>"] = "move_selection_previous"
                    }
                }
            },
        })

        local builtin = require('telescope.builtin')

        vim.keymap.set('n', '<leader>pf', builtin.find_files, { desc = 'Find files' })
        vim.keymap.set('n', '<leader>pg', builtin.git_files, { desc = 'Find git-tracked files' })
        vim.keymap.set('n', '<leader>pws', function()
            local word = vim.fn.expand("<cword>")
            builtin.grep_string({ search = word })
        end, { desc = 'Find word under cursor' })
        vim.keymap.set('n', '<leader>pWs', function()
            local word = vim.fn.expand("<cWORD>")
            builtin.grep_string({ search = word })
        end, { desc = 'Find WORD under cursor' })
        vim.keymap.set('n', '<leader>ps', builtin.live_grep, { desc = 'Live grep' })
        vim.keymap.set('n', '<leader>vh', builtin.help_tags, { desc = 'Show tags' })
        vim.keymap.set('n', '<leader>pd', builtin.diagnostics, { desc = 'Show diagnostics' })
    end

}
