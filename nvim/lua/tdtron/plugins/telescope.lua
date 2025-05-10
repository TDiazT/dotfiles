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

		vim.keymap.set('n', '<leader>pf', builtin.find_files, {})
		vim.keymap.set('n', '<leader>pg', builtin.git_files, {})
		vim.keymap.set('n', '<leader>pws', function()
			local word = vim.fn.expand("<cword>")
			builtin.grep_string({ search = word })
		end)
		vim.keymap.set('n', '<leader>pWs', function()
			local word = vim.fn.expand("<cWORD>")
			builtin.grep_string({ search = word })
		end)
		vim.keymap.set('n', '<leader>ps', builtin.live_grep, {}) 
		vim.keymap.set('n', '<leader>vh', builtin.help_tags, {})
		vim.keymap.set('n', '<leader>pd', builtin.diagnostics, {})

	end

}
