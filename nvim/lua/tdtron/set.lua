-- UI config
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.cursorline = true           -- highlight cursor line underneath the cursor horizontally
vim.opt.termguicolors = true

-- Tab
vim.opt.tabstop = 4                 -- number of visual spaces per TAB
vim.opt.softtabstop = 4             -- number of spacesin tab when editing
vim.opt.shiftwidth = 4              -- insert 4 spaces on a tab
vim.opt.expandtab = true            -- tabs are spaces

vim.opt.smartindent = true

vim.opt.wrap = false

-- Searching
vim.opt.incsearch = true            -- search as characters are entered
vim.opt.hlsearch = false            -- do not highlight matches
vim.opt.ignorecase = true           -- ignore case in searches by default
vim.opt.smartcase = true            -- but make it case sensitive if an uppercase is entered


vim.opt.scrolloff = 999
vim.opt.signcolumn = "yes"

vim.opt.updatetime = 50

-- Displays column at X char
-- vim.opt.colorcolumn = "80"
