-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
  callback = function()
    vim.cmd("checktime")
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "ocaml",
  callback = function()
    local ocp_indent_share = vim.fn.system("opam var ocp-indent:share"):gsub("%s+$", "")
    local ocp_indent_vim = ocp_indent_share .. "/vim"

    -- evita duplicados por si acaso
    if not vim.tbl_contains(vim.opt.rtp:get(), ocp_indent_vim) then
      vim.opt.rtp:prepend(ocp_indent_vim)
    end

    vim.opt.rtp:prepend(ocp_indent_vim)
    vim.g.autoformat = false -- Disable autoformatting for OCaml because it's a mess
  end,
})
