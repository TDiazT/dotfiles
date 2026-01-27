local function format_hunks()
  local ignore_filetypes = { "lua" }
  if vim.tbl_contains(ignore_filetypes, vim.bo.filetype) then
    vim.notify("range formatting for " .. vim.bo.filetype .. " not working properly.")
    return
  end

  local hunks = require("gitsigns").get_hunks()
  if hunks == nil then
    vim.notify("No hunks")
    return
  end

  local function format_range()
    if next(hunks) == nil then
      vim.notify("done formatting git hunks", "info", { title = "formatting" })
      return
    end
    local hunk = nil
    while next(hunks) ~= nil and (hunk == nil or hunk.type == "delete") do
      hunk = table.remove(hunks)
    end

    if hunk ~= nil and hunk.type ~= "delete" then
      local start = hunk.added.start
      local last = start + hunk.added.count

      local path = vim.fn.expand("%")
      local cmd_replace = "ocp-indent " .. path .. " --lines " .. start .. "-" .. last .. " --inplace"
      vim.fn.system(cmd_replace)

      if not vim.bo.modified then
        vim.cmd("checktime")
      else
        vim.notify("Buffer tiene cambios; no recargo para no perderlos.")
      end
    end
  end

  format_range()
end

vim.api.nvim_create_user_command("DiffFormat", format_hunks, { desc = "Format changed lines" })

-- run format on save, only for OCaml
-- vim.api.nvim_create_autocmd("BufWritePost", {
--   group = vim.api.nvim_create_augroup("DiffFormatOnSave", { clear = true }),
--   pattern = { "*.ml", "*.mli" },
--   callback = function(args)
--     if vim.bo[args.buf].filetype ~= "ocaml" then
--       return
--     end
--     format_hunks()
--   end,
-- })
