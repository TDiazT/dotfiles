return {
  "lervag/vimtex",
  lazy = false, -- we don't want to lazy load VimTeX
  -- tag = "v2.15", -- uncomment to pin to a specific release
  init = function()
    -- VimTeX configuration goes here, e.g.
    local view_method
    if vim.fn.has("mac") then
      view_method = "skim"
    elseif vim.fn.has("linux") then
      view_method = "zathura"
    else
      view_method = ""
    end

    vim.g.vimtex_view_method = view_method
  end,
}
