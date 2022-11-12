local config_group = vim.api.nvim_create_augroup('MyConfigGroup', {}) -- A global group for all your config autocommands

vim.api.nvim_create_autocmd({ 'User' }, {
  pattern = "SessionLoadPost",
  group = config_group,
  callback = function()
    vim.cmd("Neotree action=show")
  end,
})

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]
vim.api.nvim_create_autocmd(
  "FileType",
  {
    pattern = { 'alpha' },
    command = "setlocal fillchars=eob:\\ "
  }
)
