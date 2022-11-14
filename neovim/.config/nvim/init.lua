require "user.options"
require "user.base-keys"
require "user.plugins"
require "user.ui"
require "user.keymaps"
require "user.theme"
require "user.bufferline"
require "user.neotree"
require "user.lualine"
require "user.dashboard"
require "user.sessions"
require "user.cmp"
require "user.lsp"
require "user.autocommands"
require "user.misc"
require "user.comment"
require "user.terminal"

-- Load Neovide settings, if Neovide is opened
if (vim.g.neovide) then
  require "user.neovide"
end
