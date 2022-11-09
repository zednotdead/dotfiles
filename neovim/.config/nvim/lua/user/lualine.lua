local status_ok, l = pcall(require, "lualine")
if not status_ok then
  return
end

l.setup {
  options = {
    theme = 'base16',
    disabled_filetypes = { "neo-tree", "dashboard" },
  }
}
