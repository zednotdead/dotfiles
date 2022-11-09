local status_ok, neotree = pcall(require, "neo-tree")
if not status_ok then
  return
end

neotree.setup({
  default_component_configs = {
	  indent_size = 2,
	  padding = 1,
  }
})
