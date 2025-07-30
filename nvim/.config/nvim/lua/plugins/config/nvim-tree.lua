local nvimtree = require "nvim-tree"
local api = require "nvim-tree.api"

local function my_on_attach(bufnr)
  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  api.config.mappings.default_on_attach(bufnr)

  vim.keymap.set('n', '<C-t>', api.tree.change_root_to_parent, opts('Up'))
  vim.keymap.set('n', '?', api.tree.toggle_help, opts('Help'))
end

local prev = { new_name = "", old_name = "" } -- Prevents duplicate events
vim.api.nvim_create_autocmd("User", {
  pattern = "NvimTreeSetup",
  callback = function()
    local events = require("nvim-tree.api").events
    events.subscribe(events.Event.NodeRenamed, function(data)
      if prev.new_name ~= data.new_name or prev.old_name ~= data.old_name then
        data = data
        Snacks.rename.on_rename_file(data.old_name, data.new_name)
      end
    end)
  end,
})

vim.keymap.set("n",
  "<Leader><Tab>", ":NvimTreeToggle<cr>",
  { silent = true, noremap = true, desc = "Open nvim-tree" }
)

local opts = {
  on_attach = my_on_attach,
  hijack_cursor = true,
  sync_root_with_cwd = true,
  reload_on_bufenter = true,
  respect_buf_cwd = true,
  view = {
    width = 40
  },
  renderer = {
    group_empty = true,
    special_files = { "Cargo.toml", "Makefile", "README.md", "readme.md", "package.json" },
  },
  git = {
    enable = true,
  },
  diagnostics = {
    enable = true,
    show_on_dirs = true,
  },
  update_focused_file = {
    enable = true,
  },
}

nvimtree.setup(opts)
