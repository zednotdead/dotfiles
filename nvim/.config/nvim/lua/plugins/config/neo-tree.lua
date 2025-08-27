local nt = require("neo-tree")
local cmd = require("neo-tree.command")

nt.setup({
  open_files_do_not_replace_types = { "terminal", "Trouble", "qf", "edgy" },
  filesystem = {
    filtered_items = {
      hide_dotfiles = false,
      hide_by_pattern = {
        ".git"
      },
    },
    follow_current_file = {
      enabled = true,
      leave_dirs_open = true
    },
    group_empty_dirs = true,
  }
})

vim.keymap.set("n",
  "<Leader><Tab>",
  function()
    cmd.execute({
      action = "focus",
      position = "left",
      toggle = true,
    })
  end,
  { silent = true, noremap = true, desc = "Open neotree" }
)
