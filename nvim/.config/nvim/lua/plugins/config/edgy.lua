local edgy = require("edgy")

---@type Edgy.Config
local config = {
  wo = {
    winhighlight = ""
  },
  ---@type table<Edgy.Pos, {size:integer | fun():integer, wo:vim.wo}>
  options = {
    left = { size = 30 },
    bottom = { size = 10 },
    right = { size = 40 },
    top = { size = 10 },
  },
  ---@type (Edgy.View.Opts|string)[]
  bottom = {
    {
      ft = "snacks_terminal",
      size = { height = 0.3 },
      filter = function(_, win)
        return vim.api.nvim_win_get_config(win).relative == ""
      end,
      wo = { winhighlight = "" }
    },
    "Trouble",
    {
      ft = "qf",
      ---@diagnostic disable-next-line: assign-type-mismatch
      title = "QuickFix"
    },
    {
      ft = "help",
      size = { height = 20 },
      -- only show help buffers
      filter = function(buf)
        return vim.bo[buf].buftype == "help"
      end,
    },
    { ft = "spectre_panel", size = { height = 0.4 } },
  },
  ---@type (Edgy.View.Opts|string)[]
  left = {
    {
      ---@diagnostic disable-next-line: assign-type-mismatch
      title = "Neo-Tree",
      ft = "neo-tree",
      filter = function(buf)
        return vim.b[buf].neo_tree_source == "filesystem"
      end,
      pinned = true,
      open = "Neotree position=left action=open"
    },
  },
  ---@type (Edgy.View.Opts|string)[]
  right = {
    {
      title = function()
        local buf_name = vim.api.nvim_buf_get_name(0) or "[No Name]"
        return vim.fn.fnamemodify(buf_name, ":t")
      end,
      ft = "Outline",
      pinned = true,
      open = "OutlineOpen"
    },
  }
}

edgy.setup(config)
