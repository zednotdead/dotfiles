local wk = require("which-key")
wk.register(mappings, opts)

wk.register({
  ["<leader>"] = {
		o = { "<cmd>Neotree position=left action=focus toggle=true<cr>", "Toggle NeoTree" },
    x = { "<cmd>Bwipeout<cr>", "Close buffer" },
    d = { "<cmd>Neotree position=left action=close<bar>%bd<bar>Alpha<cr>", "Show dashboard" },
    f = {
      name = "+find",
      f = { "<cmd>Telescope find_files<cr>", "Find files" },
      g = { "<cmd>Telescope grep_string<cr>", "Find words in file" },
    },
    s = {
      name = "+session",
      s = { "<cmd>SessionManager save_current_session<cr>", "Save current session", },
      l = { "<cmd>SessionManager load_session<cr>", "Load session", },
      d = { "<cmd>SessionManager delete_session<cr>", "Delete session", }
    },
    p = { "<cmd>Telescope find_files<cr>", "Find files" },
    t = {
      name = "+toggle",
		  o = { "<cmd>Neotree position=left action=close<cr>", "Close NeoTree" },
      f = { "<cmd>Telescope find_files<cr>", "Find files" },
    },
    l = {
      name = "+lsp",
    },
    qq = { "<cmd>wqa!<cr>", "Close everything" }
	},
  ["<C-p>"] = { "<cmd>Telescope find_files<cr>", "Find files" },
  ["<C-f>"] = { "<cmd>Telescope grep_string<cr>", "Find words in file" },
  ["/"] = { "<cmd>lua require('Comment.api').toggle.linewise.current()<CR>", "Toggle comment" }
})

wk.register(
  {
    ["/"] = { '<ESC><CMD>lua require("Comment.api").toggle.linewise(vim.fn.visualmode())<CR>', "Toggle comment" }
  },
  { mode = "x" }
)
