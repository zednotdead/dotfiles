local telescope = require("telescope")
local builtins = require("telescope.builtin")
local wk = require("which-key")
local telescopeConfig = require("telescope.config")

-- Clone the default Telescope configuration
local vimgrep_arguments = { unpack(telescopeConfig.values.vimgrep_arguments) }

-- I want to search in hidden/dot files.
table.insert(vimgrep_arguments, "--hidden")
-- I don't want to search in the `.git` directory.
table.insert(vimgrep_arguments, "--glob")
table.insert(vimgrep_arguments, "!**/.git/*")

telescope.setup({
  defaults = {
    -- `hidden = true` is not supported in text grep commands.
    vimgrep_arguments = vimgrep_arguments,
  },
  pickers = {
    find_files = {
      -- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
      find_command = { "rg", "--files", "--hidden", "--glob", "!**/.git/*" },
    },
  },
})

wk.add({
  { "<leader>f", group = "find" }
})

vim.keymap.set('n', '<leader>fi', function () Snacks.picker.git_files({ untracked = true }) end, { desc = 'Find files' })
vim.keymap.set('n', '<leader>ff', function () Snacks.picker.git_grep({ untracked = true }) end, { desc = 'Live grep' })
vim.keymap.set('n', '<leader>fb', builtins.buffers, { desc = 'Buffers' })
