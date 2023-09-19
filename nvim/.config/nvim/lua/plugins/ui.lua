return {
  "lukas-reineke/indent-blankline.nvim",
  {
    'stevearc/dressing.nvim',
  },
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.3',
    dependencies = { 'nvim-lua/plenary.nvim' },
  },
  {
    "rcarriga/nvim-notify",
    config = true,
    opts = {},
  },
  {
    "gennaro-tedesco/nvim-possession",
    dependencies = {
      "ibhagwan/fzf-lua",
    },
    config = true,
    init = function()
      if vim.fn.isdirectory(vim.fn.stdpath("data") .. "/sessions/") == 0 then
        vim.fn.mkdir(vim.fn.stdpath("data") .. "/sessions/")
      end

      local possession = require("nvim-possession")
      vim.keymap.set("n", "<leader>sl", function()
        possession.list()
      end, { desc = "List sessions" })
      vim.keymap.set("n", "<leader>sn", function()
        possession.new()
      end, { desc = "Save session" })
      vim.keymap.set("n", "<leader>su", function()
        possession.update()
      end, { desc = "Update session" })
      vim.keymap.set("n", "<leader>sd", function()
        possession.delete()
      end, { desc = "Update session" })
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {}
  },
  {
    'goolord/alpha-nvim',
    config = function ()
      require'alpha'.setup(require'alpha.themes.dashboard'.config)
    end
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      { "s", mode = { "n", "o", "x" }, function() require("flash").jump() end, desc = "Flash" },
      { "S", mode = { "n", "o", "x" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
      { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    },
  },
}
