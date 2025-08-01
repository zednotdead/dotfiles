return {
  {
    "rmagatti/auto-session",
    lazy = false,
    config = function()
      require("plugins.config.auto-session")
    end,
  },
  {
    'pwntester/octo.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'nvim-tree/nvim-web-devicons',
    },
    opts = {},
    config = function (_, opts)
      local octo = require("octo")
      octo.setup(opts)
      vim.keymap.set("n", "<leader>gi", "<Cmd>Octo issues list<cr>", { desc = "Github issues", remap = true })
      vim.keymap.set("n", "<leader>gp", "<Cmd>Octo pr list<cr>", { desc = "Github pull requests", remap = true })
    end
  }
}
