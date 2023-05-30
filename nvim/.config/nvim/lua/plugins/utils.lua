return {
  { "tpope/vim-surround" },
  {
    -- CODE COMMENTS
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()

      vim.keymap.set("n", "<leader><leader><leader>", "<Cmd>norm gcc<CR>",
        { desc = "comment a single line" })
      vim.keymap.set("v", "<leader><leader><leader>",
        "<Plug>(comment_toggle_linewise_visual)",
        { desc = "comment multiple lines" })
    end
  },
  { "RRethy/vim-illuminate" },
  {
    "folke/which-key.nvim",
    config = function()
      local wk = require("which-key")

      wk.register({
        ["<leader>l"] = { name = "lsp" },
        ["<leader>g"] = { name = "git" },
        ["<leader>f"] = { name = "find" },
        ["<leader>m"] = { name = "minimap" },
      })
    end
  },
  { "folke/neodev.nvim" },
  {
    "nvim-tree/nvim-web-devicons",
    version = "nerd-v2-compat",
  },
}
