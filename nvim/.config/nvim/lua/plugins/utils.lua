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
  { "folke/which-key.nvim", config = true },
  { "folke/neodev.nvim" },
  {
    "nvim-tree/nvim-web-devicons",
    version = "nerd-v2-compat",
  },
}
