return {
  {
    "goolord/alpha-nvim",
    enabled = true,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VimEnter",
    config = function()
      local theme = require("alpha.themes.dashboard")

      theme.section.header.val = {
        [[　　　 　　/＾>》, -―‐‐＜＾}]],
        [[　　　 　./:::/,≠´::::::ヽ.]],
        [[　　　　/::::〃::::／}::丿ハ]],
        [[　　　./:::::i{l|／　ﾉ／ }::}]],
        [[　　 /:::::::瓜イ＞　´＜ ,:ﾉ]],
        [[　 ./::::::|ﾉﾍ.{､　(_ﾌ_ノﾉイ＿]],
        [[　 |:::::::|／}｀ｽ /          /]],
        [[.　|::::::|(_:::つ/    🐧    /　neovim!]],
        [[.￣￣￣￣￣￣￣＼/＿＿＿＿＿/￣￣￣￣￣]],
      }
      theme.section.buttons.val = {
        theme.button(
          "f",
          "  Find project",
          ":lua require'custom_telescope.project'.projects {}<CR>",
          {}
        ),

        theme.button(
          "r",
          "  Recent files",
          ":lua require'telescope.builtin'.oldfiles{}<CR>",
          {}
        ),

        theme.button(
          "q",
          "  Quit",
          ":qa<CR>",
          {}
        ),
      }
      require("alpha").setup(theme.config)
    end
  }
}
