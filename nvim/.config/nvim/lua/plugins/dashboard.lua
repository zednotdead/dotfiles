return {
  {
    'glepnir/dashboard-nvim',
    event = 'VimEnter',
    config = function()
      require('dashboard').setup {
        config = {
          project = {
            enable = true,
            limit = 8,
            label = 'Projects',
            action = 'Telescope git_files prompt_prefix=🔍 cwd='
          },
          packages = { enable = true }
        },
      }
    end,
    dependencies = { { 'nvim-tree/nvim-web-devicons' } }
  }
}
