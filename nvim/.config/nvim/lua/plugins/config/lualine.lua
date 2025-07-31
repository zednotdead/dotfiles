local gruvbox = require("gruvbox")
local lualine = require("lualine")

local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

local opts = {
  options = {
    disabled_filetypes = { 'NvimTree' },
    -- Disable sections and component separators
    component_separators = '',
    section_separators = '',
    theme = {
      normal = {
        a = { bg = gruvbox.palette.dark2, fg = gruvbox.palette.light2 },
        b = { bg = gruvbox.palette.dark1, fg = gruvbox.palette.light1 },
        c = { bg = gruvbox.palette.dark0, fg = gruvbox.palette.light0 },
        x = { bg = gruvbox.palette.dark0, fg = gruvbox.palette.light0 },
        y = { bg = gruvbox.palette.dark1, fg = gruvbox.palette.light1 },
        z = { bg = gruvbox.palette.dark2, fg = gruvbox.palette.light2 },
      },
      inactive = {
        a = { bg = gruvbox.palette.dark2, fg = gruvbox.palette.light2 },
        b = { bg = gruvbox.palette.dark1, fg = gruvbox.palette.light1 },
        c = { bg = gruvbox.palette.dark0, fg = gruvbox.palette.light0 },
        x = { bg = gruvbox.palette.dark0, fg = gruvbox.palette.light0 },
        y = { bg = gruvbox.palette.dark1, fg = gruvbox.palette.light1 },
        z = { bg = gruvbox.palette.dark2, fg = gruvbox.palette.light2 },
      },
    }
  },
  sections = {
    lualine_a = {
      {
        function()
          return '▊'
        end,
        color = { fg = gruvbox.palette.neutral_aqua },
        padding = { left = 0, right = 0 },
      },
      {
        "mode",
        icon = '',
        color = function()
          -- auto change color according to neovims mode
          local mode_color = {
            n = gruvbox.palette.bright_red,
            i = gruvbox.palette.bright_green,
            v = gruvbox.palette.bright_blue,
            [''] = gruvbox.palette.bright_blue,
            V = gruvbox.palette.bright_blue,
            c = gruvbox.palette.bright_purple,
            no = gruvbox.palette.bright_red,
            s = gruvbox.palette.bright_orange,
            S = gruvbox.palette.bright_orange,
            [''] = gruvbox.palette.bright_orange,
            ic = gruvbox.palette.bright_yellow,
            R = gruvbox.palette.bright_purple,
            Rv = gruvbox.palette.bright_purple,
            cv = gruvbox.palette.bright_red,
            ce = gruvbox.palette.bright_red,
            r = gruvbox.palette.bright_blue,
            rm = gruvbox.palette.bright_blue,
            ['r?'] = gruvbox.palette.bright_blue,
            ['!'] = gruvbox.palette.bright_red,
            t = gruvbox.palette.bright_red,
          }
          return { fg = mode_color[vim.fn.mode()] }
        end,
        padding = { left = 1, right = 1 },
      },
      "filename",
    },
    lualine_b = {
      "location",
    },
    lualine_c = {
      {
        -- Lsp server name .
        function()
          local msg = 'No Active Lsp'
          local buf_ft = vim.api.nvim_get_option_value('filetype', { buf = 0 })
          local clients = vim.lsp.get_clients()
          if next(clients) == nil then
            return msg
          end
          for _, client in ipairs(clients) do
            local filetypes = client.config.filetypes
            if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
              return client.name
            end
          end
          return msg
        end,
        icon = ' LSP:',
        color = { fg = gruvbox.palette.bright_purple },
      },
    },
    lualine_x = {
      {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = { error = ' ', warn = ' ', info = ' ' },
        diagnostics_color = {
          error = { fg = gruvbox.palette.bright_red },
          warn = { fg = gruvbox.palette.bright_yellow },
          info = { fg = gruvbox.palette.bright_blue },
        },
      },
    },
    lualine_y = {
      {
        'diff',
        -- Is it me or the symbol for modified us really weird
        symbols = { added = ' ', modified = '󰝤 ', removed = ' ' },
        diff_color = {
          added = { fg = gruvbox.palette.bright_green },
          modified = { fg = gruvbox.palette.bright_orange },
          removed = { fg = gruvbox.palette.bright_red },
        },
        cond = conditions.hide_in_width,
      },
      {
        'branch', -- option component same as &encoding in viml
        icon = '',
        color = { fg = gruvbox.palette.bright_purple },
      },
    },
    lualine_z = {
      {
        'o:encoding',       -- option component same as &encoding in viml
        fmt = string.upper, -- I'm not sure why it's upper case either ;)
        cond = conditions.hide_in_width,
        color = { fg = gruvbox.palette.bright_green }
      },
      {
        function()
          return '▊'
        end,
        color = { fg = gruvbox.palette.neutral_aqua },
        padding = { left = 0, right = 0 },
      },
    },
  },
}

lualine.setup(opts)
