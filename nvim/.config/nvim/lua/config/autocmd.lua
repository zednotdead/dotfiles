local prev = { new_name = "", old_name = "" } -- Prevents duplicate events

vim.api.nvim_create_autocmd("User", {
  pattern = "NvimTreeSetup",
  callback = function()
    local events = require("nvim-tree.api").events
    events.subscribe(events.Event.NodeRenamed, function(data)
      if prev.new_name ~= data.new_name or prev.old_name ~= data.old_name then
        data = data
        ---@module 'snacks'
        Snacks.rename.on_rename_file(data.old_name, data.new_name)
      end
    end)
  end,
})

vim.api.nvim_create_autocmd({ 'BufEnter' }, {
  pattern = 'NvimTree*',
  callback = function()
    local api = require('nvim-tree.api')

    if not api.tree.is_visible() then
      api.tree.open()
    end
  end,
})
