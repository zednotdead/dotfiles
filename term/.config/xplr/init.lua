version = "0.21.1"
---@diagnostic disable
local xplr = xplr
---@diagnostic enable

xplr.config.node_types.directory.meta.icon = ""
xplr.config.node_types.mime_essence = {
  application = {
    pdf = { meta = { icon = "" }, style = { fg = "Blue" } },
  }
}
xplr.config.node_types.extension = {
  rs = { meta = { icon = "" } },
  yaml = { meta = { icon = "" } },
  yml = { meta = { icon = "" } },
  toml = { meta = { icon = "" } },
  lock = { meta = { icon = "" } },
}
