local status_ok, bl = pcall(require, "bufferline")
if not status_ok then
  return
end

bl.setup {}
