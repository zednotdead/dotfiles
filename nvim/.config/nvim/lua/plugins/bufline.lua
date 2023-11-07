-- stolen from: https://github.com/jim-at-jibba/my-dots/blob/master/nvim/lua/plugins/nvim-cokeline.lua

return {
	"noib3/nvim-cokeline",
	dependencies = "nvim-tree/nvim-web-devicons", -- If you want devicons
	event = "VeryLazy",
	config = function()
		local get_hex = require("cokeline.hlgroups").get_hl_attr
		local is_picking_focus = require("cokeline.mappings").is_picking_focus
		local is_picking_close = require("cokeline.mappings").is_picking_close

		local comments_fg = get_hex("Comment", "fg")
		local errors_fg = get_hex("DiagnosticError", "fg")
		local warnings_fg = get_hex("DiagnosticWarn", "fg")

		local red = vim.g.terminal_color_1
		local yellow = vim.g.terminal_color_3
		local green = vim.g.terminal_color_5

		local buffer_line_background_hl = "StatusLine"
		local buffer_line_background = get_hex(buffer_line_background_hl, "bg")
		local unselected_tab_background = get_hex("TabLine", "bg")
		local selected_tab_background = get_hex("Normal", "bg")

		local function separator_generator(separator)
			return {
				text = separator,
				truncation = { priority = 1 },
				fg = function(buffer)
					return buffer.is_focused and selected_tab_background or unselected_tab_background
				end,
				bg = buffer_line_background,
			}
		end

		local components = {
			space = {
				text = " ",
				truncation = { priority = 1 },
			},

			two_spaces = {
				text = "  ",
				truncation = { priority = 1 },
			},

			separator_front = separator_generator(""),
			separator_back = separator_generator(""),
			devicon = {
				text = function(buffer)
					return (is_picking_focus() or is_picking_close()) and buffer.pick_letter .. " "
						or buffer.devicon.icon
				end,
				fg = function(buffer)
					return (is_picking_focus() and yellow) or (is_picking_close() and red) or buffer.devicon.color
				end,
				style = function(_)
					return (is_picking_focus() or is_picking_close()) and "italic,bold" or nil
				end,
				truncation = { priority = 1 },
			},

			index = {
				text = function(buffer)
					return buffer.index .. ": "
				end,
				truncation = { priority = 1 },
			},

			unique_prefix = {
				text = function(buffer)
					return buffer.unique_prefix
				end,
				fg = comments_fg,
				style = "italic",
				truncation = {
					priority = 3,
					direction = "left",
				},
			},

			filename = {
				text = function(buffer)
					return buffer.filename
				end,
				style = function(buffer)
					return ((buffer.is_focused and buffer.diagnostics.errors ~= 0) and "bold,underline")
						or (buffer.is_focused and "bold")
						or (buffer.diagnostics.errors ~= 0 and "underline")
						or nil
				end,
				truncation = {
					priority = 2,
					direction = "left",
				},
			},
			diagnostics = {
				text = function(buffer)
					return (buffer.diagnostics.errors ~= 0 and "  " .. buffer.diagnostics.errors)
						or (buffer.diagnostics.warnings ~= 0 and "  " .. buffer.diagnostics.warnings)
						or ""
				end,
				fg = function(buffer)
					return (buffer.diagnostics.errors ~= 0 and errors_fg)
						or (buffer.diagnostics.warnings ~= 0 and warnings_fg)
						or nil
				end,
				truncation = { priority = 1 },
			},

			close_or_unsaved = {
				text = function(buffer)
					return buffer.is_modified and "●" or ""
				end,
				fg = function(buffer)
					return buffer.is_modified and green or nil
				end,
				delete_buffer_on_left_click = true,
				truncation = { priority = 1 },
			},
		}

		require("cokeline").setup({
			show_if_buffers_are_at_least = 1,

			buffers = {
				filter_valid = function(buffer) return buffer.type ~= 'terminal' end,
				filter_visible = function(buffer) return buffer.type ~= 'terminal' end,
			},
			sidebar = {
				filetype = { "NvimTree" },
				components = {
					{
						text = "",
					},
				},
			},
			rendering = {
				max_buffer_width = 30,
			},
			default_hl = {
				fg = function(buffer)
					return buffer.is_focused and get_hex("Normal", "fg") or get_hex("TabLine", "fg")
				end,
				bg = function(buffer)
					return buffer.is_focused and selected_tab_background or unselected_tab_background
				end,
			},
			fill_hl = buffer_line_background_hl,
			components = {
				components.separator_front,
				components.space,
				components.devicon,
				components.space,
				components.index,
				components.unique_prefix,
				components.filename,
				components.diagnostics,
				components.two_spaces,
				components.close_or_unsaved,
				components.two_spaces,
				components.separator_back,
			},
		})
	end,
}
