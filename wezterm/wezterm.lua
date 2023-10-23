local wezterm = require('wezterm')

function isdarwin()
    return string.find(wezterm.target_triple, 'darwin')
end

local config = {}

config.font = wezterm.font('Monaco')

if isdarwin() then
    config.font_size = 15
else
    config.font_size = 12
end

config.window_padding = {
    left = 2,
    right = 2,
    top = 0,
    bottom = 0,
}

config.color_scheme = 'Tomorrow Night'

function normalize_mods(mods)
    if isdarwin() then
        return string.gsub(mods, 'ALT', 'CMD')
    else
        return mods
    end
end

config.keys = {
    {
        key = 'd',
        mods = normalize_mods 'SHIFT|ALT',
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
        key = 'r',
        mods = normalize_mods 'SHIFT|ALT',
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
}

return config
