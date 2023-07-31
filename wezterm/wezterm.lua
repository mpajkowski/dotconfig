local wezterm = require('wezterm')

local config = {}

config.font = wezterm.font('Monaco')
config.font_size = 15

config.color_scheme = 'Tomorrow Night'
config.tab_bar_at_bottom = true

function normalize_mods(mods)
    if string.find(wezterm.target_triple, 'darwin') then
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
