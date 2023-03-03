fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/diff-so-fancy"
fish_add_path "$HOME/.fzf/bin"
fish_add_path "$HOME/zig"
fish_add_path "$HOME/zls"
fish_add_path "/snap/bin"
fish_add_path /opt/homebrew/opt/openjdk/bin
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set -gx ANDROID_SDK_ROOT "/opt/android-sdk"
fish_add_path "$ANDROID_SDK_ROOT/platform-tools"
fish_add_path "$ANDROID_SDK_ROOT/tools/bin"
fish_add_path "$ANDROID_SDK_ROOT/emulator"
fish_add_path "$ANDROID_SDK_ROOT/tools"
fish_add_path "$HOME/flutter/flutter/bin"
fish_add_path "/opt/local/bin"
fish_add_path "/opt/homebrew/bin"

export MOZ_ENABLE_WAYLAND=1

fish_vi_key_bindings

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

function fish_right_prompt -d "Write out the right prompt"
    kubectl config get-contexts | grep '*' | awk '{ print $2 }'
end
