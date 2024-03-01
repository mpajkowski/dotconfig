fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/diff-so-fancy"
fish_add_path "$HOME/.fzf/bin"
fish_add_path "$HOME/zig"
fish_add_path "$HOME/zls"
fish_add_path "/snap/bin"
fish_add_path "/opt/homebrew/opt/openjdk/bin"
set -gx ANDROID_SDK_ROOT "/opt/android-sdk"
fish_add_path "$ANDROID_SDK_ROOT/platform-tools"
fish_add_path "$ANDROID_SDK_ROOT/tools/bin"
fish_add_path "$ANDROID_SDK_ROOT/emulator"
fish_add_path "$ANDROID_SDK_ROOT/tools"
fish_add_path "$HOME/flutter/flutter/bin"
fish_add_path "/opt/local/bin"
fish_add_path "/opt/homebrew/bin"
fish_add_path "/opt/homebrew/opt/coreutils/libexec/gnubin"
fish_add_path "/opt/homebrew/opt/gnu-sed/libexec/gnubin"
fish_add_path "/Applications/Emacs.app/Contents/MacOS/bin"

export DYLD_INSERT_LIBRARIES=/opt/homebrew/opt/curl/lib/libcurl.dylib
export USE_GKE_GCLOUD_AUTH_PLUGIN=True

if [ "$INSIDE_EMACS" = 'vterm' ]
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

    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end

    function vterm_cmd --description 'Run an Emacs command among the ones been defined in vterm-eval-cmds.'
        set -l vterm_elisp ()
        for arg in $argv
            set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
        end
        vterm_printf '51;E'(string join '' $vterm_elisp)
    end
else
    fish_vi_key_bindings
end

zoxide init fish | source
