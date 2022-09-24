fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/diff-so-fancy"
fish_add_path "$HOME/.fzf/bin"
fish_add_path "/snap/bin"
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set -gx ANDROID_SDK_ROOT "/opt/android-sdk"
fish_add_path "$ANDROID_SDK_ROOT/platform-tools"
fish_add_path "$ANDROID_SDK_ROOT/tools/bin"
fish_add_path "$ANDROID_SDK_ROOT/emulator"
fish_add_path "$ANDROID_SDK_ROOT/tools"
fish_add_path "$HOME/flutter/flutter/bin"
fish_add_path "/opt/local/bin"

export MOZ_ENABLE_WAYLAND=1

fish_vi_key_bindings
