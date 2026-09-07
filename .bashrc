# change shell to zsh
if [[ $- == *i* ]]; then
    export SHELL=/usr/bin/zsh
    exec /usr/bin/zsh -l
fi

# source ~/env/.bashrc_custom
# # $HOME/.cargo/bin is added to user PATH by MDM
# case ":${PATH}:" in
#     *:"$HOME/.cargo/bin":*)
#     ;;
#     *)
#     export PATH="$PATH:$HOME/.cargo/bin"
#     ;;
# esac

# Vite+ bin (https://viteplus.dev)
. "$HOME/.vite-plus/env"
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
