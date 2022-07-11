# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias docs="cd $HOME/Documents"
alias repos="cd $HOME/Repositories"
alias desk="cd $HOME/Desktop"
alias home="cd $HOME"

alias open="xdg-open &>/dev/null"
alias df="df -h"

unsetopt share_history

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git gitfast  common-aliases kubectl)

source $ZSH/oh-my-zsh.sh

# User configuration

# PROMPT="%# "

export PATH=$HOME/Bin:$HOME/bin:/usr/local/bin:$PATH
export PYTHONPATH=$PYTHONPATH:$HOME/Projects/lib
export GOPATH=~/Repositories/gowork
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin:$HOME/.npm/bin

export TERM=xterm-256color

export FZF_DEFAULT_COMMAND='ag -g ""'

bindkey '^I' complete-word

setopt noincappendhistory
setopt nosharehistory

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.tools

VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
PROJECT_HOME=~/Repositories/wavely
source ~/.local/bin/virtualenvwrapper.sh > /dev/null

source ~/.env

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/mc mc

source ~/.aliases.work

NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$NPM_PACKAGES/bin:$PATH"

# Preserve MANPATH if you already defined it somewhere in your config.
# Otherwise, fall back to `manpath` so we can inherit from `/etc/manpath`.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

autoload -Uz compinit
zstyle ':completion:*' menu select
fpath+=~/.zfunc




complete -o nospace -C /usr/bin/terraform terraform

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
