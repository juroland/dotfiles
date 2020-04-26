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
plugins=(git gitfast  common-aliases)

source $ZSH/oh-my-zsh.sh

# User configuration

# PROMPT="%# "

export PATH=$HOME/Bin:$HOME/bin:/usr/local/bin:$PATH
export PYTHONPATH=$PYTHONPATH:$HOME/Projects/lib
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/opt/intel/composer_xe_2015.0.090/mkl/include:/opt/intel/composer_xe_2015.0.090/mkl/lib:/opt/intel/composer_xe_2015.0.090/mkl/include:/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
export PATH=$PATH:/opt/intel/composer_xe_2015.0.090/bin:/home/juroland/node_modules/.bin:/home/juroland/.gem/ruby/2.1.0/bin

export PATH=$HOME/bin:$HOME/Build/lib_latex/quiz:$PATH
# export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH

export TEXINPUTS="$TEXINPUTS:/home/julien/Build/lib_latex"


export GUROBI_HOME="/home/julien/Build/gurobi600/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"

export PATH="$PATH:/opt/ibm/ILOG/CPLEX_Studio1261/cplex/bin/x86-64_linux:/home/julien/Build/clang-ctags:/home/julien/.local/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/ibm/ILOG/CPLEX_Studio1261/cplex/bin/x86-64_linux"

export GOPATH=~/Repositories/gowork
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin:$HOME/.npm/bin

export TERM=xterm-256color

export FZF_DEFAULT_COMMAND='ag -g ""'

#. /home/julien/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# added by Anaconda3 4.1.1 installer
export PATH="$PATH:/home/julien/anaconda3/bin"
#

bindkey '^I' complete-word

setopt noincappendhistory
setopt nosharehistory

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.tools
