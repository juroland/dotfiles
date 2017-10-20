" vundle

filetype off                                     " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
" Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'fatih/molokai'
Plugin 'sjl/Gundo.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-fugitive'
Plugin 'majutsushi/tagbar'
Plugin 'plasticboy/vim-markdown'
Plugin 'lervag/vimtex'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'scrooloose/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'godlygeek/tabular'
Plugin 'fatih/vim-go'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'

call vundle#end()                                " required
filetype plugin indent on                        " required

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" tmux
if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" settings

set timeoutlen=1000 ttimeoutlen=0

set number                                       " show line numbers
set backspace=indent,eol,start

set noswapfile
set nobackup
set nowritebackup
set autowrite
set autoread
set hidden
set encoding=utf-8
set relativenumber

set ignorecase                                   " case-insensitive when searching...
set smartcase                                    " ...unless it contains upper case characters.
set incsearch                                    " search as characters are entered
set hlsearch                                     " highlight matches
set noshowmatch                                  " do not highlight matching [{()}]

set wrap
set textwidth=100
set colorcolumn=+1                               " highlight column 101
set formatoptions=roqn1j

set autoindent
set smartindent
set smarttab
set expandtab                                    " tabs are spaces
set tabstop=4                                    " number of visual spaces per TAB
set softtabstop=4                                " number of spaces in tab when editing
set shiftwidth=4                                 " identation size
set cindent
set cino=g0,N-s,b1,t0,(0,W4                      " see :h cinoptions-values

set wildmenu                                     " visual autocomplete for command menu

set autochdir                                    " the working directory is always the same
                                                 " as the file you are editing
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" key mapping

let mapleader = ","

" quickfix
map <C-n> :cn<CR>
map <C-m> :cp<CR>
nnoremap <leader>a :cclose<CR>

nnoremap <leader><space> :nohlsearch<CR>

nnoremap <leader>w :w!<cr>

nnoremap <silent> <leader>q :q!<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" plugins settings

" ctrl-p

let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_working_path_mode = 'ra'

nmap <leader>b :Buffers<CR>
nmap <leader>f :Files ~/<CR>
nmap <leader>. :Files<CR>
nmap <leader>r :Tags<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Molokai plugin
let g:molokai_original=1
colorscheme molokai

syntax enable           " enable syntax processing

" Enable nice porweline symbols
let g:airline_powerline_fonts = 1

set laststatus=2        " displaying status line always

set numberwidth=5

"set showcmd             " show command in bottom bar

set cursorline          " highlight current line

filetype indent on      " load filetype-specific indent files

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=indent
" Enable folding with the spacebar
nnoremap <space> za

" jk is escape
inoremap jk <esc>

" Graph your Vim undo tree in style.
nnoremap <F5> :GundoToggle<CR>

"nmap <F8> :TagbarToggle<CR>
":map <F8> :vertical wincmd f<CR>

nnoremap <F6> :tabe
nnoremap <F7> :tabp<CR>
nnoremap <F8> :tabn<CR>

" Display extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
    " Use Ag over Grep
    set grepprg=ag\ --nogroup\ --nocolor

    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
endif


" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>


" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Always use vertical diffs
set diffopt+=vertical
" set diffopt+=horizontal

augroup markdown

    " remove previous autocmds
    autocmd!

    autocmd BufRead,BufNewFile *.md set filetype=markdown

    " Enable spellchecking for Markdown
    autocmd FileType markdown setlocal spell

    " Automatically wrap at 100 characters for Markdown
    autocmd BufRead,BufNewFile *.md setlocal textwidth=100

augroup END

augroup tex
    autocmd!
    " Enable spellchecking for tex
    autocmd FileType tex setlocal spell
    " autocmd FileType tex setlocal spell spelllang=fr
augroup END

let g:vimtex_disable_version_warning = 1
let g:vimtex_echo_ignore_wait = 1

let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py'
" ensures that the autocomplete window goes away when you’re done with it
let g:ycm_autoclose_preview_window_after_completion = 1
" do not ask about loading
let g:ycm_confirm_extra_conf = 0
" go to
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

" Python
au BufNewFile,BufRead *.py
            \ set tabstop=4       |
            \ set softtabstop=4   |
            \ set shiftwidth=4    |
            \ set textwidth=79    |
            \ set expandtab       |
            \ set autoindent      |
            \ set fileformat=unix

let python_highlight_all=1
syntax on

let NERDTreeIgnore=['\.pyc$', '\~$'] " ignore files in NERDTree

" change the current directory
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

" clang format
map <C-F> :pyfile /usr/share/vim/addons/syntax/clang-format-3.8.py<cr>
imap <C-F> <c-o>:pyfile /usr/share/vim/addons/syntax/clang-format-3.8.py<cr>

" tabs
nnoremap th  :tabfirst<CR>
nnoremap tj  :tabnext<CR>
nnoremap tk  :tabprev<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnext<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>

" vim-go

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

"let g:go_auto_type_info = 1

let g:go_fmt_command = "goimports"

augroup go
    autocmd!

    autocmd FileType go nmap <leader>d :GoDecls<cr>
    autocmd FileType go nmap <leader>l :GoMetaLinter<cr>
    autocmd FileType go nmap <leader>r :GoRun<cr>
    autocmd FileType go set list listchars=tab:\ \ ,trail:·,nbsp:·
augroup END

" ultisnips

let g:UltiSnipsExpandTrigger="<leader><tab>"
let g:UltiSnipsJumpForwardTrigger="<leader><tab>"
let g:UltiSnipsJumpBackwardTrigger="<leader>p<tab>"
