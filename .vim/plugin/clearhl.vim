function! s:StartHL()
    if !v:hlsearch || mode() isnot 'n'
        return
    endif
    let [pos, rpos] = [winsaveview(), getpos('.')]
    silent! exe "keepjumps go".(line2byte('.')+col('.')-(v:searchforward ? 2 : 0))
    try
        silent keepjumps norm! n
        if getpos('.') != rpos
            throw 0
        endif
    catch /^\%(0$\|Vim\%(\w\|:Interrupt$\)\@!\)/
        call <SID>ClearHl()
        return
    finally
        call winrestview(pos)
    endtry
    if !get(g:,'CoolTotalMatches') || !exists('*reltimestr')
        return
    endif
    exe "silent! norm! :let g:cool_char=nr2char(screenchar(screenrow(),1))\<cr>"
    let cool_char = remove(g:,'cool_char')
    if cool_char !~ '[/?]'
        return
    endif
    let [f, ws, now, noOf] = [0, &wrapscan, reltime(), [0,0]]
    set nowrapscan
    try
        while f < 2
            if reltimestr(reltime(now))[:-6] =~ '[1-9]'
                " time >= 100ms
                return
            endif
            let noOf[v:searchforward ? f : !f] += 1
            try
                silent exe "keepjumps norm! ".(f ? 'n' : 'N')
            catch /^Vim[^)]\+):E38[45]\D/
                call setpos('.',rpos)
                let f += 1
            endtry
        endwhile
    finally
        call winrestview(pos)
        let &wrapscan = ws
    endtry
    redraw|echo cool_char.@/ 'match' noOf[0] 'of' noOf[0] + noOf[1] - 1
endfunction

function! s:ClearHl()
    silent call feedkeys("\<Plug>(ClearHl)", 'm')
endfunction

function! s:HandleHlSearch(old, new)
    if a:old == a:new | return | endif
    augroup HandleSearchMovements
        au!
        if a:old == 0 && a:new == 1
            autocmd CursorMoved * call <SID>StartHL()
            autocmd InsertEnter * call <SID>ClearHl()
        endif
    augroup END
endfunction

noremap! <expr> <Plug>(ClearHl) execute('nohlsearch')
nnoremap <expr> <Plug>(ClearHl) execute('nohlsearch')

augroup CleahSearchHL
    au!
    autocmd OptionSet hlsearch call <SID>HandleHlSearch(v:option_old, v:option_new)
augroup END
" play it cool
call <SID>HandleHlSearch(0, &hlsearch)
