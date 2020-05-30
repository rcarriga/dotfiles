" set statusline=%!GetStatusLine()

let s:UserColours = {
  \ "Add": 1,
  \ "Change": 2,
  \ "Delete": 3,
  \ "Normal": 4,
  \ "Insert": 5,
  \ "Visual": 6,
  \ "Replace": 7
  \ }

function! GetStatusLine()
  return <SID>GetFileStatus().<SID>GetGitStatus()
endfunction

function! s:SetGitStatus()
  let b:gstatus_updated = ""
endfunction

function! s:GetGitStatus()
  let updated = get(b:, "gstatus_updated", "")
  if (updated != "") | return updated | endif
  let sy_status = get(b:, "sy")
  if (type(sy_status) != v:t_dict) | return "" | endif
  let dir = sy_status.info.dir
  let vcs = get(sy_status.vcs, "")
  if vcs == ""
    let gstatus = ""
  else
    let branch = system(vcs." -C ".l:dir." rev-parse --abbrev-ref HEAD")
    let gstatus = "  ".trim(branch)
  endif
  let [add, modi, rem] = sy_status.stats
  if (add)
    let gstatus=gstatus.<SID>Highlight(" +".add, "Add")
  endif
  if (modi)
    let gstatus=gstatus.<SID>Highlight(" ~".modi, "Change")
  endif
  if (rem)
    let gstatus=gstatus.<SID>Highlight(" -".rem, "Delete")
  endif
  let b:gstatus_updated = gstatus
  return gstatus
endfunction

function! s:GetFileStatus()
  let filestatus = "%f"
  if &modified
    let filestatus .= " %m"
  endif
  return <SID>Highlight(" ".filestatus." ", "Normal")
endfunction


function s:Highlight(str, group)
  return "%".get(s:UserColours, a:group)."*".a:str."%*"
endfunction
"
" augroup StatusLineUpdates
"   au!
"   au User Signify call <SID>SetGitStatus()
" augroup END
